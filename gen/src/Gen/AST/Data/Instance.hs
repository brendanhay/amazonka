-- Module      : Gen.AST.Data.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data.Instance where

import Control.Applicative
import Control.Error
import Control.Lens
import Control.Monad.Except
import Data.Aeson
import Data.List (find, partition)
import Data.Text (Text)
import Gen.AST.Data.Field
import Gen.Formatting
import Gen.Types

data Inst
  = FromXML [Field]
  | FromJSON [Field]
  | ToXML [Field]
  | ToElement (Maybe Text) (Either Text Field)
  | ToJSON [Field]
  | ToHeaders [Either (Text, Text) Field]
  | ToQuery [Either (Text, Maybe Text) Field]
  | ToPath [Either Text Field]
  | ToBody Field
  | IsHashable
  | IsNFData

instance ToJSON Inst where
  toJSON = toJSON . instToText

instToText :: Inst -> Text
instToText = \case
  FromJSON {} -> "FromJSON"
  FromXML {} -> "FromXML"
  ToJSON {} -> "ToJSON"
  ToXML {} -> "ToXML"
  ToElement {} -> "ToElement"
  ToHeaders {} -> "ToHeaders"
  ToQuery {} -> "ToQuery"
  ToPath {} -> "ToPath"
  ToBody {} -> "ToBody"
  IsHashable -> "Hashable"
  IsNFData -> "NFData"

shapeInsts :: Protocol -> Mode -> [Field] -> [Inst]
shapeInsts p m fs = go m
  where
    go :: Mode -> [Inst]
    go = \case
      Bi -> [inp p fs, out p fs]
      Uni Input -> [inp p fs]
      Uni Output -> [out p fs]

    inp :: Protocol -> [Field] -> Inst
    inp = \case
      JSON -> ToJSON
      RestJSON -> ToJSON
      RestXML -> ToXML
      Query -> ToQuery . map Right
      EC2 -> ToQuery . map Right
      APIGateway -> ToJSON

    out :: Protocol -> [Field] -> Inst
    out = \case
      JSON -> FromJSON
      RestJSON -> FromJSON
      RestXML -> FromXML
      Query -> FromXML
      EC2 -> FromXML
      APIGateway -> FromJSON

responseInsts :: [Field] -> [Inst]
responseInsts fs
  | stream = mempty
  | otherwise = [IsNFData]
  where
    (not . null -> stream, _) = partition fieldStream (notLocated fs)

requestInsts ::
  HasMetadata a f =>
  a ->
  Id ->
  HTTP ->
  Ref ->
  [Field] ->
  Either Error [Inst]
requestInsts m oname h r fs = do
  path' <- toPath
  concatQuery
    =<< replaceXML
      ( [toHeaders, path']
          ++ maybeToList toBody
          ++ removeInsts (shapeInsts p (Uni Input) fields)
      )
  where
    toHeaders :: Inst
    toHeaders =
      ToHeaders $
        map Right headers
          ++ map Left protocolHeaders

    toPath :: Either Error Inst
    toPath = ToPath <$> uriFields oname h uriPath id fs

    toBody :: Maybe Inst
    toBody = ToBody <$> (stream <|> find fieldLitPayload fields)

    body :: Bool
    body = isJust toBody

    concatQuery :: [Inst] -> Either Error [Inst]
    concatQuery is = do
      xs <- uriFields oname h uriQuery (,Nothing) fs
      return $! merged xs : filter (not . f) is
      where
        merged xs =
          let ys =
                map Right (satisfies [Querystring] fs) <> xs
                  ++ map Left protocolQuery
           in case find f is of
                Just (ToQuery zs) -> ToQuery (ys <> zs)
                _ -> ToQuery ys

        f ToQuery {} = True
        f _ = False

    replaceXML :: [Inst] -> Either Error [Inst]
    replaceXML is
      | all nonEmptyXML is = return $! filter anyXML is
      | otherwise =
        case ( r ^? refXMLNamespace . _Just . xmlUri,
               r ^. refLocationName,
               listToMaybe (mapMaybe findElement is)
             ) of
          -- 1. If there's an xmlNamespace and/or locationName on the ref,
          --    it should define separate ToXML + ToElement instances
          (ns, Just e, _) ->
            return $! ToElement (ns <|> m ^. xmlNamespace) (Left e) : is
          -- 2. Otherwise, a single field should be found in the ToXML instance
          -- and lifted to a single ToElement instance.
          (_, _, Just f) ->
            return $! ToElement ns (Right f) : filter anyXML is
            where
              ns =
                m ^. xmlNamespace
                  <|> f ^? fieldRef . refXMLNamespace . _Just . xmlUri

          -- 3. Unknown.
          (ns, e, _) ->
            throwError $
              format
                ( "Error determining root ToElement instance: " % iprimary
                    % ", namespace: "
                    % shown
                    % ", locationName: "
                    % shown
                )
                n
                ns
                e
      where
        nonEmptyXML = notXML True
        anyXML = notXML False

        notXML e = \case
          ToXML [] -> e
          ToXML {} -> False
          _ -> True

        findElement = \case
          ToXML [f] -> Just f
          _ -> Nothing

    removeInsts :: [Inst] -> [Inst]
    removeInsts = mapMaybe go
      where
        go = \case
          ToXML {} | idem || body -> Nothing
          ToElement {} | idem || body -> Nothing
          ToJSON {} | idem || body -> Nothing
          i -> Just i

        idem = (h ^. method) `elem` [HEAD, GET, DELETE]

    (listToMaybe -> stream, fields) = partition fieldStream (notLocated fs)

    protocolHeaders :: [(Text, Text)]
    protocolHeaders = case p of
      JSON -> t ++ c
      RestJSON -> c
      APIGateway -> j ++ c
      _ -> []
      where
        t = maybeToList $ ("X-Amz-Target",) <$> target
        c = maybeToList $ ("Content-Type",) <$> content
        j = [("Accept", "application/json")]

    protocolQuery :: [(Text, Maybe Text)]
    protocolQuery = case p of
      Query -> [a, v]
      EC2 -> [a, v]
      _ -> []
      where
        a = ("Action", Just action)
        v = ("Version", Just version)

    headers :: [Field]
    headers = satisfies [Header, Headers] fs

    target = (<> ("." <> action)) <$> m ^. targetPrefix

    -- Skip adding the x-amz-json-* JSON version if the request data structure
    -- already has a field serialized to the Content-Type header.
    content =
      let go x = x ^. fieldRef . refLocationName == Just "Content-Type"
       in if isJust (find go headers)
            then Nothing
            else ("application/x-amz-json-" <>) <$> m ^. jsonVersion

    action = memberId n
    version = m ^. apiVersion

    p = m ^. protocol
    n = identifier r

notLocated :: [Field] -> [Field]
notLocated = satisfy (\l -> isNothing l || Just Body == l)

uriFields ::
  (Foldable f, Traversable t) =>
  Id ->
  s ->
  Getter s (t Segment) ->
  (Text -> a) ->
  f Field ->
  Either Error (t (Either a Field))
uriFields oname h l f fs = traverse go (h ^. l)
  where
    go (Tok t) = return $ Left (f t)
    go (Var v) = Right <$> note missing (find match fs)
      where
        match x = memberId v == name x
        missing =
          format
            ( "Missing field corresponding to URI variable "
                % iprimary
                % " in field names "
                % shown
                % "\nfor operation "
                % iprimary
            )
            v
            ids
            oname

    ids :: [Text]
    ids = foldMap ((: []) . name) fs

    name x =
      fromMaybe
        (x ^. fieldId . to memberId)
        (x ^. fieldRef . refLocationName)

satisfies :: [Location] -> [Field] -> [Field]
satisfies xs = satisfy (`elem` map Just xs)

satisfy :: (Maybe Location -> Bool) -> [Field] -> [Field]
satisfy f = filter (f . fieldLocation)
