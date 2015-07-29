{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Gen.AST.Data.Instance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.AST.Data.Instance where

import           Control.Applicative
import           Control.Error
import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.List            (find, partition)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           Gen.AST.Data.Field
import           Gen.Formatting
import           Gen.Types

data Inst
    = FromXML   [Field]
    | FromJSON  [Field]
    | ToXML     [Field]
    | ToElement (Maybe Text) (Either Text Field)
    | ToJSON    [Field]
    | ToHeaders [Either (Text, Text) Field]
    | ToQuery   [Either (Text, Maybe Text) Field]
    | ToPath    [Either Text Field]
    | ToBody    Field

instance ToJSON Inst where
    toJSON = toJSON . instToText

instToText :: Inst -> Text
instToText = \case
    FromJSON  {} -> "FromJSON"
    FromXML   {} -> "FromXML"
    ToJSON    {} -> "ToJSON"
    ToXML     {} -> "ToXML"
    ToElement {} -> "ToElement"
    ToHeaders {} -> "ToHeaders"
    ToQuery   {} -> "ToQuery"
    ToPath    {} -> "ToPath"
    ToBody    {} -> "ToBody"

shapeInsts :: Protocol -> Mode -> [Field] -> [Inst]
shapeInsts p m fs = go m
  where
    go :: Mode -> [Inst]
    go = \case
        Bi         -> [inp p fs, out p fs]
        Uni Input  -> [inp p fs]
        Uni Output -> [out p fs]

    inp :: Protocol -> [Field] -> Inst
    inp = \case
        JSON     -> ToJSON
        RestJSON -> ToJSON
        RestXML  -> ToXML
        Query    -> ToQuery . map Right
        EC2      -> ToQuery . map Right

    out :: Protocol -> [Field] -> Inst
    out = \case
        JSON     -> FromJSON
        RestJSON -> FromJSON
        RestXML  -> FromXML
        Query    -> FromXML
        EC2      -> FromXML

requestInsts :: HasMetadata a f
             => a
             -> HTTP Identity
             -> Ref
             -> [Field]
             -> Either Error [Inst]
requestInsts m h r fs = do
    path' <- toPath
    concatQuery =<< replaceXML
        ( [toHeaders, path']
       ++ maybeToList toBody
       ++ removeInsts (shapeInsts p (Uni Input) fields)
        )
  where
    toHeaders :: Inst
    toHeaders = ToHeaders
         $ map Right (satisfies [Header, Headers] fs)
        ++ map Left  protocolHeaders

    toPath :: Either Error Inst
    toPath = ToPath . concatMap split <$> uriFields h uriPath id fs
      where
        split (Right f)  = [Right f]
        split (Left "/") = [] -- drop delimiters
        split (Left  x)  =
            map Left . filter (not . Text.null) $ Text.split (== '/') x

    toBody :: Maybe Inst
    toBody = ToBody <$> stream

    concatQuery :: [Inst] -> Either Error [Inst]
    concatQuery is = do
        xs <- uriFields h uriQuery (,Nothing) fs
        return $! merged xs : filter (not . f) is
      where
        merged xs =
            let ys = map Right (satisfies [Querystring] fs) <> xs
                  ++ map Left  protocolQuery
             in case find f is of
                Just (ToQuery zs) -> ToQuery (ys <> zs)
                _                 -> ToQuery ys

        f ToQuery {} = True
        f _          = False

    replaceXML :: [Inst] -> Either Error [Inst]
    replaceXML is
        | all nonEmptyXML is = return $! filter anyXML is
        | otherwise          =
            case ( r ^? refXMLNamespace . _Just . xmlUri
                  , r ^. refLocationName
                  , listToMaybe (mapMaybe findElement is)
                  ) of

            -- 1. If there's an xmlNamespace and/or locationName on the ref,
            --    it should define separate ToXML + ToElement instances
            (ns, Just e, _) ->
                return $! ToElement (ns <|> m ^. xmlNamespace) (Left e) : is

            -- 2. Otherwise, a single field should be found in the ToXML instance
            -- and lifted to a single ToElement instance.
            (_, _, Just f)  ->
                return $! ToElement ns (Right f) : filter anyXML is
              where
                ns = m ^. xmlNamespace
                 <|> f ^? fieldRef . refXMLNamespace . _Just . xmlUri

            -- 3. Unknown.
            (ns, e, _) -> throwError $
                format ("Error determining root ToElement instance: " % iprimary %
                        ", namespace: " % shown %
                        ", locationName: " % shown)
                       n ns e
      where
        nonEmptyXML = notXML True
        anyXML      = notXML False

        notXML e = \case
            ToXML [] -> e
            ToXML {} -> False
            _        -> True

        findElement = \case
            ToXML [f] -> Just f
            _         -> Nothing


    removeInsts :: [Inst] -> [Inst]
    removeInsts = mapMaybe go
      where
        go = \case
            ToXML     {} | idem || body -> Nothing
            ToElement {} | idem || body -> Nothing
            ToJSON    {} | idem || body -> Nothing
            i                           -> Just i

        idem = (h ^. method) `elem` [HEAD, GET, DELETE]
        body = isJust toBody

    (listToMaybe -> stream, fields) = partition fieldStream notLocated

    notLocated :: [Field]
    notLocated = satisfy (\l -> isNothing l || Just Body == l) fs

    protocolHeaders :: [(Text, Text)]
    protocolHeaders = case p of
        JSON     -> t ++ c
        RestJSON -> c
        _        -> []
      where
        t = maybeToList $ ("X-Amz-Target",) <$> target
        c = maybeToList $ ("Content-Type",) <$> content

    protocolQuery :: [(Text, Maybe Text)]
    protocolQuery = case p of
        Query    -> [a, v]
        EC2      -> [a, v]
        _        -> []
      where
        a = ("Action",  Just action)
        v = ("Version", Just version)

    content = ("application/x-amz-json-" <>) <$> m ^. jsonVersion
    target  = (<> ("." <> action))           <$> m ^. targetPrefix

    action  = memberId n
    version = m ^. apiVersion

    p = m ^. protocol
    n = identifier r

uriFields :: (Foldable f, Traversable t)
          => s
          -> Getter s (t Segment)
          -> (Text -> a)
          -> f Field
          -> Either Error (t (Either a Field))
uriFields h l f fs = traverse go (h ^. l)
  where
    go (Tok t) = return $ Left (f t)
    go (Var v) = Right <$> note missing (find match fs)
      where
        match x = memberId v ==
            fromMaybe (x ^. fieldId  . to memberId)
                      (x ^. fieldRef . refLocationName)

        missing = format ("Missing field corresponding to URI variable "
                         % iprimary % " in field names " % shown)
                         v ids

    ids :: [Text]
    ids = foldMap ((:[]) . memberId . _fieldId) fs

satisfies :: [Location] -> [Field] -> [Field]
satisfies xs = satisfy (`elem` map Just xs)

satisfy :: (Maybe Location -> Bool) -> [Field] -> [Field]
satisfy f = filter (f . fieldLocation)
