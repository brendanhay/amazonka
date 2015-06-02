{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

-- Module      : Compiler.AST.Data.Instance
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Compiler.AST.Data.Instance where

import           Compiler.AST.Data.Field
import           Compiler.Formatting
import           Compiler.Types          hiding (input, output)
import           Control.Error
import           Control.Lens
import           Data.Aeson
import qualified Data.Foldable           as Fold
import           Data.List               (deleteBy, find, partition)
import           Data.Maybe
import           Data.Monoid
import           Data.Text               (Text)
import qualified Data.Text               as Text
import           Data.Text.Manipulate

-- FIXME: XML namespace requirement for ToElement
-- general tidy up of syntax/instance/data/annotations
-- build

data Inst
    = FromXML   [Field]
    | FromJSON  [Field]
    | ToXML     [Field]
    | ToElement Field
    | ToJSON    [Field]
    | ToQuery   [Either Text Field]
    | ToHeaders [Field]
    | ToPath    [Either Text Field]
    | ToBody    Field
      deriving (Eq, Show)

instance ToJSON Inst where
    toJSON = toJSON . instToText

instToText :: Inst -> Text
instToText = \case
    FromJSON     {} -> "FromJSON"
    FromXML      {} -> "FromXML"
    ToJSON       {} -> "ToJSON"
    ToXML        {} -> "ToXML"
    ToElement {} -> "ToElement"
    ToQuery      {} -> "ToQuery"
    ToHeaders    {} -> "ToHeaders"
    ToPath       {} -> "ToPath"
    ToBody       {} -> "ToBody"

prodInsts :: HasRelation a
          => Protocol
          -> a
          -> [Field]
          -> Either Error [Inst]
prodInsts p r = pure . shape p (r ^. relMode)

sumInsts :: HasRelation a => Protocol -> a -> [Text]
sumInsts p r = map instToText $ shape p (r ^. relMode) []

requestInsts :: Protocol -> HTTP Identity -> [Field] -> Either Error [Inst]
requestInsts p h fs = do
    path <- toPath
    concatQuery =<< replaceXML
        ( [toHeaders, path]
       ++ maybeToList toBody
       ++ shape p (Uni Input) fields
        )
  where
    toHeaders :: Inst
    toHeaders = ToHeaders (satisfies [Header, Headers] fs)

    toPath :: Either Error Inst
    toPath = ToPath <$> uriFields h uriPath fs

    toBody :: Maybe Inst
    toBody = ToBody <$> stream

    concatQuery :: [Inst] -> Either Error [Inst]
    concatQuery is = do
        xs <- uriFields h uriQuery fs
        return $! merged xs : filter (not . f) is
      where
        merged xs =
            let ys = map Right (satisfies [Querystring] fs) <> xs
             in case find f is of
                Just (ToQuery zs) -> ToQuery (ys <> zs)
                _                 -> ToQuery ys

        f ToQuery {} = True
        f _          = False

    replaceXML :: [Inst] -> Either Error [Inst]
    replaceXML = traverse go
      where
        go (ToXML [])  = return $  ToXML []
        go (ToXML [x]) = return $! ToElement x
        go (ToXML _)   = Left "More than one field found for ToElement instance"
        go x           = return x

    (listToMaybe -> stream, fields) =
        partition (view fieldStream) notLocated

    notLocated :: [Field]
    notLocated = satisfy (\l -> isNothing l || Just Body == l) fs

uriFields h l fs = traverse go (h ^. l)
  where
    go (Tok t) = return (Left t)
    go (Var v) = Right <$> note (missing v) (find ((v ==) . view fieldId) fs)

    missing = format ("Missing field corresponding to URI var " % iprimary)

shape :: Protocol -> Mode -> [Field] -> [Inst]
shape p m fs = case m of
    Bi         -> [input  p fs, output p fs]
    Uni Input  -> [input  p fs]
    Uni Output -> [output p fs]

input :: Protocol -> [Field] -> Inst
input = \case
    JSON     -> ToJSON
    RestJSON -> ToJSON
    XML      -> ToXML
    RestXML  -> ToXML
    Query    -> ToQuery . map Right
    EC2      -> ToQuery . map Right

output :: Protocol -> [Field] -> Inst
output = \case
    JSON     -> FromJSON
    RestJSON -> FromJSON
    XML      -> FromXML
    RestXML  -> FromXML
    Query    -> FromXML
    EC2      -> FromXML

satisfies :: [Location] -> [Field] -> [Field]
satisfies xs = satisfy (`elem` map Just xs)

satisfy :: (Maybe Location -> Bool) -> [Field] -> [Field]
satisfy f = filter (f . view fieldLocation)
