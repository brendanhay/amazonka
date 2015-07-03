{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -fno-warn-orphans  #-}

-- Module      : Test.AWS.TH
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.AWS.TH where

import           Data.Time
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.AWS.Data.Text
import           Network.AWS.Data.Time
import           Network.AWS.Prelude

mkTime :: Text -> Q Exp
mkTime x =
    case fromText x :: Either String ISO8601 of
        Left  e -> error (show e)
        Right t -> [|view _Time t|]

instance Lift (Time a) where
    lift (Time x) = AppE (ConE (mkName "Time")) <$> lift x

instance Lift UTCTime where
    lift (UTCTime x y) = do
        x' <- lift x
        y' <- lift y
        return $ AppE (AppE (ConE (mkName "UTCTime")) x') y'

instance Lift DiffTime where
    lift x = [|toEnum $(lift (fromEnum x))|]

instance Lift Day where
    lift x = [|toEnum $(lift (fromEnum x))|]
