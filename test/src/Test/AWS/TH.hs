{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.AWS.TH
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.AWS.TH where

import Data.Time (Day (..), DiffTime, UTCTime (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Network.AWS.Core
import Network.AWS.Lens (view)

mkTime :: Text -> Q Exp
mkTime x =
  case fromText x :: Either String ISO8601 of
    Left e -> error (show e)
    Right t -> [|view _Time t|]

deriving instance Lift (Time a)

deriving instance Lift UTCTime

deriving instance Lift Day

-- DiffTime's constructor is not exported, so use a manual instance
instance Lift DiffTime where
#if MIN_VERSION_template_haskell(2,16,0)
  liftTyped x = [||toEnum $$(liftTyped (fromEnum x))||]
#else
  lift x = [|toEnum $(lift (fromEnum x))|]
#endif
