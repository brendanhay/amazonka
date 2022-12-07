{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.Amazonka.TH
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.TH where

import Amazonka.Core.Lens.Internal (view)
import Amazonka.Data
import Data.Time (Day (..), DiffTime, UTCTime (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkTime :: Text -> Q Exp
mkTime x =
  case fromText x :: Either String ISO8601 of
    Left e -> error (show e)
    Right t -> [|view _Time t|]

deriving instance Lift (Time a)

deriving instance Lift UTCTime

deriving instance Lift Day

-- DiffTime's constructor is not exported, so use a manual instance.
--
-- Note: An entire valid instance must be duplicated inside CPP to
-- otherwise the ormolu formatter will erroneously rewrite it.
#if MIN_VERSION_template_haskell(2,16,0)
instance Lift DiffTime where
  liftTyped x = [||toEnum $$(liftTyped (fromEnum x))||]
#else
instance Lift DiffTime where
  lift x = [|toEnum $(lift (fromEnum x))|]
#endif
