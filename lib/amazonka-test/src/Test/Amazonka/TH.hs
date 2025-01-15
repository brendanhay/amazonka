{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- Module      : Test.Amazonka.TH
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.TH where

import Amazonka.Core.Lens.Internal (view)
import Amazonka.Data
-- Provides other Lift instances, if `time` doesn't.
import Data.Time.Compat ()
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkTime :: Text -> Q Exp
mkTime x =
  case fromText x :: Either String ISO8601 of
    Left e -> error (show e)
    Right t -> [|view _Time t|]

deriving instance Lift (Time a)
