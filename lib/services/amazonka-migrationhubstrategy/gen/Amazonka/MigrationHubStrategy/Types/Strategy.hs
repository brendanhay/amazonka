{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MigrationHubStrategy.Types.Strategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MigrationHubStrategy.Types.Strategy
  ( Strategy
      ( ..,
        Strategy_Refactor,
        Strategy_Rehost,
        Strategy_Relocate,
        Strategy_Replatform,
        Strategy_Repurchase,
        Strategy_Retain,
        Strategy_Retirement
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Strategy = Strategy'
  { fromStrategy ::
      Data.Text
  }
  deriving stock
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Generic
    )
  deriving newtype
    ( Prelude.Hashable,
      Prelude.NFData,
      Data.FromText,
      Data.ToText,
      Data.ToByteString,
      Data.ToLog,
      Data.ToHeader,
      Data.ToQuery,
      Data.FromJSON,
      Data.FromJSONKey,
      Data.ToJSON,
      Data.ToJSONKey,
      Data.FromXML,
      Data.ToXML
    )

pattern Strategy_Refactor :: Strategy
pattern Strategy_Refactor = Strategy' "Refactor"

pattern Strategy_Rehost :: Strategy
pattern Strategy_Rehost = Strategy' "Rehost"

pattern Strategy_Relocate :: Strategy
pattern Strategy_Relocate = Strategy' "Relocate"

pattern Strategy_Replatform :: Strategy
pattern Strategy_Replatform = Strategy' "Replatform"

pattern Strategy_Repurchase :: Strategy
pattern Strategy_Repurchase = Strategy' "Repurchase"

pattern Strategy_Retain :: Strategy
pattern Strategy_Retain = Strategy' "Retain"

pattern Strategy_Retirement :: Strategy
pattern Strategy_Retirement = Strategy' "Retirement"

{-# COMPLETE
  Strategy_Refactor,
  Strategy_Rehost,
  Strategy_Relocate,
  Strategy_Replatform,
  Strategy_Repurchase,
  Strategy_Retain,
  Strategy_Retirement,
  Strategy'
  #-}
