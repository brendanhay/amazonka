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
-- Module      : Amazonka.CostExplorer.Types.Context
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.Context
  ( Context
      ( ..,
        Context_COST_AND_USAGE,
        Context_RESERVATIONS,
        Context_SAVINGS_PLANS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype Context = Context' {fromContext :: Data.Text}
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

pattern Context_COST_AND_USAGE :: Context
pattern Context_COST_AND_USAGE = Context' "COST_AND_USAGE"

pattern Context_RESERVATIONS :: Context
pattern Context_RESERVATIONS = Context' "RESERVATIONS"

pattern Context_SAVINGS_PLANS :: Context
pattern Context_SAVINGS_PLANS = Context' "SAVINGS_PLANS"

{-# COMPLETE
  Context_COST_AND_USAGE,
  Context_RESERVATIONS,
  Context_SAVINGS_PLANS,
  Context'
  #-}
