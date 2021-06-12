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
-- Module      : Network.AWS.CostExplorer.Types.Context
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Context
  ( Context
      ( ..,
        Context_COST_AND_USAGE,
        Context_RESERVATIONS,
        Context_SAVINGS_PLANS
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype Context = Context' {fromContext :: Core.Text}
  deriving stock
    ( Core.Show,
      Core.Read,
      Core.Eq,
      Core.Ord,
      Core.Generic
    )
  deriving newtype
    ( Core.Hashable,
      Core.NFData,
      Core.FromText,
      Core.ToText,
      Core.ToByteString,
      Core.ToLog,
      Core.ToHeader,
      Core.ToQuery,
      Core.FromJSON,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.ToJSONKey,
      Core.FromXML,
      Core.ToXML
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
