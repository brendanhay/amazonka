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
-- Module      : Amazonka.CostExplorer.Types.AnomalySubscriptionFrequency
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CostExplorer.Types.AnomalySubscriptionFrequency
  ( AnomalySubscriptionFrequency
      ( ..,
        AnomalySubscriptionFrequency_DAILY,
        AnomalySubscriptionFrequency_IMMEDIATE,
        AnomalySubscriptionFrequency_WEEKLY
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AnomalySubscriptionFrequency = AnomalySubscriptionFrequency'
  { fromAnomalySubscriptionFrequency ::
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

pattern AnomalySubscriptionFrequency_DAILY :: AnomalySubscriptionFrequency
pattern AnomalySubscriptionFrequency_DAILY = AnomalySubscriptionFrequency' "DAILY"

pattern AnomalySubscriptionFrequency_IMMEDIATE :: AnomalySubscriptionFrequency
pattern AnomalySubscriptionFrequency_IMMEDIATE = AnomalySubscriptionFrequency' "IMMEDIATE"

pattern AnomalySubscriptionFrequency_WEEKLY :: AnomalySubscriptionFrequency
pattern AnomalySubscriptionFrequency_WEEKLY = AnomalySubscriptionFrequency' "WEEKLY"

{-# COMPLETE
  AnomalySubscriptionFrequency_DAILY,
  AnomalySubscriptionFrequency_IMMEDIATE,
  AnomalySubscriptionFrequency_WEEKLY,
  AnomalySubscriptionFrequency'
  #-}
