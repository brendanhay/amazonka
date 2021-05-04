{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.AnomalySubscriptionFrequency
  ( AnomalySubscriptionFrequency
      ( ..,
        AnomalySubscriptionFrequency_DAILY,
        AnomalySubscriptionFrequency_IMMEDIATE,
        AnomalySubscriptionFrequency_WEEKLY
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype AnomalySubscriptionFrequency = AnomalySubscriptionFrequency'
  { fromAnomalySubscriptionFrequency ::
      Prelude.Text
  }
  deriving
    ( Prelude.Show,
      Prelude.Read,
      Prelude.Eq,
      Prelude.Ord,
      Prelude.Data,
      Prelude.Typeable,
      Prelude.Generic,
      Prelude.Hashable,
      Prelude.NFData,
      Prelude.FromText,
      Prelude.ToText,
      Prelude.ToByteString,
      Prelude.ToLog,
      Prelude.ToHeader,
      Prelude.ToQuery,
      Prelude.FromJSON,
      Prelude.FromJSONKey,
      Prelude.ToJSON,
      Prelude.ToJSONKey,
      Prelude.FromXML,
      Prelude.ToXML
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
