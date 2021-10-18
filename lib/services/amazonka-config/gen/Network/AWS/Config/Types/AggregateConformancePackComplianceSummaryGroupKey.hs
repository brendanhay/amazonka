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
-- Module      : Network.AWS.Config.Types.AggregateConformancePackComplianceSummaryGroupKey
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateConformancePackComplianceSummaryGroupKey
  ( AggregateConformancePackComplianceSummaryGroupKey
      ( ..,
        AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID,
        AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

newtype AggregateConformancePackComplianceSummaryGroupKey = AggregateConformancePackComplianceSummaryGroupKey'
  { fromAggregateConformancePackComplianceSummaryGroupKey ::
      Core.Text
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

pattern AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID :: AggregateConformancePackComplianceSummaryGroupKey
pattern AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID = AggregateConformancePackComplianceSummaryGroupKey' "ACCOUNT_ID"

pattern AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION :: AggregateConformancePackComplianceSummaryGroupKey
pattern AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION = AggregateConformancePackComplianceSummaryGroupKey' "AWS_REGION"

{-# COMPLETE
  AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID,
  AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION,
  AggregateConformancePackComplianceSummaryGroupKey'
  #-}
