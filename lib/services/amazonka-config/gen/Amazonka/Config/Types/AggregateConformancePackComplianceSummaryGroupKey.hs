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
-- Module      : Amazonka.Config.Types.AggregateConformancePackComplianceSummaryGroupKey
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateConformancePackComplianceSummaryGroupKey
  ( AggregateConformancePackComplianceSummaryGroupKey
      ( ..,
        AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID,
        AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype AggregateConformancePackComplianceSummaryGroupKey = AggregateConformancePackComplianceSummaryGroupKey'
  { fromAggregateConformancePackComplianceSummaryGroupKey ::
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

pattern AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID :: AggregateConformancePackComplianceSummaryGroupKey
pattern AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID = AggregateConformancePackComplianceSummaryGroupKey' "ACCOUNT_ID"

pattern AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION :: AggregateConformancePackComplianceSummaryGroupKey
pattern AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION = AggregateConformancePackComplianceSummaryGroupKey' "AWS_REGION"

{-# COMPLETE
  AggregateConformancePackComplianceSummaryGroupKey_ACCOUNT_ID,
  AggregateConformancePackComplianceSummaryGroupKey_AWS_REGION,
  AggregateConformancePackComplianceSummaryGroupKey'
  #-}
