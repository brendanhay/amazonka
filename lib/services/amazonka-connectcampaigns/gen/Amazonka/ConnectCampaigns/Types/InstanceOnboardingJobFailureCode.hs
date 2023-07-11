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
-- Module      : Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobFailureCode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobFailureCode
  ( InstanceOnboardingJobFailureCode
      ( ..,
        InstanceOnboardingJobFailureCode_EVENT_BRIDGE_ACCESS_DENIED,
        InstanceOnboardingJobFailureCode_EVENT_BRIDGE_MANAGED_RULE_LIMIT_EXCEEDED,
        InstanceOnboardingJobFailureCode_IAM_ACCESS_DENIED,
        InstanceOnboardingJobFailureCode_INTERNAL_FAILURE,
        InstanceOnboardingJobFailureCode_KMS_ACCESS_DENIED,
        InstanceOnboardingJobFailureCode_KMS_KEY_NOT_FOUND
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enumeration of the possible failure codes for instance onboarding job
newtype InstanceOnboardingJobFailureCode = InstanceOnboardingJobFailureCode'
  { fromInstanceOnboardingJobFailureCode ::
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

pattern InstanceOnboardingJobFailureCode_EVENT_BRIDGE_ACCESS_DENIED :: InstanceOnboardingJobFailureCode
pattern InstanceOnboardingJobFailureCode_EVENT_BRIDGE_ACCESS_DENIED = InstanceOnboardingJobFailureCode' "EVENT_BRIDGE_ACCESS_DENIED"

pattern InstanceOnboardingJobFailureCode_EVENT_BRIDGE_MANAGED_RULE_LIMIT_EXCEEDED :: InstanceOnboardingJobFailureCode
pattern InstanceOnboardingJobFailureCode_EVENT_BRIDGE_MANAGED_RULE_LIMIT_EXCEEDED = InstanceOnboardingJobFailureCode' "EVENT_BRIDGE_MANAGED_RULE_LIMIT_EXCEEDED"

pattern InstanceOnboardingJobFailureCode_IAM_ACCESS_DENIED :: InstanceOnboardingJobFailureCode
pattern InstanceOnboardingJobFailureCode_IAM_ACCESS_DENIED = InstanceOnboardingJobFailureCode' "IAM_ACCESS_DENIED"

pattern InstanceOnboardingJobFailureCode_INTERNAL_FAILURE :: InstanceOnboardingJobFailureCode
pattern InstanceOnboardingJobFailureCode_INTERNAL_FAILURE = InstanceOnboardingJobFailureCode' "INTERNAL_FAILURE"

pattern InstanceOnboardingJobFailureCode_KMS_ACCESS_DENIED :: InstanceOnboardingJobFailureCode
pattern InstanceOnboardingJobFailureCode_KMS_ACCESS_DENIED = InstanceOnboardingJobFailureCode' "KMS_ACCESS_DENIED"

pattern InstanceOnboardingJobFailureCode_KMS_KEY_NOT_FOUND :: InstanceOnboardingJobFailureCode
pattern InstanceOnboardingJobFailureCode_KMS_KEY_NOT_FOUND = InstanceOnboardingJobFailureCode' "KMS_KEY_NOT_FOUND"

{-# COMPLETE
  InstanceOnboardingJobFailureCode_EVENT_BRIDGE_ACCESS_DENIED,
  InstanceOnboardingJobFailureCode_EVENT_BRIDGE_MANAGED_RULE_LIMIT_EXCEEDED,
  InstanceOnboardingJobFailureCode_IAM_ACCESS_DENIED,
  InstanceOnboardingJobFailureCode_INTERNAL_FAILURE,
  InstanceOnboardingJobFailureCode_KMS_ACCESS_DENIED,
  InstanceOnboardingJobFailureCode_KMS_KEY_NOT_FOUND,
  InstanceOnboardingJobFailureCode'
  #-}
