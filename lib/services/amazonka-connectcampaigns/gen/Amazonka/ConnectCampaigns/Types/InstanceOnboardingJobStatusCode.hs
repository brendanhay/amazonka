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
-- Module      : Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatusCode
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCampaigns.Types.InstanceOnboardingJobStatusCode
  ( InstanceOnboardingJobStatusCode
      ( ..,
        InstanceOnboardingJobStatusCode_FAILED,
        InstanceOnboardingJobStatusCode_IN_PROGRESS,
        InstanceOnboardingJobStatusCode_SUCCEEDED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Enumeration of the possible states for instance onboarding job
newtype InstanceOnboardingJobStatusCode = InstanceOnboardingJobStatusCode'
  { fromInstanceOnboardingJobStatusCode ::
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

pattern InstanceOnboardingJobStatusCode_FAILED :: InstanceOnboardingJobStatusCode
pattern InstanceOnboardingJobStatusCode_FAILED = InstanceOnboardingJobStatusCode' "FAILED"

pattern InstanceOnboardingJobStatusCode_IN_PROGRESS :: InstanceOnboardingJobStatusCode
pattern InstanceOnboardingJobStatusCode_IN_PROGRESS = InstanceOnboardingJobStatusCode' "IN_PROGRESS"

pattern InstanceOnboardingJobStatusCode_SUCCEEDED :: InstanceOnboardingJobStatusCode
pattern InstanceOnboardingJobStatusCode_SUCCEEDED = InstanceOnboardingJobStatusCode' "SUCCEEDED"

{-# COMPLETE
  InstanceOnboardingJobStatusCode_FAILED,
  InstanceOnboardingJobStatusCode_IN_PROGRESS,
  InstanceOnboardingJobStatusCode_SUCCEEDED,
  InstanceOnboardingJobStatusCode'
  #-}
