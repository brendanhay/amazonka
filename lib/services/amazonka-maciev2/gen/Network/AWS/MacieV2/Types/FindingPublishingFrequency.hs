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
-- Module      : Network.AWS.MacieV2.Types.FindingPublishingFrequency
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MacieV2.Types.FindingPublishingFrequency
  ( FindingPublishingFrequency
      ( ..,
        FindingPublishingFrequency_FIFTEEN_MINUTES,
        FindingPublishingFrequency_ONE_HOUR,
        FindingPublishingFrequency_SIX_HOURS
      ),
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Prelude as Prelude

-- | The frequency with which Amazon Macie publishes updates to policy
-- findings for an account. This includes publishing updates to Security
-- Hub and Amazon EventBridge (formerly called Amazon CloudWatch Events).
-- For more information, see
-- <https://docs.aws.amazon.com/macie/latest/user/findings-monitor.html Monitoring and processing findings>
-- in the /Amazon Macie User Guide/. Valid values are:
newtype FindingPublishingFrequency = FindingPublishingFrequency'
  { fromFindingPublishingFrequency ::
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

pattern FindingPublishingFrequency_FIFTEEN_MINUTES :: FindingPublishingFrequency
pattern FindingPublishingFrequency_FIFTEEN_MINUTES = FindingPublishingFrequency' "FIFTEEN_MINUTES"

pattern FindingPublishingFrequency_ONE_HOUR :: FindingPublishingFrequency
pattern FindingPublishingFrequency_ONE_HOUR = FindingPublishingFrequency' "ONE_HOUR"

pattern FindingPublishingFrequency_SIX_HOURS :: FindingPublishingFrequency
pattern FindingPublishingFrequency_SIX_HOURS = FindingPublishingFrequency' "SIX_HOURS"

{-# COMPLETE
  FindingPublishingFrequency_FIFTEEN_MINUTES,
  FindingPublishingFrequency_ONE_HOUR,
  FindingPublishingFrequency_SIX_HOURS,
  FindingPublishingFrequency'
  #-}
