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
-- Module      : Amazonka.MacieV2.Types.FindingPublishingFrequency
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FindingPublishingFrequency
  ( FindingPublishingFrequency
      ( ..,
        FindingPublishingFrequency_FIFTEEN_MINUTES,
        FindingPublishingFrequency_ONE_HOUR,
        FindingPublishingFrequency_SIX_HOURS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The frequency with which Amazon Macie publishes updates to policy
-- findings for an account. This includes publishing updates to Security
-- Hub and Amazon EventBridge (formerly Amazon CloudWatch Events). For more
-- information, see
-- <https://docs.aws.amazon.com/macie/latest/user/findings-monitor.html Monitoring and processing findings>
-- in the /Amazon Macie User Guide/. Valid values are:
newtype FindingPublishingFrequency = FindingPublishingFrequency'
  { fromFindingPublishingFrequency ::
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
