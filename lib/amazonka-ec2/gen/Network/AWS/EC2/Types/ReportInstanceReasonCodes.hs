{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ReportInstanceReasonCodes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReportInstanceReasonCodes
  ( ReportInstanceReasonCodes
      ( ReportInstanceReasonCodes',
        ReportInstanceReasonCodesInstanceStuckInState,
        ReportInstanceReasonCodesUnresponsive,
        ReportInstanceReasonCodesNotAcceptingCredentials,
        ReportInstanceReasonCodesPasswordNotAvailable,
        ReportInstanceReasonCodesPerformanceNetwork,
        ReportInstanceReasonCodesPerformanceInstanceStore,
        ReportInstanceReasonCodesPerformanceEbsVolume,
        ReportInstanceReasonCodesPerformanceOther,
        ReportInstanceReasonCodesOther,
        fromReportInstanceReasonCodes
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ReportInstanceReasonCodes = ReportInstanceReasonCodes'
  { fromReportInstanceReasonCodes ::
      Core.Text
  }
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern ReportInstanceReasonCodesInstanceStuckInState :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesInstanceStuckInState = ReportInstanceReasonCodes' "instance-stuck-in-state"

pattern ReportInstanceReasonCodesUnresponsive :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesUnresponsive = ReportInstanceReasonCodes' "unresponsive"

pattern ReportInstanceReasonCodesNotAcceptingCredentials :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesNotAcceptingCredentials = ReportInstanceReasonCodes' "not-accepting-credentials"

pattern ReportInstanceReasonCodesPasswordNotAvailable :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesPasswordNotAvailable = ReportInstanceReasonCodes' "password-not-available"

pattern ReportInstanceReasonCodesPerformanceNetwork :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesPerformanceNetwork = ReportInstanceReasonCodes' "performance-network"

pattern ReportInstanceReasonCodesPerformanceInstanceStore :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesPerformanceInstanceStore = ReportInstanceReasonCodes' "performance-instance-store"

pattern ReportInstanceReasonCodesPerformanceEbsVolume :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesPerformanceEbsVolume = ReportInstanceReasonCodes' "performance-ebs-volume"

pattern ReportInstanceReasonCodesPerformanceOther :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesPerformanceOther = ReportInstanceReasonCodes' "performance-other"

pattern ReportInstanceReasonCodesOther :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodesOther = ReportInstanceReasonCodes' "other"

{-# COMPLETE
  ReportInstanceReasonCodesInstanceStuckInState,
  ReportInstanceReasonCodesUnresponsive,
  ReportInstanceReasonCodesNotAcceptingCredentials,
  ReportInstanceReasonCodesPasswordNotAvailable,
  ReportInstanceReasonCodesPerformanceNetwork,
  ReportInstanceReasonCodesPerformanceInstanceStore,
  ReportInstanceReasonCodesPerformanceEbsVolume,
  ReportInstanceReasonCodesPerformanceOther,
  ReportInstanceReasonCodesOther,
  ReportInstanceReasonCodes'
  #-}
