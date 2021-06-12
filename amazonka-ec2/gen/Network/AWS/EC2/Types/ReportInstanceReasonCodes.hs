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
-- Module      : Network.AWS.EC2.Types.ReportInstanceReasonCodes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ReportInstanceReasonCodes
  ( ReportInstanceReasonCodes
      ( ..,
        ReportInstanceReasonCodes_Instance_stuck_in_state,
        ReportInstanceReasonCodes_Not_accepting_credentials,
        ReportInstanceReasonCodes_Other,
        ReportInstanceReasonCodes_Password_not_available,
        ReportInstanceReasonCodes_Performance_ebs_volume,
        ReportInstanceReasonCodes_Performance_instance_store,
        ReportInstanceReasonCodes_Performance_network,
        ReportInstanceReasonCodes_Performance_other,
        ReportInstanceReasonCodes_Unresponsive
      ),
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal

newtype ReportInstanceReasonCodes = ReportInstanceReasonCodes'
  { fromReportInstanceReasonCodes ::
      Core.Text
  }
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

pattern ReportInstanceReasonCodes_Instance_stuck_in_state :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Instance_stuck_in_state = ReportInstanceReasonCodes' "instance-stuck-in-state"

pattern ReportInstanceReasonCodes_Not_accepting_credentials :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Not_accepting_credentials = ReportInstanceReasonCodes' "not-accepting-credentials"

pattern ReportInstanceReasonCodes_Other :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Other = ReportInstanceReasonCodes' "other"

pattern ReportInstanceReasonCodes_Password_not_available :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Password_not_available = ReportInstanceReasonCodes' "password-not-available"

pattern ReportInstanceReasonCodes_Performance_ebs_volume :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Performance_ebs_volume = ReportInstanceReasonCodes' "performance-ebs-volume"

pattern ReportInstanceReasonCodes_Performance_instance_store :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Performance_instance_store = ReportInstanceReasonCodes' "performance-instance-store"

pattern ReportInstanceReasonCodes_Performance_network :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Performance_network = ReportInstanceReasonCodes' "performance-network"

pattern ReportInstanceReasonCodes_Performance_other :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Performance_other = ReportInstanceReasonCodes' "performance-other"

pattern ReportInstanceReasonCodes_Unresponsive :: ReportInstanceReasonCodes
pattern ReportInstanceReasonCodes_Unresponsive = ReportInstanceReasonCodes' "unresponsive"

{-# COMPLETE
  ReportInstanceReasonCodes_Instance_stuck_in_state,
  ReportInstanceReasonCodes_Not_accepting_credentials,
  ReportInstanceReasonCodes_Other,
  ReportInstanceReasonCodes_Password_not_available,
  ReportInstanceReasonCodes_Performance_ebs_volume,
  ReportInstanceReasonCodes_Performance_instance_store,
  ReportInstanceReasonCodes_Performance_network,
  ReportInstanceReasonCodes_Performance_other,
  ReportInstanceReasonCodes_Unresponsive,
  ReportInstanceReasonCodes'
  #-}
