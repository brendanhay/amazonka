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

import Network.AWS.EC2.Internal
import qualified Network.AWS.Prelude as Prelude

newtype ReportInstanceReasonCodes = ReportInstanceReasonCodes'
  { fromReportInstanceReasonCodes ::
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
