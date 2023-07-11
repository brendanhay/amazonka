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
-- Module      : Amazonka.EC2.Types.ReportInstanceReasonCodes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ReportInstanceReasonCodes
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

newtype ReportInstanceReasonCodes = ReportInstanceReasonCodes'
  { fromReportInstanceReasonCodes ::
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
