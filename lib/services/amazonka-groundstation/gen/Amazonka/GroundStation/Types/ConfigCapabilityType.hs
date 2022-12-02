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
-- Module      : Amazonka.GroundStation.Types.ConfigCapabilityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.ConfigCapabilityType
  ( ConfigCapabilityType
      ( ..,
        ConfigCapabilityType_Antenna_downlink,
        ConfigCapabilityType_Antenna_downlink_demod_decode,
        ConfigCapabilityType_Antenna_uplink,
        ConfigCapabilityType_Dataflow_endpoint,
        ConfigCapabilityType_S3_recording,
        ConfigCapabilityType_Tracking,
        ConfigCapabilityType_Uplink_echo
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype ConfigCapabilityType = ConfigCapabilityType'
  { fromConfigCapabilityType ::
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

pattern ConfigCapabilityType_Antenna_downlink :: ConfigCapabilityType
pattern ConfigCapabilityType_Antenna_downlink = ConfigCapabilityType' "antenna-downlink"

pattern ConfigCapabilityType_Antenna_downlink_demod_decode :: ConfigCapabilityType
pattern ConfigCapabilityType_Antenna_downlink_demod_decode = ConfigCapabilityType' "antenna-downlink-demod-decode"

pattern ConfigCapabilityType_Antenna_uplink :: ConfigCapabilityType
pattern ConfigCapabilityType_Antenna_uplink = ConfigCapabilityType' "antenna-uplink"

pattern ConfigCapabilityType_Dataflow_endpoint :: ConfigCapabilityType
pattern ConfigCapabilityType_Dataflow_endpoint = ConfigCapabilityType' "dataflow-endpoint"

pattern ConfigCapabilityType_S3_recording :: ConfigCapabilityType
pattern ConfigCapabilityType_S3_recording = ConfigCapabilityType' "s3-recording"

pattern ConfigCapabilityType_Tracking :: ConfigCapabilityType
pattern ConfigCapabilityType_Tracking = ConfigCapabilityType' "tracking"

pattern ConfigCapabilityType_Uplink_echo :: ConfigCapabilityType
pattern ConfigCapabilityType_Uplink_echo = ConfigCapabilityType' "uplink-echo"

{-# COMPLETE
  ConfigCapabilityType_Antenna_downlink,
  ConfigCapabilityType_Antenna_downlink_demod_decode,
  ConfigCapabilityType_Antenna_uplink,
  ConfigCapabilityType_Dataflow_endpoint,
  ConfigCapabilityType_S3_recording,
  ConfigCapabilityType_Tracking,
  ConfigCapabilityType_Uplink_echo,
  ConfigCapabilityType'
  #-}
