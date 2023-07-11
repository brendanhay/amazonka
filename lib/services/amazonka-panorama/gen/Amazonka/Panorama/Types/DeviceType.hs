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
-- Module      : Amazonka.Panorama.Types.DeviceType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.DeviceType
  ( DeviceType
      ( ..,
        DeviceType_PANORAMA_APPLIANCE,
        DeviceType_PANORAMA_APPLIANCE_DEVELOPER_KIT
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceType = DeviceType'
  { fromDeviceType ::
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

pattern DeviceType_PANORAMA_APPLIANCE :: DeviceType
pattern DeviceType_PANORAMA_APPLIANCE = DeviceType' "PANORAMA_APPLIANCE"

pattern DeviceType_PANORAMA_APPLIANCE_DEVELOPER_KIT :: DeviceType
pattern DeviceType_PANORAMA_APPLIANCE_DEVELOPER_KIT = DeviceType' "PANORAMA_APPLIANCE_DEVELOPER_KIT"

{-# COMPLETE
  DeviceType_PANORAMA_APPLIANCE,
  DeviceType_PANORAMA_APPLIANCE_DEVELOPER_KIT,
  DeviceType'
  #-}
