{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
  ( DeviceFilterAttribute
    ( DeviceFilterAttribute'
    , DeviceFilterAttributeArn
    , DeviceFilterAttributePlatform
    , DeviceFilterAttributeOsVersion
    , DeviceFilterAttributeModel
    , DeviceFilterAttributeAvailability
    , DeviceFilterAttributeFormFactor
    , DeviceFilterAttributeManufacturer
    , DeviceFilterAttributeRemoteAccessEnabled
    , DeviceFilterAttributeRemoteDebugEnabled
    , DeviceFilterAttributeInstanceArn
    , DeviceFilterAttributeInstanceLabels
    , DeviceFilterAttributeFleetType
    , fromDeviceFilterAttribute
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype DeviceFilterAttribute = DeviceFilterAttribute'{fromDeviceFilterAttribute
                                                       :: Core.Text}
                                  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                                  Core.Generic)
                                  deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                                    Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                                    Core.FromJSON, Core.ToXML, Core.FromXML,
                                                    Core.ToText, Core.FromText, Core.ToByteString,
                                                    Core.ToQuery, Core.ToHeader)

pattern DeviceFilterAttributeArn :: DeviceFilterAttribute
pattern DeviceFilterAttributeArn = DeviceFilterAttribute' "ARN"

pattern DeviceFilterAttributePlatform :: DeviceFilterAttribute
pattern DeviceFilterAttributePlatform = DeviceFilterAttribute' "PLATFORM"

pattern DeviceFilterAttributeOsVersion :: DeviceFilterAttribute
pattern DeviceFilterAttributeOsVersion = DeviceFilterAttribute' "OS_VERSION"

pattern DeviceFilterAttributeModel :: DeviceFilterAttribute
pattern DeviceFilterAttributeModel = DeviceFilterAttribute' "MODEL"

pattern DeviceFilterAttributeAvailability :: DeviceFilterAttribute
pattern DeviceFilterAttributeAvailability = DeviceFilterAttribute' "AVAILABILITY"

pattern DeviceFilterAttributeFormFactor :: DeviceFilterAttribute
pattern DeviceFilterAttributeFormFactor = DeviceFilterAttribute' "FORM_FACTOR"

pattern DeviceFilterAttributeManufacturer :: DeviceFilterAttribute
pattern DeviceFilterAttributeManufacturer = DeviceFilterAttribute' "MANUFACTURER"

pattern DeviceFilterAttributeRemoteAccessEnabled :: DeviceFilterAttribute
pattern DeviceFilterAttributeRemoteAccessEnabled = DeviceFilterAttribute' "REMOTE_ACCESS_ENABLED"

pattern DeviceFilterAttributeRemoteDebugEnabled :: DeviceFilterAttribute
pattern DeviceFilterAttributeRemoteDebugEnabled = DeviceFilterAttribute' "REMOTE_DEBUG_ENABLED"

pattern DeviceFilterAttributeInstanceArn :: DeviceFilterAttribute
pattern DeviceFilterAttributeInstanceArn = DeviceFilterAttribute' "INSTANCE_ARN"

pattern DeviceFilterAttributeInstanceLabels :: DeviceFilterAttribute
pattern DeviceFilterAttributeInstanceLabels = DeviceFilterAttribute' "INSTANCE_LABELS"

pattern DeviceFilterAttributeFleetType :: DeviceFilterAttribute
pattern DeviceFilterAttributeFleetType = DeviceFilterAttribute' "FLEET_TYPE"

{-# COMPLETE 
  DeviceFilterAttributeArn,

  DeviceFilterAttributePlatform,

  DeviceFilterAttributeOsVersion,

  DeviceFilterAttributeModel,

  DeviceFilterAttributeAvailability,

  DeviceFilterAttributeFormFactor,

  DeviceFilterAttributeManufacturer,

  DeviceFilterAttributeRemoteAccessEnabled,

  DeviceFilterAttributeRemoteDebugEnabled,

  DeviceFilterAttributeInstanceArn,

  DeviceFilterAttributeInstanceLabels,

  DeviceFilterAttributeFleetType,
  DeviceFilterAttribute'
  #-}
