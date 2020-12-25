{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceAttribute
  ( DeviceAttribute
      ( DeviceAttribute',
        DeviceAttributeArn,
        DeviceAttributePlatform,
        DeviceAttributeFormFactor,
        DeviceAttributeManufacturer,
        DeviceAttributeRemoteAccessEnabled,
        DeviceAttributeRemoteDebugEnabled,
        DeviceAttributeAppiumVersion,
        DeviceAttributeInstanceArn,
        DeviceAttributeInstanceLabels,
        DeviceAttributeFleetType,
        DeviceAttributeOsVersion,
        DeviceAttributeModel,
        DeviceAttributeAvailability,
        fromDeviceAttribute
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype DeviceAttribute = DeviceAttribute'
  { fromDeviceAttribute ::
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

pattern DeviceAttributeArn :: DeviceAttribute
pattern DeviceAttributeArn = DeviceAttribute' "ARN"

pattern DeviceAttributePlatform :: DeviceAttribute
pattern DeviceAttributePlatform = DeviceAttribute' "PLATFORM"

pattern DeviceAttributeFormFactor :: DeviceAttribute
pattern DeviceAttributeFormFactor = DeviceAttribute' "FORM_FACTOR"

pattern DeviceAttributeManufacturer :: DeviceAttribute
pattern DeviceAttributeManufacturer = DeviceAttribute' "MANUFACTURER"

pattern DeviceAttributeRemoteAccessEnabled :: DeviceAttribute
pattern DeviceAttributeRemoteAccessEnabled = DeviceAttribute' "REMOTE_ACCESS_ENABLED"

pattern DeviceAttributeRemoteDebugEnabled :: DeviceAttribute
pattern DeviceAttributeRemoteDebugEnabled = DeviceAttribute' "REMOTE_DEBUG_ENABLED"

pattern DeviceAttributeAppiumVersion :: DeviceAttribute
pattern DeviceAttributeAppiumVersion = DeviceAttribute' "APPIUM_VERSION"

pattern DeviceAttributeInstanceArn :: DeviceAttribute
pattern DeviceAttributeInstanceArn = DeviceAttribute' "INSTANCE_ARN"

pattern DeviceAttributeInstanceLabels :: DeviceAttribute
pattern DeviceAttributeInstanceLabels = DeviceAttribute' "INSTANCE_LABELS"

pattern DeviceAttributeFleetType :: DeviceAttribute
pattern DeviceAttributeFleetType = DeviceAttribute' "FLEET_TYPE"

pattern DeviceAttributeOsVersion :: DeviceAttribute
pattern DeviceAttributeOsVersion = DeviceAttribute' "OS_VERSION"

pattern DeviceAttributeModel :: DeviceAttribute
pattern DeviceAttributeModel = DeviceAttribute' "MODEL"

pattern DeviceAttributeAvailability :: DeviceAttribute
pattern DeviceAttributeAvailability = DeviceAttribute' "AVAILABILITY"

{-# COMPLETE
  DeviceAttributeArn,
  DeviceAttributePlatform,
  DeviceAttributeFormFactor,
  DeviceAttributeManufacturer,
  DeviceAttributeRemoteAccessEnabled,
  DeviceAttributeRemoteDebugEnabled,
  DeviceAttributeAppiumVersion,
  DeviceAttributeInstanceArn,
  DeviceAttributeInstanceLabels,
  DeviceAttributeFleetType,
  DeviceAttributeOsVersion,
  DeviceAttributeModel,
  DeviceAttributeAvailability,
  DeviceAttribute'
  #-}
