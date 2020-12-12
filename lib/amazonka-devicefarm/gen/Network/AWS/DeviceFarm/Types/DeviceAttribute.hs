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
        ARN,
        AppiumVersion,
        Availability,
        FleetType,
        FormFactor,
        InstanceARN,
        InstanceLabels,
        Manufacturer,
        Model,
        OSVersion,
        Platform,
        RemoteAccessEnabled,
        RemoteDebugEnabled
      ),
  )
where

import qualified Network.AWS.Prelude as Lude

newtype DeviceAttribute = DeviceAttribute' Lude.Text
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData,
      Lude.ToJSONKey,
      Lude.FromJSONKey,
      Lude.ToJSON,
      Lude.FromJSON,
      Lude.ToXML,
      Lude.FromXML,
      Lude.ToText,
      Lude.FromText,
      Lude.ToByteString,
      Lude.ToQuery,
      Lude.ToHeader
    )

pattern ARN :: DeviceAttribute
pattern ARN = DeviceAttribute' "ARN"

pattern AppiumVersion :: DeviceAttribute
pattern AppiumVersion = DeviceAttribute' "APPIUM_VERSION"

pattern Availability :: DeviceAttribute
pattern Availability = DeviceAttribute' "AVAILABILITY"

pattern FleetType :: DeviceAttribute
pattern FleetType = DeviceAttribute' "FLEET_TYPE"

pattern FormFactor :: DeviceAttribute
pattern FormFactor = DeviceAttribute' "FORM_FACTOR"

pattern InstanceARN :: DeviceAttribute
pattern InstanceARN = DeviceAttribute' "INSTANCE_ARN"

pattern InstanceLabels :: DeviceAttribute
pattern InstanceLabels = DeviceAttribute' "INSTANCE_LABELS"

pattern Manufacturer :: DeviceAttribute
pattern Manufacturer = DeviceAttribute' "MANUFACTURER"

pattern Model :: DeviceAttribute
pattern Model = DeviceAttribute' "MODEL"

pattern OSVersion :: DeviceAttribute
pattern OSVersion = DeviceAttribute' "OS_VERSION"

pattern Platform :: DeviceAttribute
pattern Platform = DeviceAttribute' "PLATFORM"

pattern RemoteAccessEnabled :: DeviceAttribute
pattern RemoteAccessEnabled = DeviceAttribute' "REMOTE_ACCESS_ENABLED"

pattern RemoteDebugEnabled :: DeviceAttribute
pattern RemoteDebugEnabled = DeviceAttribute' "REMOTE_DEBUG_ENABLED"

{-# COMPLETE
  ARN,
  AppiumVersion,
  Availability,
  FleetType,
  FormFactor,
  InstanceARN,
  InstanceLabels,
  Manufacturer,
  Model,
  OSVersion,
  Platform,
  RemoteAccessEnabled,
  RemoteDebugEnabled,
  DeviceAttribute'
  #-}
