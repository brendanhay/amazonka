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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceFilterAttribute
  ( DeviceFilterAttribute
      ( ..,
        DeviceFilterAttribute_ARN,
        DeviceFilterAttribute_AVAILABILITY,
        DeviceFilterAttribute_FLEET_TYPE,
        DeviceFilterAttribute_FORM_FACTOR,
        DeviceFilterAttribute_INSTANCE_ARN,
        DeviceFilterAttribute_INSTANCE_LABELS,
        DeviceFilterAttribute_MANUFACTURER,
        DeviceFilterAttribute_MODEL,
        DeviceFilterAttribute_OS_VERSION,
        DeviceFilterAttribute_PLATFORM,
        DeviceFilterAttribute_REMOTE_ACCESS_ENABLED,
        DeviceFilterAttribute_REMOTE_DEBUG_ENABLED
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeviceFilterAttribute = DeviceFilterAttribute'
  { fromDeviceFilterAttribute ::
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

pattern DeviceFilterAttribute_ARN :: DeviceFilterAttribute
pattern DeviceFilterAttribute_ARN = DeviceFilterAttribute' "ARN"

pattern DeviceFilterAttribute_AVAILABILITY :: DeviceFilterAttribute
pattern DeviceFilterAttribute_AVAILABILITY = DeviceFilterAttribute' "AVAILABILITY"

pattern DeviceFilterAttribute_FLEET_TYPE :: DeviceFilterAttribute
pattern DeviceFilterAttribute_FLEET_TYPE = DeviceFilterAttribute' "FLEET_TYPE"

pattern DeviceFilterAttribute_FORM_FACTOR :: DeviceFilterAttribute
pattern DeviceFilterAttribute_FORM_FACTOR = DeviceFilterAttribute' "FORM_FACTOR"

pattern DeviceFilterAttribute_INSTANCE_ARN :: DeviceFilterAttribute
pattern DeviceFilterAttribute_INSTANCE_ARN = DeviceFilterAttribute' "INSTANCE_ARN"

pattern DeviceFilterAttribute_INSTANCE_LABELS :: DeviceFilterAttribute
pattern DeviceFilterAttribute_INSTANCE_LABELS = DeviceFilterAttribute' "INSTANCE_LABELS"

pattern DeviceFilterAttribute_MANUFACTURER :: DeviceFilterAttribute
pattern DeviceFilterAttribute_MANUFACTURER = DeviceFilterAttribute' "MANUFACTURER"

pattern DeviceFilterAttribute_MODEL :: DeviceFilterAttribute
pattern DeviceFilterAttribute_MODEL = DeviceFilterAttribute' "MODEL"

pattern DeviceFilterAttribute_OS_VERSION :: DeviceFilterAttribute
pattern DeviceFilterAttribute_OS_VERSION = DeviceFilterAttribute' "OS_VERSION"

pattern DeviceFilterAttribute_PLATFORM :: DeviceFilterAttribute
pattern DeviceFilterAttribute_PLATFORM = DeviceFilterAttribute' "PLATFORM"

pattern DeviceFilterAttribute_REMOTE_ACCESS_ENABLED :: DeviceFilterAttribute
pattern DeviceFilterAttribute_REMOTE_ACCESS_ENABLED = DeviceFilterAttribute' "REMOTE_ACCESS_ENABLED"

pattern DeviceFilterAttribute_REMOTE_DEBUG_ENABLED :: DeviceFilterAttribute
pattern DeviceFilterAttribute_REMOTE_DEBUG_ENABLED = DeviceFilterAttribute' "REMOTE_DEBUG_ENABLED"

{-# COMPLETE
  DeviceFilterAttribute_ARN,
  DeviceFilterAttribute_AVAILABILITY,
  DeviceFilterAttribute_FLEET_TYPE,
  DeviceFilterAttribute_FORM_FACTOR,
  DeviceFilterAttribute_INSTANCE_ARN,
  DeviceFilterAttribute_INSTANCE_LABELS,
  DeviceFilterAttribute_MANUFACTURER,
  DeviceFilterAttribute_MODEL,
  DeviceFilterAttribute_OS_VERSION,
  DeviceFilterAttribute_PLATFORM,
  DeviceFilterAttribute_REMOTE_ACCESS_ENABLED,
  DeviceFilterAttribute_REMOTE_DEBUG_ENABLED,
  DeviceFilterAttribute'
  #-}
