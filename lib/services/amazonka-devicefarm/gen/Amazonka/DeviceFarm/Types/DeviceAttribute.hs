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
-- Module      : Amazonka.DeviceFarm.Types.DeviceAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.DeviceAttribute
  ( DeviceAttribute
      ( ..,
        DeviceAttribute_APPIUM_VERSION,
        DeviceAttribute_ARN,
        DeviceAttribute_AVAILABILITY,
        DeviceAttribute_FLEET_TYPE,
        DeviceAttribute_FORM_FACTOR,
        DeviceAttribute_INSTANCE_ARN,
        DeviceAttribute_INSTANCE_LABELS,
        DeviceAttribute_MANUFACTURER,
        DeviceAttribute_MODEL,
        DeviceAttribute_OS_VERSION,
        DeviceAttribute_PLATFORM,
        DeviceAttribute_REMOTE_ACCESS_ENABLED,
        DeviceAttribute_REMOTE_DEBUG_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceAttribute = DeviceAttribute'
  { fromDeviceAttribute ::
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

pattern DeviceAttribute_APPIUM_VERSION :: DeviceAttribute
pattern DeviceAttribute_APPIUM_VERSION = DeviceAttribute' "APPIUM_VERSION"

pattern DeviceAttribute_ARN :: DeviceAttribute
pattern DeviceAttribute_ARN = DeviceAttribute' "ARN"

pattern DeviceAttribute_AVAILABILITY :: DeviceAttribute
pattern DeviceAttribute_AVAILABILITY = DeviceAttribute' "AVAILABILITY"

pattern DeviceAttribute_FLEET_TYPE :: DeviceAttribute
pattern DeviceAttribute_FLEET_TYPE = DeviceAttribute' "FLEET_TYPE"

pattern DeviceAttribute_FORM_FACTOR :: DeviceAttribute
pattern DeviceAttribute_FORM_FACTOR = DeviceAttribute' "FORM_FACTOR"

pattern DeviceAttribute_INSTANCE_ARN :: DeviceAttribute
pattern DeviceAttribute_INSTANCE_ARN = DeviceAttribute' "INSTANCE_ARN"

pattern DeviceAttribute_INSTANCE_LABELS :: DeviceAttribute
pattern DeviceAttribute_INSTANCE_LABELS = DeviceAttribute' "INSTANCE_LABELS"

pattern DeviceAttribute_MANUFACTURER :: DeviceAttribute
pattern DeviceAttribute_MANUFACTURER = DeviceAttribute' "MANUFACTURER"

pattern DeviceAttribute_MODEL :: DeviceAttribute
pattern DeviceAttribute_MODEL = DeviceAttribute' "MODEL"

pattern DeviceAttribute_OS_VERSION :: DeviceAttribute
pattern DeviceAttribute_OS_VERSION = DeviceAttribute' "OS_VERSION"

pattern DeviceAttribute_PLATFORM :: DeviceAttribute
pattern DeviceAttribute_PLATFORM = DeviceAttribute' "PLATFORM"

pattern DeviceAttribute_REMOTE_ACCESS_ENABLED :: DeviceAttribute
pattern DeviceAttribute_REMOTE_ACCESS_ENABLED = DeviceAttribute' "REMOTE_ACCESS_ENABLED"

pattern DeviceAttribute_REMOTE_DEBUG_ENABLED :: DeviceAttribute
pattern DeviceAttribute_REMOTE_DEBUG_ENABLED = DeviceAttribute' "REMOTE_DEBUG_ENABLED"

{-# COMPLETE
  DeviceAttribute_APPIUM_VERSION,
  DeviceAttribute_ARN,
  DeviceAttribute_AVAILABILITY,
  DeviceAttribute_FLEET_TYPE,
  DeviceAttribute_FORM_FACTOR,
  DeviceAttribute_INSTANCE_ARN,
  DeviceAttribute_INSTANCE_LABELS,
  DeviceAttribute_MANUFACTURER,
  DeviceAttribute_MODEL,
  DeviceAttribute_OS_VERSION,
  DeviceAttribute_PLATFORM,
  DeviceAttribute_REMOTE_ACCESS_ENABLED,
  DeviceAttribute_REMOTE_DEBUG_ENABLED,
  DeviceAttribute'
  #-}
