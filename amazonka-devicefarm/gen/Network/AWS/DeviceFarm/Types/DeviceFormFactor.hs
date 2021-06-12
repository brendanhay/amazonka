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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceFormFactor
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceFormFactor
  ( DeviceFormFactor
      ( ..,
        DeviceFormFactor_PHONE,
        DeviceFormFactor_TABLET
      ),
  )
where

import qualified Network.AWS.Core as Core

newtype DeviceFormFactor = DeviceFormFactor'
  { fromDeviceFormFactor ::
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

pattern DeviceFormFactor_PHONE :: DeviceFormFactor
pattern DeviceFormFactor_PHONE = DeviceFormFactor' "PHONE"

pattern DeviceFormFactor_TABLET :: DeviceFormFactor
pattern DeviceFormFactor_TABLET = DeviceFormFactor' "TABLET"

{-# COMPLETE
  DeviceFormFactor_PHONE,
  DeviceFormFactor_TABLET,
  DeviceFormFactor'
  #-}
