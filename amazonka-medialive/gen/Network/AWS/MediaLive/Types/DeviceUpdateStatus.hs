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
-- Module      : Network.AWS.MediaLive.Types.DeviceUpdateStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.DeviceUpdateStatus
  ( DeviceUpdateStatus
      ( ..,
        DeviceUpdateStatus_NOT_UP_TO_DATE,
        DeviceUpdateStatus_UP_TO_DATE
      ),
  )
where

import qualified Network.AWS.Core as Core

-- | The status of software on the input device.
newtype DeviceUpdateStatus = DeviceUpdateStatus'
  { fromDeviceUpdateStatus ::
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

pattern DeviceUpdateStatus_NOT_UP_TO_DATE :: DeviceUpdateStatus
pattern DeviceUpdateStatus_NOT_UP_TO_DATE = DeviceUpdateStatus' "NOT_UP_TO_DATE"

pattern DeviceUpdateStatus_UP_TO_DATE :: DeviceUpdateStatus
pattern DeviceUpdateStatus_UP_TO_DATE = DeviceUpdateStatus' "UP_TO_DATE"

{-# COMPLETE
  DeviceUpdateStatus_NOT_UP_TO_DATE,
  DeviceUpdateStatus_UP_TO_DATE,
  DeviceUpdateStatus'
  #-}
