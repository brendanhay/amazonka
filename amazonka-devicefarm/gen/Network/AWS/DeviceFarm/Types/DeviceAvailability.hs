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
-- Module      : Network.AWS.DeviceFarm.Types.DeviceAvailability
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.DeviceAvailability
  ( DeviceAvailability
      ( ..,
        DeviceAvailability_AVAILABLE,
        DeviceAvailability_BUSY,
        DeviceAvailability_HIGHLY_AVAILABLE,
        DeviceAvailability_TEMPORARY_NOT_AVAILABLE
      ),
  )
where

import qualified Network.AWS.Prelude as Prelude

newtype DeviceAvailability = DeviceAvailability'
  { fromDeviceAvailability ::
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

pattern DeviceAvailability_AVAILABLE :: DeviceAvailability
pattern DeviceAvailability_AVAILABLE = DeviceAvailability' "AVAILABLE"

pattern DeviceAvailability_BUSY :: DeviceAvailability
pattern DeviceAvailability_BUSY = DeviceAvailability' "BUSY"

pattern DeviceAvailability_HIGHLY_AVAILABLE :: DeviceAvailability
pattern DeviceAvailability_HIGHLY_AVAILABLE = DeviceAvailability' "HIGHLY_AVAILABLE"

pattern DeviceAvailability_TEMPORARY_NOT_AVAILABLE :: DeviceAvailability
pattern DeviceAvailability_TEMPORARY_NOT_AVAILABLE = DeviceAvailability' "TEMPORARY_NOT_AVAILABLE"

{-# COMPLETE
  DeviceAvailability_AVAILABLE,
  DeviceAvailability_BUSY,
  DeviceAvailability_HIGHLY_AVAILABLE,
  DeviceAvailability_TEMPORARY_NOT_AVAILABLE,
  DeviceAvailability'
  #-}
