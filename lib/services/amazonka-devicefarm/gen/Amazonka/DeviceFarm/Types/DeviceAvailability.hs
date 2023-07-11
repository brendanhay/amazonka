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
-- Module      : Amazonka.DeviceFarm.Types.DeviceAvailability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DeviceFarm.Types.DeviceAvailability
  ( DeviceAvailability
      ( ..,
        DeviceAvailability_AVAILABLE,
        DeviceAvailability_BUSY,
        DeviceAvailability_HIGHLY_AVAILABLE,
        DeviceAvailability_TEMPORARY_NOT_AVAILABLE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceAvailability = DeviceAvailability'
  { fromDeviceAvailability ::
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
