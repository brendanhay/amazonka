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
-- Module      : Amazonka.IoT.Types.DeviceDefenderIndexingMode
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.DeviceDefenderIndexingMode
  ( DeviceDefenderIndexingMode
      ( ..,
        DeviceDefenderIndexingMode_OFF,
        DeviceDefenderIndexingMode_VIOLATIONS
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceDefenderIndexingMode = DeviceDefenderIndexingMode'
  { fromDeviceDefenderIndexingMode ::
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

pattern DeviceDefenderIndexingMode_OFF :: DeviceDefenderIndexingMode
pattern DeviceDefenderIndexingMode_OFF = DeviceDefenderIndexingMode' "OFF"

pattern DeviceDefenderIndexingMode_VIOLATIONS :: DeviceDefenderIndexingMode
pattern DeviceDefenderIndexingMode_VIOLATIONS = DeviceDefenderIndexingMode' "VIOLATIONS"

{-# COMPLETE
  DeviceDefenderIndexingMode_OFF,
  DeviceDefenderIndexingMode_VIOLATIONS,
  DeviceDefenderIndexingMode'
  #-}
