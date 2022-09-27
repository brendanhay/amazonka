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
-- Module      : Amazonka.PrivateNetworks.Types.DeviceIdentifierStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.DeviceIdentifierStatus
  ( DeviceIdentifierStatus
      ( ..,
        DeviceIdentifierStatus_ACTIVE,
        DeviceIdentifierStatus_INACTIVE
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeviceIdentifierStatus = DeviceIdentifierStatus'
  { fromDeviceIdentifierStatus ::
      Core.Text
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

pattern DeviceIdentifierStatus_ACTIVE :: DeviceIdentifierStatus
pattern DeviceIdentifierStatus_ACTIVE = DeviceIdentifierStatus' "ACTIVE"

pattern DeviceIdentifierStatus_INACTIVE :: DeviceIdentifierStatus
pattern DeviceIdentifierStatus_INACTIVE = DeviceIdentifierStatus' "INACTIVE"

{-# COMPLETE
  DeviceIdentifierStatus_ACTIVE,
  DeviceIdentifierStatus_INACTIVE,
  DeviceIdentifierStatus'
  #-}
