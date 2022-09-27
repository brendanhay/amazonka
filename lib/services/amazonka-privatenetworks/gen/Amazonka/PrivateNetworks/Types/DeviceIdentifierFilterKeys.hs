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
-- Module      : Amazonka.PrivateNetworks.Types.DeviceIdentifierFilterKeys
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PrivateNetworks.Types.DeviceIdentifierFilterKeys
  ( DeviceIdentifierFilterKeys
      ( ..,
        DeviceIdentifierFilterKeys_ORDER,
        DeviceIdentifierFilterKeys_STATUS,
        DeviceIdentifierFilterKeys_TRAFFIC_GROUP
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Prelude as Prelude

newtype DeviceIdentifierFilterKeys = DeviceIdentifierFilterKeys'
  { fromDeviceIdentifierFilterKeys ::
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

pattern DeviceIdentifierFilterKeys_ORDER :: DeviceIdentifierFilterKeys
pattern DeviceIdentifierFilterKeys_ORDER = DeviceIdentifierFilterKeys' "ORDER"

pattern DeviceIdentifierFilterKeys_STATUS :: DeviceIdentifierFilterKeys
pattern DeviceIdentifierFilterKeys_STATUS = DeviceIdentifierFilterKeys' "STATUS"

pattern DeviceIdentifierFilterKeys_TRAFFIC_GROUP :: DeviceIdentifierFilterKeys
pattern DeviceIdentifierFilterKeys_TRAFFIC_GROUP = DeviceIdentifierFilterKeys' "TRAFFIC_GROUP"

{-# COMPLETE
  DeviceIdentifierFilterKeys_ORDER,
  DeviceIdentifierFilterKeys_STATUS,
  DeviceIdentifierFilterKeys_TRAFFIC_GROUP,
  DeviceIdentifierFilterKeys'
  #-}
