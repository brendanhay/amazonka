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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

newtype DeviceIdentifierFilterKeys = DeviceIdentifierFilterKeys'
  { fromDeviceIdentifierFilterKeys ::
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
