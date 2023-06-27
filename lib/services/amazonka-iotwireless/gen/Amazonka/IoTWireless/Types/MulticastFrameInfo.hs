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
-- Module      : Amazonka.IoTWireless.Types.MulticastFrameInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTWireless.Types.MulticastFrameInfo
  ( MulticastFrameInfo
      ( ..,
        MulticastFrameInfo_DISABLED,
        MulticastFrameInfo_ENABLED
      ),
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | @FrameInfo@ of your multicast group resources for the trace content. Use
-- FrameInfo to debug the multicast communication between your LoRaWAN end
-- devices and the network server.
newtype MulticastFrameInfo = MulticastFrameInfo'
  { fromMulticastFrameInfo ::
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

pattern MulticastFrameInfo_DISABLED :: MulticastFrameInfo
pattern MulticastFrameInfo_DISABLED = MulticastFrameInfo' "DISABLED"

pattern MulticastFrameInfo_ENABLED :: MulticastFrameInfo
pattern MulticastFrameInfo_ENABLED = MulticastFrameInfo' "ENABLED"

{-# COMPLETE
  MulticastFrameInfo_DISABLED,
  MulticastFrameInfo_ENABLED,
  MulticastFrameInfo'
  #-}
