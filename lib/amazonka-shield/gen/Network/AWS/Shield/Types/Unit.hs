{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.Unit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.Unit
  ( Unit
      ( Unit',
        UnitBits,
        UnitBytes,
        UnitPackets,
        UnitRequests,
        fromUnit
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype Unit = Unit' {fromUnit :: Core.Text}
  deriving stock
    ( Core.Eq,
      Core.Ord,
      Core.Read,
      Core.Show,
      Core.Generic
    )
  deriving newtype
    ( Core.IsString,
      Core.Hashable,
      Core.NFData,
      Core.ToJSONKey,
      Core.FromJSONKey,
      Core.ToJSON,
      Core.FromJSON,
      Core.ToXML,
      Core.FromXML,
      Core.ToText,
      Core.FromText,
      Core.ToByteString,
      Core.ToQuery,
      Core.ToHeader
    )

pattern UnitBits :: Unit
pattern UnitBits = Unit' "BITS"

pattern UnitBytes :: Unit
pattern UnitBytes = Unit' "BYTES"

pattern UnitPackets :: Unit
pattern UnitPackets = Unit' "PACKETS"

pattern UnitRequests :: Unit
pattern UnitRequests = Unit' "REQUESTS"

{-# COMPLETE
  UnitBits,
  UnitBytes,
  UnitPackets,
  UnitRequests,
  Unit'
  #-}
