{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Cloud9.Types.ConnectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Cloud9.Types.ConnectionType
  ( ConnectionType
      ( ConnectionType',
        ConnectionTypeConnectSsh,
        ConnectionTypeConnectSsm,
        fromConnectionType
      ),
  )
where

import qualified Network.AWS.Prelude as Core

newtype ConnectionType = ConnectionType'
  { fromConnectionType ::
      Core.Text
  }
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

pattern ConnectionTypeConnectSsh :: ConnectionType
pattern ConnectionTypeConnectSsh = ConnectionType' "CONNECT_SSH"

pattern ConnectionTypeConnectSsm :: ConnectionType
pattern ConnectionTypeConnectSsm = ConnectionType' "CONNECT_SSM"

{-# COMPLETE
  ConnectionTypeConnectSsh,
  ConnectionTypeConnectSsm,
  ConnectionType'
  #-}
