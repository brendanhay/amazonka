{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ConnectionType
  ( ConnectionType
    ( ConnectionType'
    , ConnectionTypeJdbc
    , ConnectionTypeSftp
    , ConnectionTypeMongodb
    , ConnectionTypeKafka
    , ConnectionTypeNetwork
    , fromConnectionType
    )
  ) where

import qualified Network.AWS.Prelude as Core

newtype ConnectionType = ConnectionType'{fromConnectionType ::
                                         Core.Text}
                           deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show,
                                           Core.Generic)
                           deriving newtype (Core.IsString, Core.Hashable, Core.NFData,
                                             Core.ToJSONKey, Core.FromJSONKey, Core.ToJSON,
                                             Core.FromJSON, Core.ToXML, Core.FromXML, Core.ToText,
                                             Core.FromText, Core.ToByteString, Core.ToQuery,
                                             Core.ToHeader)

pattern ConnectionTypeJdbc :: ConnectionType
pattern ConnectionTypeJdbc = ConnectionType' "JDBC"

pattern ConnectionTypeSftp :: ConnectionType
pattern ConnectionTypeSftp = ConnectionType' "SFTP"

pattern ConnectionTypeMongodb :: ConnectionType
pattern ConnectionTypeMongodb = ConnectionType' "MONGODB"

pattern ConnectionTypeKafka :: ConnectionType
pattern ConnectionTypeKafka = ConnectionType' "KAFKA"

pattern ConnectionTypeNetwork :: ConnectionType
pattern ConnectionTypeNetwork = ConnectionType' "NETWORK"

{-# COMPLETE 
  ConnectionTypeJdbc,

  ConnectionTypeSftp,

  ConnectionTypeMongodb,

  ConnectionTypeKafka,

  ConnectionTypeNetwork,
  ConnectionType'
  #-}
