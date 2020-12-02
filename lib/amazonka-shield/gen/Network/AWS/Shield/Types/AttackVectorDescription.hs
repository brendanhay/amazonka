{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVectorDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVectorDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the attack.
--
--
--
-- /See:/ 'attackVectorDescription' smart constructor.
newtype AttackVectorDescription = AttackVectorDescription'
  { _avdVectorType ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'AttackVectorDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'avdVectorType' - The attack type. Valid values:     * UDP_TRAFFIC     * UDP_FRAGMENT     * GENERIC_UDP_REFLECTION     * DNS_REFLECTION     * NTP_REFLECTION     * CHARGEN_REFLECTION     * SSDP_REFLECTION     * PORT_MAPPER     * RIP_REFLECTION     * SNMP_REFLECTION     * MSSQL_REFLECTION     * NET_BIOS_REFLECTION     * SYN_FLOOD     * ACK_FLOOD     * REQUEST_FLOOD     * HTTP_REFLECTION     * UDS_REFLECTION     * MEMCACHED_REFLECTION
attackVectorDescription ::
  -- | 'avdVectorType'
  Text ->
  AttackVectorDescription
attackVectorDescription pVectorType_ =
  AttackVectorDescription' {_avdVectorType = pVectorType_}

-- | The attack type. Valid values:     * UDP_TRAFFIC     * UDP_FRAGMENT     * GENERIC_UDP_REFLECTION     * DNS_REFLECTION     * NTP_REFLECTION     * CHARGEN_REFLECTION     * SSDP_REFLECTION     * PORT_MAPPER     * RIP_REFLECTION     * SNMP_REFLECTION     * MSSQL_REFLECTION     * NET_BIOS_REFLECTION     * SYN_FLOOD     * ACK_FLOOD     * REQUEST_FLOOD     * HTTP_REFLECTION     * UDS_REFLECTION     * MEMCACHED_REFLECTION
avdVectorType :: Lens' AttackVectorDescription Text
avdVectorType = lens _avdVectorType (\s a -> s {_avdVectorType = a})

instance FromJSON AttackVectorDescription where
  parseJSON =
    withObject
      "AttackVectorDescription"
      (\x -> AttackVectorDescription' <$> (x .: "VectorType"))

instance Hashable AttackVectorDescription

instance NFData AttackVectorDescription
