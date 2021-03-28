{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVectorDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Shield.Types.AttackVectorDescription
  ( AttackVectorDescription (..)
  -- * Smart constructor
  , mkAttackVectorDescription
  -- * Lenses
  , avdVectorType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the attack.
--
-- /See:/ 'mkAttackVectorDescription' smart constructor.
newtype AttackVectorDescription = AttackVectorDescription'
  { vectorType :: Core.Text
    -- ^ The attack type. Valid values:
--
--
--     * UDP_TRAFFIC
--
--
--     * UDP_FRAGMENT
--
--
--     * GENERIC_UDP_REFLECTION
--
--
--     * DNS_REFLECTION
--
--
--     * NTP_REFLECTION
--
--
--     * CHARGEN_REFLECTION
--
--
--     * SSDP_REFLECTION
--
--
--     * PORT_MAPPER
--
--
--     * RIP_REFLECTION
--
--
--     * SNMP_REFLECTION
--
--
--     * MSSQL_REFLECTION
--
--
--     * NET_BIOS_REFLECTION
--
--
--     * SYN_FLOOD
--
--
--     * ACK_FLOOD
--
--
--     * REQUEST_FLOOD
--
--
--     * HTTP_REFLECTION
--
--
--     * UDS_REFLECTION
--
--
--     * MEMCACHED_REFLECTION
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AttackVectorDescription' value with any optional fields omitted.
mkAttackVectorDescription
    :: Core.Text -- ^ 'vectorType'
    -> AttackVectorDescription
mkAttackVectorDescription vectorType
  = AttackVectorDescription'{vectorType}

-- | The attack type. Valid values:
--
--
--     * UDP_TRAFFIC
--
--
--     * UDP_FRAGMENT
--
--
--     * GENERIC_UDP_REFLECTION
--
--
--     * DNS_REFLECTION
--
--
--     * NTP_REFLECTION
--
--
--     * CHARGEN_REFLECTION
--
--
--     * SSDP_REFLECTION
--
--
--     * PORT_MAPPER
--
--
--     * RIP_REFLECTION
--
--
--     * SNMP_REFLECTION
--
--
--     * MSSQL_REFLECTION
--
--
--     * NET_BIOS_REFLECTION
--
--
--     * SYN_FLOOD
--
--
--     * ACK_FLOOD
--
--
--     * REQUEST_FLOOD
--
--
--     * HTTP_REFLECTION
--
--
--     * UDS_REFLECTION
--
--
--     * MEMCACHED_REFLECTION
--
--
--
-- /Note:/ Consider using 'vectorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
avdVectorType :: Lens.Lens' AttackVectorDescription Core.Text
avdVectorType = Lens.field @"vectorType"
{-# INLINEABLE avdVectorType #-}
{-# DEPRECATED vectorType "Use generic-lens or generic-optics with 'vectorType' instead"  #-}

instance Core.FromJSON AttackVectorDescription where
        parseJSON
          = Core.withObject "AttackVectorDescription" Core.$
              \ x -> AttackVectorDescription' Core.<$> (x Core..: "VectorType")
