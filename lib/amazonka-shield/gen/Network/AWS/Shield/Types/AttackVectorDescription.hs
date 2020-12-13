{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.AttackVectorDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.AttackVectorDescription
  ( AttackVectorDescription (..),

    -- * Smart constructor
    mkAttackVectorDescription,

    -- * Lenses
    avdVectorType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the attack.
--
-- /See:/ 'mkAttackVectorDescription' smart constructor.
newtype AttackVectorDescription = AttackVectorDescription'
  { -- | The attack type. Valid values:
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
    vectorType :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttackVectorDescription' with the minimum fields required to make a request.
--
-- * 'vectorType' - The attack type. Valid values:
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
mkAttackVectorDescription ::
  -- | 'vectorType'
  Lude.Text ->
  AttackVectorDescription
mkAttackVectorDescription pVectorType_ =
  AttackVectorDescription' {vectorType = pVectorType_}

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
avdVectorType :: Lens.Lens' AttackVectorDescription Lude.Text
avdVectorType = Lens.lens (vectorType :: AttackVectorDescription -> Lude.Text) (\s a -> s {vectorType = a} :: AttackVectorDescription)
{-# DEPRECATED avdVectorType "Use generic-lens or generic-optics with 'vectorType' instead." #-}

instance Lude.FromJSON AttackVectorDescription where
  parseJSON =
    Lude.withObject
      "AttackVectorDescription"
      (\x -> AttackVectorDescription' Lude.<$> (x Lude..: "VectorType"))
