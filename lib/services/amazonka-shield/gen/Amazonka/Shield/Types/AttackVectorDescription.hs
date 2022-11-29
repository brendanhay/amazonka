{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Shield.Types.AttackVectorDescription
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Shield.Types.AttackVectorDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the attack.
--
-- /See:/ 'newAttackVectorDescription' smart constructor.
data AttackVectorDescription = AttackVectorDescription'
  { -- | The attack type. Valid values:
    --
    -- -   UDP_TRAFFIC
    --
    -- -   UDP_FRAGMENT
    --
    -- -   GENERIC_UDP_REFLECTION
    --
    -- -   DNS_REFLECTION
    --
    -- -   NTP_REFLECTION
    --
    -- -   CHARGEN_REFLECTION
    --
    -- -   SSDP_REFLECTION
    --
    -- -   PORT_MAPPER
    --
    -- -   RIP_REFLECTION
    --
    -- -   SNMP_REFLECTION
    --
    -- -   MSSQL_REFLECTION
    --
    -- -   NET_BIOS_REFLECTION
    --
    -- -   SYN_FLOOD
    --
    -- -   ACK_FLOOD
    --
    -- -   REQUEST_FLOOD
    --
    -- -   HTTP_REFLECTION
    --
    -- -   UDS_REFLECTION
    --
    -- -   MEMCACHED_REFLECTION
    vectorType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttackVectorDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vectorType', 'attackVectorDescription_vectorType' - The attack type. Valid values:
--
-- -   UDP_TRAFFIC
--
-- -   UDP_FRAGMENT
--
-- -   GENERIC_UDP_REFLECTION
--
-- -   DNS_REFLECTION
--
-- -   NTP_REFLECTION
--
-- -   CHARGEN_REFLECTION
--
-- -   SSDP_REFLECTION
--
-- -   PORT_MAPPER
--
-- -   RIP_REFLECTION
--
-- -   SNMP_REFLECTION
--
-- -   MSSQL_REFLECTION
--
-- -   NET_BIOS_REFLECTION
--
-- -   SYN_FLOOD
--
-- -   ACK_FLOOD
--
-- -   REQUEST_FLOOD
--
-- -   HTTP_REFLECTION
--
-- -   UDS_REFLECTION
--
-- -   MEMCACHED_REFLECTION
newAttackVectorDescription ::
  -- | 'vectorType'
  Prelude.Text ->
  AttackVectorDescription
newAttackVectorDescription pVectorType_ =
  AttackVectorDescription' {vectorType = pVectorType_}

-- | The attack type. Valid values:
--
-- -   UDP_TRAFFIC
--
-- -   UDP_FRAGMENT
--
-- -   GENERIC_UDP_REFLECTION
--
-- -   DNS_REFLECTION
--
-- -   NTP_REFLECTION
--
-- -   CHARGEN_REFLECTION
--
-- -   SSDP_REFLECTION
--
-- -   PORT_MAPPER
--
-- -   RIP_REFLECTION
--
-- -   SNMP_REFLECTION
--
-- -   MSSQL_REFLECTION
--
-- -   NET_BIOS_REFLECTION
--
-- -   SYN_FLOOD
--
-- -   ACK_FLOOD
--
-- -   REQUEST_FLOOD
--
-- -   HTTP_REFLECTION
--
-- -   UDS_REFLECTION
--
-- -   MEMCACHED_REFLECTION
attackVectorDescription_vectorType :: Lens.Lens' AttackVectorDescription Prelude.Text
attackVectorDescription_vectorType = Lens.lens (\AttackVectorDescription' {vectorType} -> vectorType) (\s@AttackVectorDescription' {} a -> s {vectorType = a} :: AttackVectorDescription)

instance Core.FromJSON AttackVectorDescription where
  parseJSON =
    Core.withObject
      "AttackVectorDescription"
      ( \x ->
          AttackVectorDescription'
            Prelude.<$> (x Core..: "VectorType")
      )

instance Prelude.Hashable AttackVectorDescription where
  hashWithSalt _salt AttackVectorDescription' {..} =
    _salt `Prelude.hashWithSalt` vectorType

instance Prelude.NFData AttackVectorDescription where
  rnf AttackVectorDescription' {..} =
    Prelude.rnf vectorType
