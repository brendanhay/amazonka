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
-- Module      : Amazonka.Transfer.Types.ProtocolDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ProtocolDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The protocol settings that are configured for your server.
--
-- This type is only valid in the @UpdateServer@ API.
--
-- /See:/ 'newProtocolDetails' smart constructor.
data ProtocolDetails = ProtocolDetails'
  { -- | Indicates passive mode, for FTP and FTPS protocols. Enter a single
    -- dotted-quad IPv4 address, such as the external IP address of a firewall,
    -- router, or load balancer. For example:
    --
    -- @ aws transfer update-server --protocol-details PassiveIp=0.0.0.0 @
    --
    -- Replace @ 0.0.0.0 @ in the example above with the actual IP address you
    -- want to use.
    --
    -- If you change the @PassiveIp@ value, you must stop and then restart your
    -- Transfer server for the change to take effect. For details on using
    -- Passive IP (PASV) in a NAT environment, see
    -- <http://aws.amazon.com/blogs/storage/configuring-your-ftps-server-behind-a-firewall-or-nat-with-aws-transfer-family/ Configuring your FTPS server behind a firewall or NAT with Amazon Web Services Transfer Family>.
    passiveIp :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProtocolDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passiveIp', 'protocolDetails_passiveIp' - Indicates passive mode, for FTP and FTPS protocols. Enter a single
-- dotted-quad IPv4 address, such as the external IP address of a firewall,
-- router, or load balancer. For example:
--
-- @ aws transfer update-server --protocol-details PassiveIp=0.0.0.0 @
--
-- Replace @ 0.0.0.0 @ in the example above with the actual IP address you
-- want to use.
--
-- If you change the @PassiveIp@ value, you must stop and then restart your
-- Transfer server for the change to take effect. For details on using
-- Passive IP (PASV) in a NAT environment, see
-- <http://aws.amazon.com/blogs/storage/configuring-your-ftps-server-behind-a-firewall-or-nat-with-aws-transfer-family/ Configuring your FTPS server behind a firewall or NAT with Amazon Web Services Transfer Family>.
newProtocolDetails ::
  ProtocolDetails
newProtocolDetails =
  ProtocolDetails' {passiveIp = Prelude.Nothing}

-- | Indicates passive mode, for FTP and FTPS protocols. Enter a single
-- dotted-quad IPv4 address, such as the external IP address of a firewall,
-- router, or load balancer. For example:
--
-- @ aws transfer update-server --protocol-details PassiveIp=0.0.0.0 @
--
-- Replace @ 0.0.0.0 @ in the example above with the actual IP address you
-- want to use.
--
-- If you change the @PassiveIp@ value, you must stop and then restart your
-- Transfer server for the change to take effect. For details on using
-- Passive IP (PASV) in a NAT environment, see
-- <http://aws.amazon.com/blogs/storage/configuring-your-ftps-server-behind-a-firewall-or-nat-with-aws-transfer-family/ Configuring your FTPS server behind a firewall or NAT with Amazon Web Services Transfer Family>.
protocolDetails_passiveIp :: Lens.Lens' ProtocolDetails (Prelude.Maybe Prelude.Text)
protocolDetails_passiveIp = Lens.lens (\ProtocolDetails' {passiveIp} -> passiveIp) (\s@ProtocolDetails' {} a -> s {passiveIp = a} :: ProtocolDetails)

instance Core.FromJSON ProtocolDetails where
  parseJSON =
    Core.withObject
      "ProtocolDetails"
      ( \x ->
          ProtocolDetails'
            Prelude.<$> (x Core..:? "PassiveIp")
      )

instance Prelude.Hashable ProtocolDetails where
  hashWithSalt _salt ProtocolDetails' {..} =
    _salt `Prelude.hashWithSalt` passiveIp

instance Prelude.NFData ProtocolDetails where
  rnf ProtocolDetails' {..} = Prelude.rnf passiveIp

instance Core.ToJSON ProtocolDetails where
  toJSON ProtocolDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("PassiveIp" Core..=) Prelude.<$> passiveIp]
      )
