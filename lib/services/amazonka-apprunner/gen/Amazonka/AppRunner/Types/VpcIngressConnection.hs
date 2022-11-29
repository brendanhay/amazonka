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
-- Module      : Amazonka.AppRunner.Types.VpcIngressConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.VpcIngressConnection where

import Amazonka.AppRunner.Types.IngressVpcConfiguration
import Amazonka.AppRunner.Types.VpcIngressConnectionStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The App Runner resource that specifies an App Runner endpoint for
-- incoming traffic. It establishes a connection between a VPC interface
-- endpoint and a App Runner service, to make your App Runner service
-- accessible from only within an Amazon VPC.
--
-- /See:/ 'newVpcIngressConnection' smart constructor.
data VpcIngressConnection = VpcIngressConnection'
  { -- | The domain name associated with the VPC Ingress Connection resource.
    domainName :: Prelude.Maybe Prelude.Text,
    -- | The current status of the VPC Ingress Connection. The VPC Ingress
    -- Connection displays one of the following statuses: @AVAILABLE@,
    -- @PENDING_CREATION@, @PENDING_UPDATE@,
    -- @PENDING_DELETION@,@FAILED_CREATION@, @FAILED_UPDATE@,
    -- @FAILED_DELETION@, and @DELETED@..
    status :: Prelude.Maybe VpcIngressConnectionStatus,
    -- | The time when the App Runner service was deleted. It\'s in the Unix time
    -- stamp format.
    --
    -- -   Type: Timestamp
    --
    -- -   Required: No
    deletedAt :: Prelude.Maybe Core.POSIX,
    -- | The Account Id you use to create the VPC Ingress Connection resource.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Specifications for the customer’s VPC and related PrivateLink VPC
    -- endpoint that are used to associate with the VPC Ingress Connection
    -- resource.
    ingressVpcConfiguration :: Prelude.Maybe IngressVpcConfiguration,
    -- | The customer-provided VPC Ingress Connection name.
    vpcIngressConnectionName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the VPC Ingress Connection.
    vpcIngressConnectionArn :: Prelude.Maybe Prelude.Text,
    -- | The time when the VPC Ingress Connection was created. It\'s in the Unix
    -- time stamp format.
    --
    -- -   Type: Timestamp
    --
    -- -   Required: Yes
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the service associated with the VPC
    -- Ingress Connection.
    serviceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcIngressConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainName', 'vpcIngressConnection_domainName' - The domain name associated with the VPC Ingress Connection resource.
--
-- 'status', 'vpcIngressConnection_status' - The current status of the VPC Ingress Connection. The VPC Ingress
-- Connection displays one of the following statuses: @AVAILABLE@,
-- @PENDING_CREATION@, @PENDING_UPDATE@,
-- @PENDING_DELETION@,@FAILED_CREATION@, @FAILED_UPDATE@,
-- @FAILED_DELETION@, and @DELETED@..
--
-- 'deletedAt', 'vpcIngressConnection_deletedAt' - The time when the App Runner service was deleted. It\'s in the Unix time
-- stamp format.
--
-- -   Type: Timestamp
--
-- -   Required: No
--
-- 'accountId', 'vpcIngressConnection_accountId' - The Account Id you use to create the VPC Ingress Connection resource.
--
-- 'ingressVpcConfiguration', 'vpcIngressConnection_ingressVpcConfiguration' - Specifications for the customer’s VPC and related PrivateLink VPC
-- endpoint that are used to associate with the VPC Ingress Connection
-- resource.
--
-- 'vpcIngressConnectionName', 'vpcIngressConnection_vpcIngressConnectionName' - The customer-provided VPC Ingress Connection name.
--
-- 'vpcIngressConnectionArn', 'vpcIngressConnection_vpcIngressConnectionArn' - The Amazon Resource Name (ARN) of the VPC Ingress Connection.
--
-- 'createdAt', 'vpcIngressConnection_createdAt' - The time when the VPC Ingress Connection was created. It\'s in the Unix
-- time stamp format.
--
-- -   Type: Timestamp
--
-- -   Required: Yes
--
-- 'serviceArn', 'vpcIngressConnection_serviceArn' - The Amazon Resource Name (ARN) of the service associated with the VPC
-- Ingress Connection.
newVpcIngressConnection ::
  VpcIngressConnection
newVpcIngressConnection =
  VpcIngressConnection'
    { domainName = Prelude.Nothing,
      status = Prelude.Nothing,
      deletedAt = Prelude.Nothing,
      accountId = Prelude.Nothing,
      ingressVpcConfiguration = Prelude.Nothing,
      vpcIngressConnectionName = Prelude.Nothing,
      vpcIngressConnectionArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      serviceArn = Prelude.Nothing
    }

-- | The domain name associated with the VPC Ingress Connection resource.
vpcIngressConnection_domainName :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.Text)
vpcIngressConnection_domainName = Lens.lens (\VpcIngressConnection' {domainName} -> domainName) (\s@VpcIngressConnection' {} a -> s {domainName = a} :: VpcIngressConnection)

-- | The current status of the VPC Ingress Connection. The VPC Ingress
-- Connection displays one of the following statuses: @AVAILABLE@,
-- @PENDING_CREATION@, @PENDING_UPDATE@,
-- @PENDING_DELETION@,@FAILED_CREATION@, @FAILED_UPDATE@,
-- @FAILED_DELETION@, and @DELETED@..
vpcIngressConnection_status :: Lens.Lens' VpcIngressConnection (Prelude.Maybe VpcIngressConnectionStatus)
vpcIngressConnection_status = Lens.lens (\VpcIngressConnection' {status} -> status) (\s@VpcIngressConnection' {} a -> s {status = a} :: VpcIngressConnection)

-- | The time when the App Runner service was deleted. It\'s in the Unix time
-- stamp format.
--
-- -   Type: Timestamp
--
-- -   Required: No
vpcIngressConnection_deletedAt :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.UTCTime)
vpcIngressConnection_deletedAt = Lens.lens (\VpcIngressConnection' {deletedAt} -> deletedAt) (\s@VpcIngressConnection' {} a -> s {deletedAt = a} :: VpcIngressConnection) Prelude.. Lens.mapping Core._Time

-- | The Account Id you use to create the VPC Ingress Connection resource.
vpcIngressConnection_accountId :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.Text)
vpcIngressConnection_accountId = Lens.lens (\VpcIngressConnection' {accountId} -> accountId) (\s@VpcIngressConnection' {} a -> s {accountId = a} :: VpcIngressConnection)

-- | Specifications for the customer’s VPC and related PrivateLink VPC
-- endpoint that are used to associate with the VPC Ingress Connection
-- resource.
vpcIngressConnection_ingressVpcConfiguration :: Lens.Lens' VpcIngressConnection (Prelude.Maybe IngressVpcConfiguration)
vpcIngressConnection_ingressVpcConfiguration = Lens.lens (\VpcIngressConnection' {ingressVpcConfiguration} -> ingressVpcConfiguration) (\s@VpcIngressConnection' {} a -> s {ingressVpcConfiguration = a} :: VpcIngressConnection)

-- | The customer-provided VPC Ingress Connection name.
vpcIngressConnection_vpcIngressConnectionName :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.Text)
vpcIngressConnection_vpcIngressConnectionName = Lens.lens (\VpcIngressConnection' {vpcIngressConnectionName} -> vpcIngressConnectionName) (\s@VpcIngressConnection' {} a -> s {vpcIngressConnectionName = a} :: VpcIngressConnection)

-- | The Amazon Resource Name (ARN) of the VPC Ingress Connection.
vpcIngressConnection_vpcIngressConnectionArn :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.Text)
vpcIngressConnection_vpcIngressConnectionArn = Lens.lens (\VpcIngressConnection' {vpcIngressConnectionArn} -> vpcIngressConnectionArn) (\s@VpcIngressConnection' {} a -> s {vpcIngressConnectionArn = a} :: VpcIngressConnection)

-- | The time when the VPC Ingress Connection was created. It\'s in the Unix
-- time stamp format.
--
-- -   Type: Timestamp
--
-- -   Required: Yes
vpcIngressConnection_createdAt :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.UTCTime)
vpcIngressConnection_createdAt = Lens.lens (\VpcIngressConnection' {createdAt} -> createdAt) (\s@VpcIngressConnection' {} a -> s {createdAt = a} :: VpcIngressConnection) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the service associated with the VPC
-- Ingress Connection.
vpcIngressConnection_serviceArn :: Lens.Lens' VpcIngressConnection (Prelude.Maybe Prelude.Text)
vpcIngressConnection_serviceArn = Lens.lens (\VpcIngressConnection' {serviceArn} -> serviceArn) (\s@VpcIngressConnection' {} a -> s {serviceArn = a} :: VpcIngressConnection)

instance Core.FromJSON VpcIngressConnection where
  parseJSON =
    Core.withObject
      "VpcIngressConnection"
      ( \x ->
          VpcIngressConnection'
            Prelude.<$> (x Core..:? "DomainName")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "DeletedAt")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "IngressVpcConfiguration")
            Prelude.<*> (x Core..:? "VpcIngressConnectionName")
            Prelude.<*> (x Core..:? "VpcIngressConnectionArn")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "ServiceArn")
      )

instance Prelude.Hashable VpcIngressConnection where
  hashWithSalt _salt VpcIngressConnection' {..} =
    _salt `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` deletedAt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` ingressVpcConfiguration
      `Prelude.hashWithSalt` vpcIngressConnectionName
      `Prelude.hashWithSalt` vpcIngressConnectionArn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData VpcIngressConnection where
  rnf VpcIngressConnection' {..} =
    Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf deletedAt
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf ingressVpcConfiguration
      `Prelude.seq` Prelude.rnf vpcIngressConnectionName
      `Prelude.seq` Prelude.rnf vpcIngressConnectionArn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf serviceArn
