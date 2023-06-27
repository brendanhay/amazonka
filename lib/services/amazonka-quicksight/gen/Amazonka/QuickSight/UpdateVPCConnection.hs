{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.UpdateVPCConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a VPC connection.
module Amazonka.QuickSight.UpdateVPCConnection
  ( -- * Creating a Request
    UpdateVPCConnection (..),
    newUpdateVPCConnection,

    -- * Request Lenses
    updateVPCConnection_dnsResolvers,
    updateVPCConnection_awsAccountId,
    updateVPCConnection_vPCConnectionId,
    updateVPCConnection_name,
    updateVPCConnection_subnetIds,
    updateVPCConnection_securityGroupIds,
    updateVPCConnection_roleArn,

    -- * Destructuring the Response
    UpdateVPCConnectionResponse (..),
    newUpdateVPCConnectionResponse,

    -- * Response Lenses
    updateVPCConnectionResponse_arn,
    updateVPCConnectionResponse_availabilityStatus,
    updateVPCConnectionResponse_requestId,
    updateVPCConnectionResponse_updateStatus,
    updateVPCConnectionResponse_vPCConnectionId,
    updateVPCConnectionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateVPCConnection' smart constructor.
data UpdateVPCConnection = UpdateVPCConnection'
  { -- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
    dnsResolvers :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Web Services account ID of the account that contains the VPC
    -- connection that you want to update.
    awsAccountId :: Prelude.Text,
    -- | The ID of the VPC connection that you\'re updating. This ID is a unique
    -- identifier for each Amazon Web Services Region in an Amazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Text,
    -- | The display name for the VPC connection.
    name :: Prelude.Text,
    -- | A list of subnet IDs for the VPC connection.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | A list of security group IDs for the VPC connection.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | An IAM role associated with the VPC connection.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVPCConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsResolvers', 'updateVPCConnection_dnsResolvers' - A list of IP addresses of DNS resolver endpoints for the VPC connection.
--
-- 'awsAccountId', 'updateVPCConnection_awsAccountId' - The Amazon Web Services account ID of the account that contains the VPC
-- connection that you want to update.
--
-- 'vPCConnectionId', 'updateVPCConnection_vPCConnectionId' - The ID of the VPC connection that you\'re updating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
--
-- 'name', 'updateVPCConnection_name' - The display name for the VPC connection.
--
-- 'subnetIds', 'updateVPCConnection_subnetIds' - A list of subnet IDs for the VPC connection.
--
-- 'securityGroupIds', 'updateVPCConnection_securityGroupIds' - A list of security group IDs for the VPC connection.
--
-- 'roleArn', 'updateVPCConnection_roleArn' - An IAM role associated with the VPC connection.
newUpdateVPCConnection ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'vPCConnectionId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'subnetIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'securityGroupIds'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  UpdateVPCConnection
newUpdateVPCConnection
  pAwsAccountId_
  pVPCConnectionId_
  pName_
  pSubnetIds_
  pSecurityGroupIds_
  pRoleArn_ =
    UpdateVPCConnection'
      { dnsResolvers =
          Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        vPCConnectionId = pVPCConnectionId_,
        name = pName_,
        subnetIds = Lens.coerced Lens.# pSubnetIds_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_,
        roleArn = pRoleArn_
      }

-- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
updateVPCConnection_dnsResolvers :: Lens.Lens' UpdateVPCConnection (Prelude.Maybe [Prelude.Text])
updateVPCConnection_dnsResolvers = Lens.lens (\UpdateVPCConnection' {dnsResolvers} -> dnsResolvers) (\s@UpdateVPCConnection' {} a -> s {dnsResolvers = a} :: UpdateVPCConnection) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID of the account that contains the VPC
-- connection that you want to update.
updateVPCConnection_awsAccountId :: Lens.Lens' UpdateVPCConnection Prelude.Text
updateVPCConnection_awsAccountId = Lens.lens (\UpdateVPCConnection' {awsAccountId} -> awsAccountId) (\s@UpdateVPCConnection' {} a -> s {awsAccountId = a} :: UpdateVPCConnection)

-- | The ID of the VPC connection that you\'re updating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
updateVPCConnection_vPCConnectionId :: Lens.Lens' UpdateVPCConnection Prelude.Text
updateVPCConnection_vPCConnectionId = Lens.lens (\UpdateVPCConnection' {vPCConnectionId} -> vPCConnectionId) (\s@UpdateVPCConnection' {} a -> s {vPCConnectionId = a} :: UpdateVPCConnection)

-- | The display name for the VPC connection.
updateVPCConnection_name :: Lens.Lens' UpdateVPCConnection Prelude.Text
updateVPCConnection_name = Lens.lens (\UpdateVPCConnection' {name} -> name) (\s@UpdateVPCConnection' {} a -> s {name = a} :: UpdateVPCConnection)

-- | A list of subnet IDs for the VPC connection.
updateVPCConnection_subnetIds :: Lens.Lens' UpdateVPCConnection (Prelude.NonEmpty Prelude.Text)
updateVPCConnection_subnetIds = Lens.lens (\UpdateVPCConnection' {subnetIds} -> subnetIds) (\s@UpdateVPCConnection' {} a -> s {subnetIds = a} :: UpdateVPCConnection) Prelude.. Lens.coerced

-- | A list of security group IDs for the VPC connection.
updateVPCConnection_securityGroupIds :: Lens.Lens' UpdateVPCConnection (Prelude.NonEmpty Prelude.Text)
updateVPCConnection_securityGroupIds = Lens.lens (\UpdateVPCConnection' {securityGroupIds} -> securityGroupIds) (\s@UpdateVPCConnection' {} a -> s {securityGroupIds = a} :: UpdateVPCConnection) Prelude.. Lens.coerced

-- | An IAM role associated with the VPC connection.
updateVPCConnection_roleArn :: Lens.Lens' UpdateVPCConnection Prelude.Text
updateVPCConnection_roleArn = Lens.lens (\UpdateVPCConnection' {roleArn} -> roleArn) (\s@UpdateVPCConnection' {} a -> s {roleArn = a} :: UpdateVPCConnection)

instance Core.AWSRequest UpdateVPCConnection where
  type
    AWSResponse UpdateVPCConnection =
      UpdateVPCConnectionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateVPCConnectionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AvailabilityStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "UpdateStatus")
            Prelude.<*> (x Data..?> "VPCConnectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateVPCConnection where
  hashWithSalt _salt UpdateVPCConnection' {..} =
    _salt
      `Prelude.hashWithSalt` dnsResolvers
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` vPCConnectionId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData UpdateVPCConnection where
  rnf UpdateVPCConnection' {..} =
    Prelude.rnf dnsResolvers
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf vPCConnectionId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders UpdateVPCConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateVPCConnection where
  toJSON UpdateVPCConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DnsResolvers" Data..=) Prelude.<$> dnsResolvers,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath UpdateVPCConnection where
  toPath UpdateVPCConnection' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/vpc-connections/",
        Data.toBS vPCConnectionId
      ]

instance Data.ToQuery UpdateVPCConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateVPCConnectionResponse' smart constructor.
data UpdateVPCConnectionResponse = UpdateVPCConnectionResponse'
  { -- | The Amazon Resource Name (ARN) of the VPC connection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The availability status of the VPC connection.
    availabilityStatus :: Prelude.Maybe VPCConnectionAvailabilityStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The update status of the VPC connection\'s last update.
    updateStatus :: Prelude.Maybe VPCConnectionResourceStatus,
    -- | The ID of the VPC connection that you are updating. This ID is a unique
    -- identifier for each Amazon Web Services Region in anAmazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateVPCConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateVPCConnectionResponse_arn' - The Amazon Resource Name (ARN) of the VPC connection.
--
-- 'availabilityStatus', 'updateVPCConnectionResponse_availabilityStatus' - The availability status of the VPC connection.
--
-- 'requestId', 'updateVPCConnectionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'updateStatus', 'updateVPCConnectionResponse_updateStatus' - The update status of the VPC connection\'s last update.
--
-- 'vPCConnectionId', 'updateVPCConnectionResponse_vPCConnectionId' - The ID of the VPC connection that you are updating. This ID is a unique
-- identifier for each Amazon Web Services Region in anAmazon Web Services
-- account.
--
-- 'status', 'updateVPCConnectionResponse_status' - The HTTP status of the request.
newUpdateVPCConnectionResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateVPCConnectionResponse
newUpdateVPCConnectionResponse pStatus_ =
  UpdateVPCConnectionResponse'
    { arn = Prelude.Nothing,
      availabilityStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      vPCConnectionId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the VPC connection.
updateVPCConnectionResponse_arn :: Lens.Lens' UpdateVPCConnectionResponse (Prelude.Maybe Prelude.Text)
updateVPCConnectionResponse_arn = Lens.lens (\UpdateVPCConnectionResponse' {arn} -> arn) (\s@UpdateVPCConnectionResponse' {} a -> s {arn = a} :: UpdateVPCConnectionResponse)

-- | The availability status of the VPC connection.
updateVPCConnectionResponse_availabilityStatus :: Lens.Lens' UpdateVPCConnectionResponse (Prelude.Maybe VPCConnectionAvailabilityStatus)
updateVPCConnectionResponse_availabilityStatus = Lens.lens (\UpdateVPCConnectionResponse' {availabilityStatus} -> availabilityStatus) (\s@UpdateVPCConnectionResponse' {} a -> s {availabilityStatus = a} :: UpdateVPCConnectionResponse)

-- | The Amazon Web Services request ID for this operation.
updateVPCConnectionResponse_requestId :: Lens.Lens' UpdateVPCConnectionResponse (Prelude.Maybe Prelude.Text)
updateVPCConnectionResponse_requestId = Lens.lens (\UpdateVPCConnectionResponse' {requestId} -> requestId) (\s@UpdateVPCConnectionResponse' {} a -> s {requestId = a} :: UpdateVPCConnectionResponse)

-- | The update status of the VPC connection\'s last update.
updateVPCConnectionResponse_updateStatus :: Lens.Lens' UpdateVPCConnectionResponse (Prelude.Maybe VPCConnectionResourceStatus)
updateVPCConnectionResponse_updateStatus = Lens.lens (\UpdateVPCConnectionResponse' {updateStatus} -> updateStatus) (\s@UpdateVPCConnectionResponse' {} a -> s {updateStatus = a} :: UpdateVPCConnectionResponse)

-- | The ID of the VPC connection that you are updating. This ID is a unique
-- identifier for each Amazon Web Services Region in anAmazon Web Services
-- account.
updateVPCConnectionResponse_vPCConnectionId :: Lens.Lens' UpdateVPCConnectionResponse (Prelude.Maybe Prelude.Text)
updateVPCConnectionResponse_vPCConnectionId = Lens.lens (\UpdateVPCConnectionResponse' {vPCConnectionId} -> vPCConnectionId) (\s@UpdateVPCConnectionResponse' {} a -> s {vPCConnectionId = a} :: UpdateVPCConnectionResponse)

-- | The HTTP status of the request.
updateVPCConnectionResponse_status :: Lens.Lens' UpdateVPCConnectionResponse Prelude.Int
updateVPCConnectionResponse_status = Lens.lens (\UpdateVPCConnectionResponse' {status} -> status) (\s@UpdateVPCConnectionResponse' {} a -> s {status = a} :: UpdateVPCConnectionResponse)

instance Prelude.NFData UpdateVPCConnectionResponse where
  rnf UpdateVPCConnectionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf vPCConnectionId
      `Prelude.seq` Prelude.rnf status
