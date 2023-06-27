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
-- Module      : Amazonka.QuickSight.CreateVPCConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new VPC connection.
module Amazonka.QuickSight.CreateVPCConnection
  ( -- * Creating a Request
    CreateVPCConnection (..),
    newCreateVPCConnection,

    -- * Request Lenses
    createVPCConnection_dnsResolvers,
    createVPCConnection_tags,
    createVPCConnection_awsAccountId,
    createVPCConnection_vPCConnectionId,
    createVPCConnection_name,
    createVPCConnection_subnetIds,
    createVPCConnection_securityGroupIds,
    createVPCConnection_roleArn,

    -- * Destructuring the Response
    CreateVPCConnectionResponse (..),
    newCreateVPCConnectionResponse,

    -- * Response Lenses
    createVPCConnectionResponse_arn,
    createVPCConnectionResponse_availabilityStatus,
    createVPCConnectionResponse_creationStatus,
    createVPCConnectionResponse_requestId,
    createVPCConnectionResponse_vPCConnectionId,
    createVPCConnectionResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVPCConnection' smart constructor.
data CreateVPCConnection = CreateVPCConnection'
  { -- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
    dnsResolvers :: Prelude.Maybe [Prelude.Text],
    -- | A map of the key-value pairs for the resource tag or tags assigned to
    -- the VPC connection.
    tags :: Prelude.Maybe (Prelude.NonEmpty Tag),
    -- | The Amazon Web Services account ID of the account where you want to
    -- create a new VPC connection.
    awsAccountId :: Prelude.Text,
    -- | The ID of the VPC connection that you\'re creating. This ID is a unique
    -- identifier for each Amazon Web Services Region in an Amazon Web Services
    -- account.
    vPCConnectionId :: Prelude.Text,
    -- | The display name for the VPC connection.
    name :: Prelude.Text,
    -- | A list of subnet IDs for the VPC connection.
    subnetIds :: Prelude.NonEmpty Prelude.Text,
    -- | A list of security group IDs for the VPC connection.
    securityGroupIds :: Prelude.NonEmpty Prelude.Text,
    -- | The IAM role to associate with the VPC connection.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVPCConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dnsResolvers', 'createVPCConnection_dnsResolvers' - A list of IP addresses of DNS resolver endpoints for the VPC connection.
--
-- 'tags', 'createVPCConnection_tags' - A map of the key-value pairs for the resource tag or tags assigned to
-- the VPC connection.
--
-- 'awsAccountId', 'createVPCConnection_awsAccountId' - The Amazon Web Services account ID of the account where you want to
-- create a new VPC connection.
--
-- 'vPCConnectionId', 'createVPCConnection_vPCConnectionId' - The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
--
-- 'name', 'createVPCConnection_name' - The display name for the VPC connection.
--
-- 'subnetIds', 'createVPCConnection_subnetIds' - A list of subnet IDs for the VPC connection.
--
-- 'securityGroupIds', 'createVPCConnection_securityGroupIds' - A list of security group IDs for the VPC connection.
--
-- 'roleArn', 'createVPCConnection_roleArn' - The IAM role to associate with the VPC connection.
newCreateVPCConnection ::
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
  CreateVPCConnection
newCreateVPCConnection
  pAwsAccountId_
  pVPCConnectionId_
  pName_
  pSubnetIds_
  pSecurityGroupIds_
  pRoleArn_ =
    CreateVPCConnection'
      { dnsResolvers =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        vPCConnectionId = pVPCConnectionId_,
        name = pName_,
        subnetIds = Lens.coerced Lens.# pSubnetIds_,
        securityGroupIds =
          Lens.coerced Lens.# pSecurityGroupIds_,
        roleArn = pRoleArn_
      }

-- | A list of IP addresses of DNS resolver endpoints for the VPC connection.
createVPCConnection_dnsResolvers :: Lens.Lens' CreateVPCConnection (Prelude.Maybe [Prelude.Text])
createVPCConnection_dnsResolvers = Lens.lens (\CreateVPCConnection' {dnsResolvers} -> dnsResolvers) (\s@CreateVPCConnection' {} a -> s {dnsResolvers = a} :: CreateVPCConnection) Prelude.. Lens.mapping Lens.coerced

-- | A map of the key-value pairs for the resource tag or tags assigned to
-- the VPC connection.
createVPCConnection_tags :: Lens.Lens' CreateVPCConnection (Prelude.Maybe (Prelude.NonEmpty Tag))
createVPCConnection_tags = Lens.lens (\CreateVPCConnection' {tags} -> tags) (\s@CreateVPCConnection' {} a -> s {tags = a} :: CreateVPCConnection) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID of the account where you want to
-- create a new VPC connection.
createVPCConnection_awsAccountId :: Lens.Lens' CreateVPCConnection Prelude.Text
createVPCConnection_awsAccountId = Lens.lens (\CreateVPCConnection' {awsAccountId} -> awsAccountId) (\s@CreateVPCConnection' {} a -> s {awsAccountId = a} :: CreateVPCConnection)

-- | The ID of the VPC connection that you\'re creating. This ID is a unique
-- identifier for each Amazon Web Services Region in an Amazon Web Services
-- account.
createVPCConnection_vPCConnectionId :: Lens.Lens' CreateVPCConnection Prelude.Text
createVPCConnection_vPCConnectionId = Lens.lens (\CreateVPCConnection' {vPCConnectionId} -> vPCConnectionId) (\s@CreateVPCConnection' {} a -> s {vPCConnectionId = a} :: CreateVPCConnection)

-- | The display name for the VPC connection.
createVPCConnection_name :: Lens.Lens' CreateVPCConnection Prelude.Text
createVPCConnection_name = Lens.lens (\CreateVPCConnection' {name} -> name) (\s@CreateVPCConnection' {} a -> s {name = a} :: CreateVPCConnection)

-- | A list of subnet IDs for the VPC connection.
createVPCConnection_subnetIds :: Lens.Lens' CreateVPCConnection (Prelude.NonEmpty Prelude.Text)
createVPCConnection_subnetIds = Lens.lens (\CreateVPCConnection' {subnetIds} -> subnetIds) (\s@CreateVPCConnection' {} a -> s {subnetIds = a} :: CreateVPCConnection) Prelude.. Lens.coerced

-- | A list of security group IDs for the VPC connection.
createVPCConnection_securityGroupIds :: Lens.Lens' CreateVPCConnection (Prelude.NonEmpty Prelude.Text)
createVPCConnection_securityGroupIds = Lens.lens (\CreateVPCConnection' {securityGroupIds} -> securityGroupIds) (\s@CreateVPCConnection' {} a -> s {securityGroupIds = a} :: CreateVPCConnection) Prelude.. Lens.coerced

-- | The IAM role to associate with the VPC connection.
createVPCConnection_roleArn :: Lens.Lens' CreateVPCConnection Prelude.Text
createVPCConnection_roleArn = Lens.lens (\CreateVPCConnection' {roleArn} -> roleArn) (\s@CreateVPCConnection' {} a -> s {roleArn = a} :: CreateVPCConnection)

instance Core.AWSRequest CreateVPCConnection where
  type
    AWSResponse CreateVPCConnection =
      CreateVPCConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateVPCConnectionResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "AvailabilityStatus")
            Prelude.<*> (x Data..?> "CreationStatus")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "VPCConnectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateVPCConnection where
  hashWithSalt _salt CreateVPCConnection' {..} =
    _salt
      `Prelude.hashWithSalt` dnsResolvers
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` vPCConnectionId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` subnetIds
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData CreateVPCConnection where
  rnf CreateVPCConnection' {..} =
    Prelude.rnf dnsResolvers
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf vPCConnectionId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf subnetIds
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders CreateVPCConnection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateVPCConnection where
  toJSON CreateVPCConnection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DnsResolvers" Data..=) Prelude.<$> dnsResolvers,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("VPCConnectionId" Data..= vPCConnectionId),
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("SubnetIds" Data..= subnetIds),
            Prelude.Just
              ("SecurityGroupIds" Data..= securityGroupIds),
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath CreateVPCConnection where
  toPath CreateVPCConnection' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/vpc-connections"
      ]

instance Data.ToQuery CreateVPCConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateVPCConnectionResponse' smart constructor.
data CreateVPCConnectionResponse = CreateVPCConnectionResponse'
  { -- | The Amazon Resource Name (ARN) of the VPC connection.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The availability status of the VPC connection.
    availabilityStatus :: Prelude.Maybe VPCConnectionAvailabilityStatus,
    -- | The status of the creation of the VPC connection.
    creationStatus :: Prelude.Maybe VPCConnectionResourceStatus,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The ID for the VPC connection that you\'re creating. This ID is unique
    -- per Amazon Web Services Region for each Amazon Web Services account.
    vPCConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVPCConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createVPCConnectionResponse_arn' - The Amazon Resource Name (ARN) of the VPC connection.
--
-- 'availabilityStatus', 'createVPCConnectionResponse_availabilityStatus' - The availability status of the VPC connection.
--
-- 'creationStatus', 'createVPCConnectionResponse_creationStatus' - The status of the creation of the VPC connection.
--
-- 'requestId', 'createVPCConnectionResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'vPCConnectionId', 'createVPCConnectionResponse_vPCConnectionId' - The ID for the VPC connection that you\'re creating. This ID is unique
-- per Amazon Web Services Region for each Amazon Web Services account.
--
-- 'status', 'createVPCConnectionResponse_status' - The HTTP status of the request.
newCreateVPCConnectionResponse ::
  -- | 'status'
  Prelude.Int ->
  CreateVPCConnectionResponse
newCreateVPCConnectionResponse pStatus_ =
  CreateVPCConnectionResponse'
    { arn = Prelude.Nothing,
      availabilityStatus = Prelude.Nothing,
      creationStatus = Prelude.Nothing,
      requestId = Prelude.Nothing,
      vPCConnectionId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the VPC connection.
createVPCConnectionResponse_arn :: Lens.Lens' CreateVPCConnectionResponse (Prelude.Maybe Prelude.Text)
createVPCConnectionResponse_arn = Lens.lens (\CreateVPCConnectionResponse' {arn} -> arn) (\s@CreateVPCConnectionResponse' {} a -> s {arn = a} :: CreateVPCConnectionResponse)

-- | The availability status of the VPC connection.
createVPCConnectionResponse_availabilityStatus :: Lens.Lens' CreateVPCConnectionResponse (Prelude.Maybe VPCConnectionAvailabilityStatus)
createVPCConnectionResponse_availabilityStatus = Lens.lens (\CreateVPCConnectionResponse' {availabilityStatus} -> availabilityStatus) (\s@CreateVPCConnectionResponse' {} a -> s {availabilityStatus = a} :: CreateVPCConnectionResponse)

-- | The status of the creation of the VPC connection.
createVPCConnectionResponse_creationStatus :: Lens.Lens' CreateVPCConnectionResponse (Prelude.Maybe VPCConnectionResourceStatus)
createVPCConnectionResponse_creationStatus = Lens.lens (\CreateVPCConnectionResponse' {creationStatus} -> creationStatus) (\s@CreateVPCConnectionResponse' {} a -> s {creationStatus = a} :: CreateVPCConnectionResponse)

-- | The Amazon Web Services request ID for this operation.
createVPCConnectionResponse_requestId :: Lens.Lens' CreateVPCConnectionResponse (Prelude.Maybe Prelude.Text)
createVPCConnectionResponse_requestId = Lens.lens (\CreateVPCConnectionResponse' {requestId} -> requestId) (\s@CreateVPCConnectionResponse' {} a -> s {requestId = a} :: CreateVPCConnectionResponse)

-- | The ID for the VPC connection that you\'re creating. This ID is unique
-- per Amazon Web Services Region for each Amazon Web Services account.
createVPCConnectionResponse_vPCConnectionId :: Lens.Lens' CreateVPCConnectionResponse (Prelude.Maybe Prelude.Text)
createVPCConnectionResponse_vPCConnectionId = Lens.lens (\CreateVPCConnectionResponse' {vPCConnectionId} -> vPCConnectionId) (\s@CreateVPCConnectionResponse' {} a -> s {vPCConnectionId = a} :: CreateVPCConnectionResponse)

-- | The HTTP status of the request.
createVPCConnectionResponse_status :: Lens.Lens' CreateVPCConnectionResponse Prelude.Int
createVPCConnectionResponse_status = Lens.lens (\CreateVPCConnectionResponse' {status} -> status) (\s@CreateVPCConnectionResponse' {} a -> s {status = a} :: CreateVPCConnectionResponse)

instance Prelude.NFData CreateVPCConnectionResponse where
  rnf CreateVPCConnectionResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf availabilityStatus
      `Prelude.seq` Prelude.rnf creationStatus
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf vPCConnectionId
      `Prelude.seq` Prelude.rnf status
