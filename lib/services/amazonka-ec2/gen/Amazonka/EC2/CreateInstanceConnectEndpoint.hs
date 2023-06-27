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
-- Module      : Amazonka.EC2.CreateInstanceConnectEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an EC2 Instance Connect Endpoint.
--
-- An EC2 Instance Connect Endpoint allows you to connect to a resource,
-- without requiring the resource to have a public IPv4 address. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Connect-using-EC2-Instance-Connect-Endpoint.html Connect to your resources without requiring a public IPv4 address using EC2 Instance Connect Endpoint>
-- in the /Amazon EC2 User Guide/.
module Amazonka.EC2.CreateInstanceConnectEndpoint
  ( -- * Creating a Request
    CreateInstanceConnectEndpoint (..),
    newCreateInstanceConnectEndpoint,

    -- * Request Lenses
    createInstanceConnectEndpoint_clientToken,
    createInstanceConnectEndpoint_dryRun,
    createInstanceConnectEndpoint_preserveClientIp,
    createInstanceConnectEndpoint_securityGroupIds,
    createInstanceConnectEndpoint_tagSpecifications,
    createInstanceConnectEndpoint_subnetId,

    -- * Destructuring the Response
    CreateInstanceConnectEndpointResponse (..),
    newCreateInstanceConnectEndpointResponse,

    -- * Response Lenses
    createInstanceConnectEndpointResponse_clientToken,
    createInstanceConnectEndpointResponse_instanceConnectEndpoint,
    createInstanceConnectEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateInstanceConnectEndpoint' smart constructor.
data CreateInstanceConnectEndpoint = CreateInstanceConnectEndpoint'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether your client\'s IP address is preserved as the source.
    -- The value is @true@ or @false@.
    --
    -- -   If @true@, your client\'s IP address is used when you connect to a
    --     resource.
    --
    -- -   If @false@, the elastic network interface IP address is used when
    --     you connect to a resource.
    --
    -- Default: @true@
    preserveClientIp :: Prelude.Maybe Prelude.Bool,
    -- | One or more security groups to associate with the endpoint. If you
    -- don\'t specify a security group, the default security group for your VPC
    -- will be associated with the endpoint.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | The tags to apply to the EC2 Instance Connect Endpoint during creation.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | The ID of the subnet in which to create the EC2 Instance Connect
    -- Endpoint.
    subnetId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceConnectEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createInstanceConnectEndpoint_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'dryRun', 'createInstanceConnectEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'preserveClientIp', 'createInstanceConnectEndpoint_preserveClientIp' - Indicates whether your client\'s IP address is preserved as the source.
-- The value is @true@ or @false@.
--
-- -   If @true@, your client\'s IP address is used when you connect to a
--     resource.
--
-- -   If @false@, the elastic network interface IP address is used when
--     you connect to a resource.
--
-- Default: @true@
--
-- 'securityGroupIds', 'createInstanceConnectEndpoint_securityGroupIds' - One or more security groups to associate with the endpoint. If you
-- don\'t specify a security group, the default security group for your VPC
-- will be associated with the endpoint.
--
-- 'tagSpecifications', 'createInstanceConnectEndpoint_tagSpecifications' - The tags to apply to the EC2 Instance Connect Endpoint during creation.
--
-- 'subnetId', 'createInstanceConnectEndpoint_subnetId' - The ID of the subnet in which to create the EC2 Instance Connect
-- Endpoint.
newCreateInstanceConnectEndpoint ::
  -- | 'subnetId'
  Prelude.Text ->
  CreateInstanceConnectEndpoint
newCreateInstanceConnectEndpoint pSubnetId_ =
  CreateInstanceConnectEndpoint'
    { clientToken =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      preserveClientIp = Prelude.Nothing,
      securityGroupIds = Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      subnetId = pSubnetId_
    }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createInstanceConnectEndpoint_clientToken :: Lens.Lens' CreateInstanceConnectEndpoint (Prelude.Maybe Prelude.Text)
createInstanceConnectEndpoint_clientToken = Lens.lens (\CreateInstanceConnectEndpoint' {clientToken} -> clientToken) (\s@CreateInstanceConnectEndpoint' {} a -> s {clientToken = a} :: CreateInstanceConnectEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createInstanceConnectEndpoint_dryRun :: Lens.Lens' CreateInstanceConnectEndpoint (Prelude.Maybe Prelude.Bool)
createInstanceConnectEndpoint_dryRun = Lens.lens (\CreateInstanceConnectEndpoint' {dryRun} -> dryRun) (\s@CreateInstanceConnectEndpoint' {} a -> s {dryRun = a} :: CreateInstanceConnectEndpoint)

-- | Indicates whether your client\'s IP address is preserved as the source.
-- The value is @true@ or @false@.
--
-- -   If @true@, your client\'s IP address is used when you connect to a
--     resource.
--
-- -   If @false@, the elastic network interface IP address is used when
--     you connect to a resource.
--
-- Default: @true@
createInstanceConnectEndpoint_preserveClientIp :: Lens.Lens' CreateInstanceConnectEndpoint (Prelude.Maybe Prelude.Bool)
createInstanceConnectEndpoint_preserveClientIp = Lens.lens (\CreateInstanceConnectEndpoint' {preserveClientIp} -> preserveClientIp) (\s@CreateInstanceConnectEndpoint' {} a -> s {preserveClientIp = a} :: CreateInstanceConnectEndpoint)

-- | One or more security groups to associate with the endpoint. If you
-- don\'t specify a security group, the default security group for your VPC
-- will be associated with the endpoint.
createInstanceConnectEndpoint_securityGroupIds :: Lens.Lens' CreateInstanceConnectEndpoint (Prelude.Maybe [Prelude.Text])
createInstanceConnectEndpoint_securityGroupIds = Lens.lens (\CreateInstanceConnectEndpoint' {securityGroupIds} -> securityGroupIds) (\s@CreateInstanceConnectEndpoint' {} a -> s {securityGroupIds = a} :: CreateInstanceConnectEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The tags to apply to the EC2 Instance Connect Endpoint during creation.
createInstanceConnectEndpoint_tagSpecifications :: Lens.Lens' CreateInstanceConnectEndpoint (Prelude.Maybe [TagSpecification])
createInstanceConnectEndpoint_tagSpecifications = Lens.lens (\CreateInstanceConnectEndpoint' {tagSpecifications} -> tagSpecifications) (\s@CreateInstanceConnectEndpoint' {} a -> s {tagSpecifications = a} :: CreateInstanceConnectEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the subnet in which to create the EC2 Instance Connect
-- Endpoint.
createInstanceConnectEndpoint_subnetId :: Lens.Lens' CreateInstanceConnectEndpoint Prelude.Text
createInstanceConnectEndpoint_subnetId = Lens.lens (\CreateInstanceConnectEndpoint' {subnetId} -> subnetId) (\s@CreateInstanceConnectEndpoint' {} a -> s {subnetId = a} :: CreateInstanceConnectEndpoint)

instance
  Core.AWSRequest
    CreateInstanceConnectEndpoint
  where
  type
    AWSResponse CreateInstanceConnectEndpoint =
      CreateInstanceConnectEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInstanceConnectEndpointResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> (x Data..@? "instanceConnectEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateInstanceConnectEndpoint
  where
  hashWithSalt _salt CreateInstanceConnectEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` preserveClientIp
      `Prelude.hashWithSalt` securityGroupIds
      `Prelude.hashWithSalt` tagSpecifications
      `Prelude.hashWithSalt` subnetId

instance Prelude.NFData CreateInstanceConnectEndpoint where
  rnf CreateInstanceConnectEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf preserveClientIp
      `Prelude.seq` Prelude.rnf securityGroupIds
      `Prelude.seq` Prelude.rnf tagSpecifications
      `Prelude.seq` Prelude.rnf subnetId

instance Data.ToHeaders CreateInstanceConnectEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateInstanceConnectEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateInstanceConnectEndpoint where
  toQuery CreateInstanceConnectEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateInstanceConnectEndpoint" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        "PreserveClientIp" Data.=: preserveClientIp,
        Data.toQuery
          ( Data.toQueryList "SecurityGroupId"
              Prelude.<$> securityGroupIds
          ),
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "SubnetId" Data.=: subnetId
      ]

-- | /See:/ 'newCreateInstanceConnectEndpointResponse' smart constructor.
data CreateInstanceConnectEndpointResponse = CreateInstanceConnectEndpointResponse'
  { -- | Unique, case-sensitive idempotency token provided by the client in the
    -- the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the EC2 Instance Connect Endpoint.
    instanceConnectEndpoint :: Prelude.Maybe Ec2InstanceConnectEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInstanceConnectEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createInstanceConnectEndpointResponse_clientToken' - Unique, case-sensitive idempotency token provided by the client in the
-- the request.
--
-- 'instanceConnectEndpoint', 'createInstanceConnectEndpointResponse_instanceConnectEndpoint' - Information about the EC2 Instance Connect Endpoint.
--
-- 'httpStatus', 'createInstanceConnectEndpointResponse_httpStatus' - The response's http status code.
newCreateInstanceConnectEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateInstanceConnectEndpointResponse
newCreateInstanceConnectEndpointResponse pHttpStatus_ =
  CreateInstanceConnectEndpointResponse'
    { clientToken =
        Prelude.Nothing,
      instanceConnectEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Unique, case-sensitive idempotency token provided by the client in the
-- the request.
createInstanceConnectEndpointResponse_clientToken :: Lens.Lens' CreateInstanceConnectEndpointResponse (Prelude.Maybe Prelude.Text)
createInstanceConnectEndpointResponse_clientToken = Lens.lens (\CreateInstanceConnectEndpointResponse' {clientToken} -> clientToken) (\s@CreateInstanceConnectEndpointResponse' {} a -> s {clientToken = a} :: CreateInstanceConnectEndpointResponse)

-- | Information about the EC2 Instance Connect Endpoint.
createInstanceConnectEndpointResponse_instanceConnectEndpoint :: Lens.Lens' CreateInstanceConnectEndpointResponse (Prelude.Maybe Ec2InstanceConnectEndpoint)
createInstanceConnectEndpointResponse_instanceConnectEndpoint = Lens.lens (\CreateInstanceConnectEndpointResponse' {instanceConnectEndpoint} -> instanceConnectEndpoint) (\s@CreateInstanceConnectEndpointResponse' {} a -> s {instanceConnectEndpoint = a} :: CreateInstanceConnectEndpointResponse)

-- | The response's http status code.
createInstanceConnectEndpointResponse_httpStatus :: Lens.Lens' CreateInstanceConnectEndpointResponse Prelude.Int
createInstanceConnectEndpointResponse_httpStatus = Lens.lens (\CreateInstanceConnectEndpointResponse' {httpStatus} -> httpStatus) (\s@CreateInstanceConnectEndpointResponse' {} a -> s {httpStatus = a} :: CreateInstanceConnectEndpointResponse)

instance
  Prelude.NFData
    CreateInstanceConnectEndpointResponse
  where
  rnf CreateInstanceConnectEndpointResponse' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf instanceConnectEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
