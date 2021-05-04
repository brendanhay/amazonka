{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.CreateVpcEndpointServiceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint service configuration to which service consumers
-- (AWS accounts, IAM users, and IAM roles) can connect.
--
-- To create an endpoint service configuration, you must first create one
-- of the following for your service:
--
-- -   A
--     <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/introduction.html Network Load Balancer>.
--     Service consumers connect to your service using an interface
--     endpoint.
--
-- -   A
--     <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/introduction.html Gateway Load Balancer>.
--     Service consumers connect to your service using a Gateway Load
--     Balancer endpoint.
--
-- For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-service.html VPC Endpoint Services>
-- in the /Amazon Virtual Private Cloud User Guide/.
--
-- If you set the private DNS name, you must prove that you own the private
-- DNS domain name. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.CreateVpcEndpointServiceConfiguration
  ( -- * Creating a Request
    CreateVpcEndpointServiceConfiguration (..),
    newCreateVpcEndpointServiceConfiguration,

    -- * Request Lenses
    createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns,
    createVpcEndpointServiceConfiguration_tagSpecifications,
    createVpcEndpointServiceConfiguration_dryRun,
    createVpcEndpointServiceConfiguration_privateDnsName,
    createVpcEndpointServiceConfiguration_acceptanceRequired,
    createVpcEndpointServiceConfiguration_networkLoadBalancerArns,
    createVpcEndpointServiceConfiguration_clientToken,

    -- * Destructuring the Response
    CreateVpcEndpointServiceConfigurationResponse (..),
    newCreateVpcEndpointServiceConfigurationResponse,

    -- * Response Lenses
    createVpcEndpointServiceConfigurationResponse_serviceConfiguration,
    createVpcEndpointServiceConfigurationResponse_clientToken,
    createVpcEndpointServiceConfigurationResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateVpcEndpointServiceConfiguration' smart constructor.
data CreateVpcEndpointServiceConfiguration = CreateVpcEndpointServiceConfiguration'
  { -- | The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The tags to associate with the service.
    tagSpecifications :: Prelude.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | (Interface endpoint configuration) The private DNS name to assign to the
    -- VPC endpoint service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether requests from service consumers to create an endpoint
    -- to your service must be accepted. To accept a request, use
    -- AcceptVpcEndpointConnections.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of one or more Network Load Balancers
    -- for your service.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayLoadBalancerArns', 'createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
--
-- 'tagSpecifications', 'createVpcEndpointServiceConfiguration_tagSpecifications' - The tags to associate with the service.
--
-- 'dryRun', 'createVpcEndpointServiceConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'privateDnsName', 'createVpcEndpointServiceConfiguration_privateDnsName' - (Interface endpoint configuration) The private DNS name to assign to the
-- VPC endpoint service.
--
-- 'acceptanceRequired', 'createVpcEndpointServiceConfiguration_acceptanceRequired' - Indicates whether requests from service consumers to create an endpoint
-- to your service must be accepted. To accept a request, use
-- AcceptVpcEndpointConnections.
--
-- 'networkLoadBalancerArns', 'createVpcEndpointServiceConfiguration_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of one or more Network Load Balancers
-- for your service.
--
-- 'clientToken', 'createVpcEndpointServiceConfiguration_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
newCreateVpcEndpointServiceConfiguration ::
  CreateVpcEndpointServiceConfiguration
newCreateVpcEndpointServiceConfiguration =
  CreateVpcEndpointServiceConfiguration'
    { gatewayLoadBalancerArns =
        Prelude.Nothing,
      tagSpecifications = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      networkLoadBalancerArns =
        Prelude.Nothing,
      clientToken = Prelude.Nothing
    }

-- | The Amazon Resource Names (ARNs) of one or more Gateway Load Balancers.
createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns = Lens.lens (\CreateVpcEndpointServiceConfiguration' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {gatewayLoadBalancerArns = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The tags to associate with the service.
createVpcEndpointServiceConfiguration_tagSpecifications :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [TagSpecification])
createVpcEndpointServiceConfiguration_tagSpecifications = Lens.lens (\CreateVpcEndpointServiceConfiguration' {tagSpecifications} -> tagSpecifications) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {tagSpecifications = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpcEndpointServiceConfiguration_dryRun :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
createVpcEndpointServiceConfiguration_dryRun = Lens.lens (\CreateVpcEndpointServiceConfiguration' {dryRun} -> dryRun) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {dryRun = a} :: CreateVpcEndpointServiceConfiguration)

-- | (Interface endpoint configuration) The private DNS name to assign to the
-- VPC endpoint service.
createVpcEndpointServiceConfiguration_privateDnsName :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Text)
createVpcEndpointServiceConfiguration_privateDnsName = Lens.lens (\CreateVpcEndpointServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {privateDnsName = a} :: CreateVpcEndpointServiceConfiguration)

-- | Indicates whether requests from service consumers to create an endpoint
-- to your service must be accepted. To accept a request, use
-- AcceptVpcEndpointConnections.
createVpcEndpointServiceConfiguration_acceptanceRequired :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
createVpcEndpointServiceConfiguration_acceptanceRequired = Lens.lens (\CreateVpcEndpointServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {acceptanceRequired = a} :: CreateVpcEndpointServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of one or more Network Load Balancers
-- for your service.
createVpcEndpointServiceConfiguration_networkLoadBalancerArns :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
createVpcEndpointServiceConfiguration_networkLoadBalancerArns = Lens.lens (\CreateVpcEndpointServiceConfiguration' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {networkLoadBalancerArns = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>.
createVpcEndpointServiceConfiguration_clientToken :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Text)
createVpcEndpointServiceConfiguration_clientToken = Lens.lens (\CreateVpcEndpointServiceConfiguration' {clientToken} -> clientToken) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {clientToken = a} :: CreateVpcEndpointServiceConfiguration)

instance
  Prelude.AWSRequest
    CreateVpcEndpointServiceConfiguration
  where
  type
    Rs CreateVpcEndpointServiceConfiguration =
      CreateVpcEndpointServiceConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpcEndpointServiceConfigurationResponse'
            Prelude.<$> (x Prelude..@? "serviceConfiguration")
              Prelude.<*> (x Prelude..@? "clientToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVpcEndpointServiceConfiguration

instance
  Prelude.NFData
    CreateVpcEndpointServiceConfiguration

instance
  Prelude.ToHeaders
    CreateVpcEndpointServiceConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    CreateVpcEndpointServiceConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    CreateVpcEndpointServiceConfiguration
  where
  toQuery CreateVpcEndpointServiceConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "CreateVpcEndpointServiceConfiguration" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "GatewayLoadBalancerArn"
              Prelude.<$> gatewayLoadBalancerArns
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          ),
        "DryRun" Prelude.=: dryRun,
        "PrivateDnsName" Prelude.=: privateDnsName,
        "AcceptanceRequired" Prelude.=: acceptanceRequired,
        Prelude.toQuery
          ( Prelude.toQueryList "NetworkLoadBalancerArn"
              Prelude.<$> networkLoadBalancerArns
          ),
        "ClientToken" Prelude.=: clientToken
      ]

-- | /See:/ 'newCreateVpcEndpointServiceConfigurationResponse' smart constructor.
data CreateVpcEndpointServiceConfigurationResponse = CreateVpcEndpointServiceConfigurationResponse'
  { -- | Information about the service configuration.
    serviceConfiguration :: Prelude.Maybe ServiceConfiguration,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointServiceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceConfiguration', 'createVpcEndpointServiceConfigurationResponse_serviceConfiguration' - Information about the service configuration.
--
-- 'clientToken', 'createVpcEndpointServiceConfigurationResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'httpStatus', 'createVpcEndpointServiceConfigurationResponse_httpStatus' - The response's http status code.
newCreateVpcEndpointServiceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcEndpointServiceConfigurationResponse
newCreateVpcEndpointServiceConfigurationResponse
  pHttpStatus_ =
    CreateVpcEndpointServiceConfigurationResponse'
      { serviceConfiguration =
          Prelude.Nothing,
        clientToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the service configuration.
createVpcEndpointServiceConfigurationResponse_serviceConfiguration :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse (Prelude.Maybe ServiceConfiguration)
createVpcEndpointServiceConfigurationResponse_serviceConfiguration = Lens.lens (\CreateVpcEndpointServiceConfigurationResponse' {serviceConfiguration} -> serviceConfiguration) (\s@CreateVpcEndpointServiceConfigurationResponse' {} a -> s {serviceConfiguration = a} :: CreateVpcEndpointServiceConfigurationResponse)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createVpcEndpointServiceConfigurationResponse_clientToken :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse (Prelude.Maybe Prelude.Text)
createVpcEndpointServiceConfigurationResponse_clientToken = Lens.lens (\CreateVpcEndpointServiceConfigurationResponse' {clientToken} -> clientToken) (\s@CreateVpcEndpointServiceConfigurationResponse' {} a -> s {clientToken = a} :: CreateVpcEndpointServiceConfigurationResponse)

-- | The response's http status code.
createVpcEndpointServiceConfigurationResponse_httpStatus :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse Prelude.Int
createVpcEndpointServiceConfigurationResponse_httpStatus = Lens.lens (\CreateVpcEndpointServiceConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateVpcEndpointServiceConfigurationResponse' {} a -> s {httpStatus = a} :: CreateVpcEndpointServiceConfigurationResponse)

instance
  Prelude.NFData
    CreateVpcEndpointServiceConfigurationResponse
