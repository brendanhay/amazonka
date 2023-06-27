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
-- Module      : Amazonka.EC2.CreateVpcEndpointServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a VPC endpoint service to which service consumers (Amazon Web
-- Services accounts, users, and IAM roles) can connect.
--
-- Before you create an endpoint service, you must create one of the
-- following for your service:
--
-- -   A
--     <https://docs.aws.amazon.com/elasticloadbalancing/latest/network/ Network Load Balancer>.
--     Service consumers connect to your service using an interface
--     endpoint.
--
-- -   A
--     <https://docs.aws.amazon.com/elasticloadbalancing/latest/gateway/ Gateway Load Balancer>.
--     Service consumers connect to your service using a Gateway Load
--     Balancer endpoint.
--
-- If you set the private DNS name, you must prove that you own the private
-- DNS domain name.
--
-- For more information, see the
-- <https://docs.aws.amazon.com/vpc/latest/privatelink/ Amazon Web Services PrivateLink Guide>.
module Amazonka.EC2.CreateVpcEndpointServiceConfiguration
  ( -- * Creating a Request
    CreateVpcEndpointServiceConfiguration (..),
    newCreateVpcEndpointServiceConfiguration,

    -- * Request Lenses
    createVpcEndpointServiceConfiguration_acceptanceRequired,
    createVpcEndpointServiceConfiguration_clientToken,
    createVpcEndpointServiceConfiguration_dryRun,
    createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns,
    createVpcEndpointServiceConfiguration_networkLoadBalancerArns,
    createVpcEndpointServiceConfiguration_privateDnsName,
    createVpcEndpointServiceConfiguration_supportedIpAddressTypes,
    createVpcEndpointServiceConfiguration_tagSpecifications,

    -- * Destructuring the Response
    CreateVpcEndpointServiceConfigurationResponse (..),
    newCreateVpcEndpointServiceConfigurationResponse,

    -- * Response Lenses
    createVpcEndpointServiceConfigurationResponse_clientToken,
    createVpcEndpointServiceConfigurationResponse_serviceConfiguration,
    createVpcEndpointServiceConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateVpcEndpointServiceConfiguration' smart constructor.
data CreateVpcEndpointServiceConfiguration = CreateVpcEndpointServiceConfiguration'
  { -- | Indicates whether requests from service consumers to create an endpoint
    -- to your service must be accepted manually.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers.
    gatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of the Network Load Balancers.
    networkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint configuration) The private DNS name to assign to the
    -- VPC endpoint service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The supported IP address types. The possible values are @ipv4@ and
    -- @ipv6@.
    supportedIpAddressTypes :: Prelude.Maybe [Prelude.Text],
    -- | The tags to associate with the service.
    tagSpecifications :: Prelude.Maybe [TagSpecification]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptanceRequired', 'createVpcEndpointServiceConfiguration_acceptanceRequired' - Indicates whether requests from service consumers to create an endpoint
-- to your service must be accepted manually.
--
-- 'clientToken', 'createVpcEndpointServiceConfiguration_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
--
-- 'dryRun', 'createVpcEndpointServiceConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'gatewayLoadBalancerArns', 'createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Gateway Load Balancers.
--
-- 'networkLoadBalancerArns', 'createVpcEndpointServiceConfiguration_networkLoadBalancerArns' - The Amazon Resource Names (ARNs) of the Network Load Balancers.
--
-- 'privateDnsName', 'createVpcEndpointServiceConfiguration_privateDnsName' - (Interface endpoint configuration) The private DNS name to assign to the
-- VPC endpoint service.
--
-- 'supportedIpAddressTypes', 'createVpcEndpointServiceConfiguration_supportedIpAddressTypes' - The supported IP address types. The possible values are @ipv4@ and
-- @ipv6@.
--
-- 'tagSpecifications', 'createVpcEndpointServiceConfiguration_tagSpecifications' - The tags to associate with the service.
newCreateVpcEndpointServiceConfiguration ::
  CreateVpcEndpointServiceConfiguration
newCreateVpcEndpointServiceConfiguration =
  CreateVpcEndpointServiceConfiguration'
    { acceptanceRequired =
        Prelude.Nothing,
      clientToken = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      gatewayLoadBalancerArns =
        Prelude.Nothing,
      networkLoadBalancerArns =
        Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      supportedIpAddressTypes =
        Prelude.Nothing,
      tagSpecifications = Prelude.Nothing
    }

-- | Indicates whether requests from service consumers to create an endpoint
-- to your service must be accepted manually.
createVpcEndpointServiceConfiguration_acceptanceRequired :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
createVpcEndpointServiceConfiguration_acceptanceRequired = Lens.lens (\CreateVpcEndpointServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {acceptanceRequired = a} :: CreateVpcEndpointServiceConfiguration)

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to ensure idempotency>.
createVpcEndpointServiceConfiguration_clientToken :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Text)
createVpcEndpointServiceConfiguration_clientToken = Lens.lens (\CreateVpcEndpointServiceConfiguration' {clientToken} -> clientToken) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {clientToken = a} :: CreateVpcEndpointServiceConfiguration)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createVpcEndpointServiceConfiguration_dryRun :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
createVpcEndpointServiceConfiguration_dryRun = Lens.lens (\CreateVpcEndpointServiceConfiguration' {dryRun} -> dryRun) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {dryRun = a} :: CreateVpcEndpointServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of the Gateway Load Balancers.
createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
createVpcEndpointServiceConfiguration_gatewayLoadBalancerArns = Lens.lens (\CreateVpcEndpointServiceConfiguration' {gatewayLoadBalancerArns} -> gatewayLoadBalancerArns) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {gatewayLoadBalancerArns = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of the Network Load Balancers.
createVpcEndpointServiceConfiguration_networkLoadBalancerArns :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
createVpcEndpointServiceConfiguration_networkLoadBalancerArns = Lens.lens (\CreateVpcEndpointServiceConfiguration' {networkLoadBalancerArns} -> networkLoadBalancerArns) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {networkLoadBalancerArns = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | (Interface endpoint configuration) The private DNS name to assign to the
-- VPC endpoint service.
createVpcEndpointServiceConfiguration_privateDnsName :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Text)
createVpcEndpointServiceConfiguration_privateDnsName = Lens.lens (\CreateVpcEndpointServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {privateDnsName = a} :: CreateVpcEndpointServiceConfiguration)

-- | The supported IP address types. The possible values are @ipv4@ and
-- @ipv6@.
createVpcEndpointServiceConfiguration_supportedIpAddressTypes :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
createVpcEndpointServiceConfiguration_supportedIpAddressTypes = Lens.lens (\CreateVpcEndpointServiceConfiguration' {supportedIpAddressTypes} -> supportedIpAddressTypes) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {supportedIpAddressTypes = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The tags to associate with the service.
createVpcEndpointServiceConfiguration_tagSpecifications :: Lens.Lens' CreateVpcEndpointServiceConfiguration (Prelude.Maybe [TagSpecification])
createVpcEndpointServiceConfiguration_tagSpecifications = Lens.lens (\CreateVpcEndpointServiceConfiguration' {tagSpecifications} -> tagSpecifications) (\s@CreateVpcEndpointServiceConfiguration' {} a -> s {tagSpecifications = a} :: CreateVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    CreateVpcEndpointServiceConfiguration
  where
  type
    AWSResponse
      CreateVpcEndpointServiceConfiguration =
      CreateVpcEndpointServiceConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          CreateVpcEndpointServiceConfigurationResponse'
            Prelude.<$> (x Data..@? "clientToken")
            Prelude.<*> (x Data..@? "serviceConfiguration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateVpcEndpointServiceConfiguration
  where
  hashWithSalt
    _salt
    CreateVpcEndpointServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` acceptanceRequired
        `Prelude.hashWithSalt` clientToken
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` gatewayLoadBalancerArns
        `Prelude.hashWithSalt` networkLoadBalancerArns
        `Prelude.hashWithSalt` privateDnsName
        `Prelude.hashWithSalt` supportedIpAddressTypes
        `Prelude.hashWithSalt` tagSpecifications

instance
  Prelude.NFData
    CreateVpcEndpointServiceConfiguration
  where
  rnf CreateVpcEndpointServiceConfiguration' {..} =
    Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf gatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf networkLoadBalancerArns
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf supportedIpAddressTypes
      `Prelude.seq` Prelude.rnf tagSpecifications

instance
  Data.ToHeaders
    CreateVpcEndpointServiceConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    CreateVpcEndpointServiceConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    CreateVpcEndpointServiceConfiguration
  where
  toQuery CreateVpcEndpointServiceConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "CreateVpcEndpointServiceConfiguration" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AcceptanceRequired" Data.=: acceptanceRequired,
        "ClientToken" Data.=: clientToken,
        "DryRun" Data.=: dryRun,
        Data.toQuery
          ( Data.toQueryList "GatewayLoadBalancerArn"
              Prelude.<$> gatewayLoadBalancerArns
          ),
        Data.toQuery
          ( Data.toQueryList "NetworkLoadBalancerArn"
              Prelude.<$> networkLoadBalancerArns
          ),
        "PrivateDnsName" Data.=: privateDnsName,
        Data.toQuery
          ( Data.toQueryList "SupportedIpAddressType"
              Prelude.<$> supportedIpAddressTypes
          ),
        Data.toQuery
          ( Data.toQueryList "TagSpecification"
              Prelude.<$> tagSpecifications
          )
      ]

-- | /See:/ 'newCreateVpcEndpointServiceConfigurationResponse' smart constructor.
data CreateVpcEndpointServiceConfigurationResponse = CreateVpcEndpointServiceConfigurationResponse'
  { -- | Unique, case-sensitive identifier that you provide to ensure the
    -- idempotency of the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the service configuration.
    serviceConfiguration :: Prelude.Maybe ServiceConfiguration,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateVpcEndpointServiceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createVpcEndpointServiceConfigurationResponse_clientToken' - Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
--
-- 'serviceConfiguration', 'createVpcEndpointServiceConfigurationResponse_serviceConfiguration' - Information about the service configuration.
--
-- 'httpStatus', 'createVpcEndpointServiceConfigurationResponse_httpStatus' - The response's http status code.
newCreateVpcEndpointServiceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateVpcEndpointServiceConfigurationResponse
newCreateVpcEndpointServiceConfigurationResponse
  pHttpStatus_ =
    CreateVpcEndpointServiceConfigurationResponse'
      { clientToken =
          Prelude.Nothing,
        serviceConfiguration =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Unique, case-sensitive identifier that you provide to ensure the
-- idempotency of the request.
createVpcEndpointServiceConfigurationResponse_clientToken :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse (Prelude.Maybe Prelude.Text)
createVpcEndpointServiceConfigurationResponse_clientToken = Lens.lens (\CreateVpcEndpointServiceConfigurationResponse' {clientToken} -> clientToken) (\s@CreateVpcEndpointServiceConfigurationResponse' {} a -> s {clientToken = a} :: CreateVpcEndpointServiceConfigurationResponse)

-- | Information about the service configuration.
createVpcEndpointServiceConfigurationResponse_serviceConfiguration :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse (Prelude.Maybe ServiceConfiguration)
createVpcEndpointServiceConfigurationResponse_serviceConfiguration = Lens.lens (\CreateVpcEndpointServiceConfigurationResponse' {serviceConfiguration} -> serviceConfiguration) (\s@CreateVpcEndpointServiceConfigurationResponse' {} a -> s {serviceConfiguration = a} :: CreateVpcEndpointServiceConfigurationResponse)

-- | The response's http status code.
createVpcEndpointServiceConfigurationResponse_httpStatus :: Lens.Lens' CreateVpcEndpointServiceConfigurationResponse Prelude.Int
createVpcEndpointServiceConfigurationResponse_httpStatus = Lens.lens (\CreateVpcEndpointServiceConfigurationResponse' {httpStatus} -> httpStatus) (\s@CreateVpcEndpointServiceConfigurationResponse' {} a -> s {httpStatus = a} :: CreateVpcEndpointServiceConfigurationResponse)

instance
  Prelude.NFData
    CreateVpcEndpointServiceConfigurationResponse
  where
  rnf
    CreateVpcEndpointServiceConfigurationResponse' {..} =
      Prelude.rnf clientToken
        `Prelude.seq` Prelude.rnf serviceConfiguration
        `Prelude.seq` Prelude.rnf httpStatus
