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
-- Module      : Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of your VPC endpoint service configuration. You
-- can change the Network Load Balancers or Gateway Load Balancers for your
-- service, and you can specify whether acceptance is required for requests
-- to connect to your endpoint service through an interface VPC endpoint.
--
-- If you set or modify the private DNS name, you must prove that you own
-- the private DNS domain name. For more information, see
-- <https://docs.aws.amazon.com/vpc/latest/userguide/endpoint-services-dns-validation.html VPC Endpoint Service Private DNS Name Verification>
-- in the /Amazon Virtual Private Cloud User Guide/.
module Network.AWS.EC2.ModifyVpcEndpointServiceConfiguration
  ( -- * Creating a Request
    ModifyVpcEndpointServiceConfiguration (..),
    newModifyVpcEndpointServiceConfiguration,

    -- * Request Lenses
    modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_dryRun,
    modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_privateDnsName,
    modifyVpcEndpointServiceConfiguration_acceptanceRequired,
    modifyVpcEndpointServiceConfiguration_removePrivateDnsName,
    modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_serviceId,

    -- * Destructuring the Response
    ModifyVpcEndpointServiceConfigurationResponse (..),
    newModifyVpcEndpointServiceConfigurationResponse,

    -- * Response Lenses
    modifyVpcEndpointServiceConfigurationResponse_return,
    modifyVpcEndpointServiceConfigurationResponse_httpStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyVpcEndpointServiceConfiguration' smart constructor.
data ModifyVpcEndpointServiceConfiguration = ModifyVpcEndpointServiceConfiguration'
  { -- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove
    -- from your service configuration.
    removeNetworkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to
    -- your service configuration.
    addNetworkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint configuration) The private DNS name to assign to the
    -- endpoint service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether requests to create an endpoint to your service must be
    -- accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | (Interface endpoint configuration) Removes the private DNS name of the
    -- endpoint service.
    removePrivateDnsName :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to
    -- your service configuration.
    addGatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove
    -- from your service configuration.
    removeGatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the service.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeNetworkLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns' - The Amazon Resource Names (ARNs) of Network Load Balancers to remove
-- from your service configuration.
--
-- 'dryRun', 'modifyVpcEndpointServiceConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'addNetworkLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns' - The Amazon Resource Names (ARNs) of Network Load Balancers to add to
-- your service configuration.
--
-- 'privateDnsName', 'modifyVpcEndpointServiceConfiguration_privateDnsName' - (Interface endpoint configuration) The private DNS name to assign to the
-- endpoint service.
--
-- 'acceptanceRequired', 'modifyVpcEndpointServiceConfiguration_acceptanceRequired' - Indicates whether requests to create an endpoint to your service must be
-- accepted.
--
-- 'removePrivateDnsName', 'modifyVpcEndpointServiceConfiguration_removePrivateDnsName' - (Interface endpoint configuration) Removes the private DNS name of the
-- endpoint service.
--
-- 'addGatewayLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to
-- your service configuration.
--
-- 'removeGatewayLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove
-- from your service configuration.
--
-- 'serviceId', 'modifyVpcEndpointServiceConfiguration_serviceId' - The ID of the service.
newModifyVpcEndpointServiceConfiguration ::
  -- | 'serviceId'
  Prelude.Text ->
  ModifyVpcEndpointServiceConfiguration
newModifyVpcEndpointServiceConfiguration pServiceId_ =
  ModifyVpcEndpointServiceConfiguration'
    { removeNetworkLoadBalancerArns =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      addNetworkLoadBalancerArns =
        Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      acceptanceRequired = Prelude.Nothing,
      removePrivateDnsName =
        Prelude.Nothing,
      addGatewayLoadBalancerArns =
        Prelude.Nothing,
      removeGatewayLoadBalancerArns =
        Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove
-- from your service configuration.
modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removeNetworkLoadBalancerArns} -> removeNetworkLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removeNetworkLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointServiceConfiguration_dryRun :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfiguration_dryRun = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {dryRun} -> dryRun) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {dryRun = a} :: ModifyVpcEndpointServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to
-- your service configuration.
modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {addNetworkLoadBalancerArns} -> addNetworkLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {addNetworkLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | (Interface endpoint configuration) The private DNS name to assign to the
-- endpoint service.
modifyVpcEndpointServiceConfiguration_privateDnsName :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Text)
modifyVpcEndpointServiceConfiguration_privateDnsName = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {privateDnsName = a} :: ModifyVpcEndpointServiceConfiguration)

-- | Indicates whether requests to create an endpoint to your service must be
-- accepted.
modifyVpcEndpointServiceConfiguration_acceptanceRequired :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfiguration_acceptanceRequired = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {acceptanceRequired = a} :: ModifyVpcEndpointServiceConfiguration)

-- | (Interface endpoint configuration) Removes the private DNS name of the
-- endpoint service.
modifyVpcEndpointServiceConfiguration_removePrivateDnsName :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfiguration_removePrivateDnsName = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removePrivateDnsName} -> removePrivateDnsName) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removePrivateDnsName = a} :: ModifyVpcEndpointServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to
-- your service configuration.
modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {addGatewayLoadBalancerArns} -> addGatewayLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {addGatewayLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove
-- from your service configuration.
modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removeGatewayLoadBalancerArns} -> removeGatewayLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removeGatewayLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the service.
modifyVpcEndpointServiceConfiguration_serviceId :: Lens.Lens' ModifyVpcEndpointServiceConfiguration Prelude.Text
modifyVpcEndpointServiceConfiguration_serviceId = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {serviceId} -> serviceId) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {serviceId = a} :: ModifyVpcEndpointServiceConfiguration)

instance
  Prelude.AWSRequest
    ModifyVpcEndpointServiceConfiguration
  where
  type
    Rs ModifyVpcEndpointServiceConfiguration =
      ModifyVpcEndpointServiceConfigurationResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointServiceConfigurationResponse'
            Prelude.<$> (x Prelude..@? "return")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointServiceConfiguration

instance
  Prelude.NFData
    ModifyVpcEndpointServiceConfiguration

instance
  Prelude.ToHeaders
    ModifyVpcEndpointServiceConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    ModifyVpcEndpointServiceConfiguration
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    ModifyVpcEndpointServiceConfiguration
  where
  toQuery ModifyVpcEndpointServiceConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "ModifyVpcEndpointServiceConfiguration" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        Prelude.toQuery
          ( Prelude.toQueryList "RemoveNetworkLoadBalancerArn"
              Prelude.<$> removeNetworkLoadBalancerArns
          ),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQuery
          ( Prelude.toQueryList "AddNetworkLoadBalancerArn"
              Prelude.<$> addNetworkLoadBalancerArns
          ),
        "PrivateDnsName" Prelude.=: privateDnsName,
        "AcceptanceRequired" Prelude.=: acceptanceRequired,
        "RemovePrivateDnsName"
          Prelude.=: removePrivateDnsName,
        Prelude.toQuery
          ( Prelude.toQueryList "AddGatewayLoadBalancerArn"
              Prelude.<$> addGatewayLoadBalancerArns
          ),
        Prelude.toQuery
          ( Prelude.toQueryList "RemoveGatewayLoadBalancerArn"
              Prelude.<$> removeGatewayLoadBalancerArns
          ),
        "ServiceId" Prelude.=: serviceId
      ]

-- | /See:/ 'newModifyVpcEndpointServiceConfigurationResponse' smart constructor.
data ModifyVpcEndpointServiceConfigurationResponse = ModifyVpcEndpointServiceConfigurationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServiceConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'modifyVpcEndpointServiceConfigurationResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'modifyVpcEndpointServiceConfigurationResponse_httpStatus' - The response's http status code.
newModifyVpcEndpointServiceConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVpcEndpointServiceConfigurationResponse
newModifyVpcEndpointServiceConfigurationResponse
  pHttpStatus_ =
    ModifyVpcEndpointServiceConfigurationResponse'
      { return' =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
modifyVpcEndpointServiceConfigurationResponse_return :: Lens.Lens' ModifyVpcEndpointServiceConfigurationResponse (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfigurationResponse_return = Lens.lens (\ModifyVpcEndpointServiceConfigurationResponse' {return'} -> return') (\s@ModifyVpcEndpointServiceConfigurationResponse' {} a -> s {return' = a} :: ModifyVpcEndpointServiceConfigurationResponse)

-- | The response's http status code.
modifyVpcEndpointServiceConfigurationResponse_httpStatus :: Lens.Lens' ModifyVpcEndpointServiceConfigurationResponse Prelude.Int
modifyVpcEndpointServiceConfigurationResponse_httpStatus = Lens.lens (\ModifyVpcEndpointServiceConfigurationResponse' {httpStatus} -> httpStatus) (\s@ModifyVpcEndpointServiceConfigurationResponse' {} a -> s {httpStatus = a} :: ModifyVpcEndpointServiceConfigurationResponse)

instance
  Prelude.NFData
    ModifyVpcEndpointServiceConfigurationResponse
