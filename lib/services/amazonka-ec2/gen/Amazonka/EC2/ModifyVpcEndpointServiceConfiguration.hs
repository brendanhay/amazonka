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
-- Module      : Amazonka.EC2.ModifyVpcEndpointServiceConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the attributes of your VPC endpoint service configuration. You
-- can change the Network Load Balancers or Gateway Load Balancers for your
-- service, and you can specify whether acceptance is required for requests
-- to connect to your endpoint service through an interface VPC endpoint.
--
-- If you set or modify the private DNS name, you must prove that you own
-- the private DNS domain name.
module Amazonka.EC2.ModifyVpcEndpointServiceConfiguration
  ( -- * Creating a Request
    ModifyVpcEndpointServiceConfiguration (..),
    newModifyVpcEndpointServiceConfiguration,

    -- * Request Lenses
    modifyVpcEndpointServiceConfiguration_acceptanceRequired,
    modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_addSupportedIpAddressTypes,
    modifyVpcEndpointServiceConfiguration_dryRun,
    modifyVpcEndpointServiceConfiguration_privateDnsName,
    modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns,
    modifyVpcEndpointServiceConfiguration_removePrivateDnsName,
    modifyVpcEndpointServiceConfiguration_removeSupportedIpAddressTypes,
    modifyVpcEndpointServiceConfiguration_serviceId,

    -- * Destructuring the Response
    ModifyVpcEndpointServiceConfigurationResponse (..),
    newModifyVpcEndpointServiceConfigurationResponse,

    -- * Response Lenses
    modifyVpcEndpointServiceConfigurationResponse_return,
    modifyVpcEndpointServiceConfigurationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVpcEndpointServiceConfiguration' smart constructor.
data ModifyVpcEndpointServiceConfiguration = ModifyVpcEndpointServiceConfiguration'
  { -- | Indicates whether requests to create an endpoint to your service must be
    -- accepted.
    acceptanceRequired :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to
    -- your service configuration.
    addGatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to
    -- your service configuration.
    addNetworkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The IP address types to add to your service configuration.
    addSupportedIpAddressTypes :: Prelude.Maybe [Prelude.Text],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | (Interface endpoint configuration) The private DNS name to assign to the
    -- endpoint service.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove
    -- from your service configuration.
    removeGatewayLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove
    -- from your service configuration.
    removeNetworkLoadBalancerArns :: Prelude.Maybe [Prelude.Text],
    -- | (Interface endpoint configuration) Removes the private DNS name of the
    -- endpoint service.
    removePrivateDnsName :: Prelude.Maybe Prelude.Bool,
    -- | The IP address types to remove from your service configuration.
    removeSupportedIpAddressTypes :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the service.
    serviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVpcEndpointServiceConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptanceRequired', 'modifyVpcEndpointServiceConfiguration_acceptanceRequired' - Indicates whether requests to create an endpoint to your service must be
-- accepted.
--
-- 'addGatewayLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to
-- your service configuration.
--
-- 'addNetworkLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns' - The Amazon Resource Names (ARNs) of Network Load Balancers to add to
-- your service configuration.
--
-- 'addSupportedIpAddressTypes', 'modifyVpcEndpointServiceConfiguration_addSupportedIpAddressTypes' - The IP address types to add to your service configuration.
--
-- 'dryRun', 'modifyVpcEndpointServiceConfiguration_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'privateDnsName', 'modifyVpcEndpointServiceConfiguration_privateDnsName' - (Interface endpoint configuration) The private DNS name to assign to the
-- endpoint service.
--
-- 'removeGatewayLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns' - The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove
-- from your service configuration.
--
-- 'removeNetworkLoadBalancerArns', 'modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns' - The Amazon Resource Names (ARNs) of Network Load Balancers to remove
-- from your service configuration.
--
-- 'removePrivateDnsName', 'modifyVpcEndpointServiceConfiguration_removePrivateDnsName' - (Interface endpoint configuration) Removes the private DNS name of the
-- endpoint service.
--
-- 'removeSupportedIpAddressTypes', 'modifyVpcEndpointServiceConfiguration_removeSupportedIpAddressTypes' - The IP address types to remove from your service configuration.
--
-- 'serviceId', 'modifyVpcEndpointServiceConfiguration_serviceId' - The ID of the service.
newModifyVpcEndpointServiceConfiguration ::
  -- | 'serviceId'
  Prelude.Text ->
  ModifyVpcEndpointServiceConfiguration
newModifyVpcEndpointServiceConfiguration pServiceId_ =
  ModifyVpcEndpointServiceConfiguration'
    { acceptanceRequired =
        Prelude.Nothing,
      addGatewayLoadBalancerArns =
        Prelude.Nothing,
      addNetworkLoadBalancerArns =
        Prelude.Nothing,
      addSupportedIpAddressTypes =
        Prelude.Nothing,
      dryRun = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      removeGatewayLoadBalancerArns =
        Prelude.Nothing,
      removeNetworkLoadBalancerArns =
        Prelude.Nothing,
      removePrivateDnsName =
        Prelude.Nothing,
      removeSupportedIpAddressTypes =
        Prelude.Nothing,
      serviceId = pServiceId_
    }

-- | Indicates whether requests to create an endpoint to your service must be
-- accepted.
modifyVpcEndpointServiceConfiguration_acceptanceRequired :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfiguration_acceptanceRequired = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {acceptanceRequired} -> acceptanceRequired) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {acceptanceRequired = a} :: ModifyVpcEndpointServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to add to
-- your service configuration.
modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_addGatewayLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {addGatewayLoadBalancerArns} -> addGatewayLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {addGatewayLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to add to
-- your service configuration.
modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_addNetworkLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {addNetworkLoadBalancerArns} -> addNetworkLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {addNetworkLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The IP address types to add to your service configuration.
modifyVpcEndpointServiceConfiguration_addSupportedIpAddressTypes :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_addSupportedIpAddressTypes = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {addSupportedIpAddressTypes} -> addSupportedIpAddressTypes) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {addSupportedIpAddressTypes = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVpcEndpointServiceConfiguration_dryRun :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfiguration_dryRun = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {dryRun} -> dryRun) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {dryRun = a} :: ModifyVpcEndpointServiceConfiguration)

-- | (Interface endpoint configuration) The private DNS name to assign to the
-- endpoint service.
modifyVpcEndpointServiceConfiguration_privateDnsName :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Text)
modifyVpcEndpointServiceConfiguration_privateDnsName = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {privateDnsName} -> privateDnsName) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {privateDnsName = a} :: ModifyVpcEndpointServiceConfiguration)

-- | The Amazon Resource Names (ARNs) of Gateway Load Balancers to remove
-- from your service configuration.
modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_removeGatewayLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removeGatewayLoadBalancerArns} -> removeGatewayLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removeGatewayLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Names (ARNs) of Network Load Balancers to remove
-- from your service configuration.
modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_removeNetworkLoadBalancerArns = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removeNetworkLoadBalancerArns} -> removeNetworkLoadBalancerArns) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removeNetworkLoadBalancerArns = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | (Interface endpoint configuration) Removes the private DNS name of the
-- endpoint service.
modifyVpcEndpointServiceConfiguration_removePrivateDnsName :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe Prelude.Bool)
modifyVpcEndpointServiceConfiguration_removePrivateDnsName = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removePrivateDnsName} -> removePrivateDnsName) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removePrivateDnsName = a} :: ModifyVpcEndpointServiceConfiguration)

-- | The IP address types to remove from your service configuration.
modifyVpcEndpointServiceConfiguration_removeSupportedIpAddressTypes :: Lens.Lens' ModifyVpcEndpointServiceConfiguration (Prelude.Maybe [Prelude.Text])
modifyVpcEndpointServiceConfiguration_removeSupportedIpAddressTypes = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {removeSupportedIpAddressTypes} -> removeSupportedIpAddressTypes) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {removeSupportedIpAddressTypes = a} :: ModifyVpcEndpointServiceConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the service.
modifyVpcEndpointServiceConfiguration_serviceId :: Lens.Lens' ModifyVpcEndpointServiceConfiguration Prelude.Text
modifyVpcEndpointServiceConfiguration_serviceId = Lens.lens (\ModifyVpcEndpointServiceConfiguration' {serviceId} -> serviceId) (\s@ModifyVpcEndpointServiceConfiguration' {} a -> s {serviceId = a} :: ModifyVpcEndpointServiceConfiguration)

instance
  Core.AWSRequest
    ModifyVpcEndpointServiceConfiguration
  where
  type
    AWSResponse
      ModifyVpcEndpointServiceConfiguration =
      ModifyVpcEndpointServiceConfigurationResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVpcEndpointServiceConfigurationResponse'
            Prelude.<$> (x Data..@? "return")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVpcEndpointServiceConfiguration
  where
  hashWithSalt
    _salt
    ModifyVpcEndpointServiceConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` acceptanceRequired
        `Prelude.hashWithSalt` addGatewayLoadBalancerArns
        `Prelude.hashWithSalt` addNetworkLoadBalancerArns
        `Prelude.hashWithSalt` addSupportedIpAddressTypes
        `Prelude.hashWithSalt` dryRun
        `Prelude.hashWithSalt` privateDnsName
        `Prelude.hashWithSalt` removeGatewayLoadBalancerArns
        `Prelude.hashWithSalt` removeNetworkLoadBalancerArns
        `Prelude.hashWithSalt` removePrivateDnsName
        `Prelude.hashWithSalt` removeSupportedIpAddressTypes
        `Prelude.hashWithSalt` serviceId

instance
  Prelude.NFData
    ModifyVpcEndpointServiceConfiguration
  where
  rnf ModifyVpcEndpointServiceConfiguration' {..} =
    Prelude.rnf acceptanceRequired
      `Prelude.seq` Prelude.rnf addGatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf addNetworkLoadBalancerArns
      `Prelude.seq` Prelude.rnf addSupportedIpAddressTypes
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf removeGatewayLoadBalancerArns
      `Prelude.seq` Prelude.rnf removeNetworkLoadBalancerArns
      `Prelude.seq` Prelude.rnf removePrivateDnsName
      `Prelude.seq` Prelude.rnf removeSupportedIpAddressTypes
      `Prelude.seq` Prelude.rnf serviceId

instance
  Data.ToHeaders
    ModifyVpcEndpointServiceConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToPath
    ModifyVpcEndpointServiceConfiguration
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ModifyVpcEndpointServiceConfiguration
  where
  toQuery ModifyVpcEndpointServiceConfiguration' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVpcEndpointServiceConfiguration" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "AcceptanceRequired" Data.=: acceptanceRequired,
        Data.toQuery
          ( Data.toQueryList "AddGatewayLoadBalancerArn"
              Prelude.<$> addGatewayLoadBalancerArns
          ),
        Data.toQuery
          ( Data.toQueryList "AddNetworkLoadBalancerArn"
              Prelude.<$> addNetworkLoadBalancerArns
          ),
        Data.toQuery
          ( Data.toQueryList "AddSupportedIpAddressType"
              Prelude.<$> addSupportedIpAddressTypes
          ),
        "DryRun" Data.=: dryRun,
        "PrivateDnsName" Data.=: privateDnsName,
        Data.toQuery
          ( Data.toQueryList "RemoveGatewayLoadBalancerArn"
              Prelude.<$> removeGatewayLoadBalancerArns
          ),
        Data.toQuery
          ( Data.toQueryList "RemoveNetworkLoadBalancerArn"
              Prelude.<$> removeNetworkLoadBalancerArns
          ),
        "RemovePrivateDnsName" Data.=: removePrivateDnsName,
        Data.toQuery
          ( Data.toQueryList "RemoveSupportedIpAddressType"
              Prelude.<$> removeSupportedIpAddressTypes
          ),
        "ServiceId" Data.=: serviceId
      ]

-- | /See:/ 'newModifyVpcEndpointServiceConfigurationResponse' smart constructor.
data ModifyVpcEndpointServiceConfigurationResponse = ModifyVpcEndpointServiceConfigurationResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Prelude.Maybe Prelude.Bool,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf
    ModifyVpcEndpointServiceConfigurationResponse' {..} =
      Prelude.rnf return'
        `Prelude.seq` Prelude.rnf httpStatus
