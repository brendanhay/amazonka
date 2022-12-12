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
-- Module      : Amazonka.EC2.ModifyVerifiedAccessEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the configuration of an Amazon Web Services Verified Access
-- endpoint.
module Amazonka.EC2.ModifyVerifiedAccessEndpoint
  ( -- * Creating a Request
    ModifyVerifiedAccessEndpoint (..),
    newModifyVerifiedAccessEndpoint,

    -- * Request Lenses
    modifyVerifiedAccessEndpoint_clientToken,
    modifyVerifiedAccessEndpoint_description,
    modifyVerifiedAccessEndpoint_dryRun,
    modifyVerifiedAccessEndpoint_loadBalancerOptions,
    modifyVerifiedAccessEndpoint_networkInterfaceOptions,
    modifyVerifiedAccessEndpoint_verifiedAccessGroupId,
    modifyVerifiedAccessEndpoint_verifiedAccessEndpointId,

    -- * Destructuring the Response
    ModifyVerifiedAccessEndpointResponse (..),
    newModifyVerifiedAccessEndpointResponse,

    -- * Response Lenses
    modifyVerifiedAccessEndpointResponse_verifiedAccessEndpoint,
    modifyVerifiedAccessEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyVerifiedAccessEndpoint' smart constructor.
data ModifyVerifiedAccessEndpoint = ModifyVerifiedAccessEndpoint'
  { -- | A unique, case-sensitive token that you provide to ensure idempotency of
    -- your modification request. For more information, see
    -- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description for the Amazon Web Services Verified Access endpoint.
    description :: Prelude.Maybe Prelude.Text,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The load balancer details if creating the Amazon Web Services Verified
    -- Access endpoint as @load-balancer@type.
    loadBalancerOptions :: Prelude.Maybe ModifyVerifiedAccessEndpointLoadBalancerOptions,
    -- | The network interface options.
    networkInterfaceOptions :: Prelude.Maybe ModifyVerifiedAccessEndpointEniOptions,
    -- | The ID of the Amazon Web Services Verified Access group.
    verifiedAccessGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services Verified Access endpoint.
    verifiedAccessEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'modifyVerifiedAccessEndpoint_clientToken' - A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
--
-- 'description', 'modifyVerifiedAccessEndpoint_description' - A description for the Amazon Web Services Verified Access endpoint.
--
-- 'dryRun', 'modifyVerifiedAccessEndpoint_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'loadBalancerOptions', 'modifyVerifiedAccessEndpoint_loadBalancerOptions' - The load balancer details if creating the Amazon Web Services Verified
-- Access endpoint as @load-balancer@type.
--
-- 'networkInterfaceOptions', 'modifyVerifiedAccessEndpoint_networkInterfaceOptions' - The network interface options.
--
-- 'verifiedAccessGroupId', 'modifyVerifiedAccessEndpoint_verifiedAccessGroupId' - The ID of the Amazon Web Services Verified Access group.
--
-- 'verifiedAccessEndpointId', 'modifyVerifiedAccessEndpoint_verifiedAccessEndpointId' - The ID of the Amazon Web Services Verified Access endpoint.
newModifyVerifiedAccessEndpoint ::
  -- | 'verifiedAccessEndpointId'
  Prelude.Text ->
  ModifyVerifiedAccessEndpoint
newModifyVerifiedAccessEndpoint
  pVerifiedAccessEndpointId_ =
    ModifyVerifiedAccessEndpoint'
      { clientToken =
          Prelude.Nothing,
        description = Prelude.Nothing,
        dryRun = Prelude.Nothing,
        loadBalancerOptions = Prelude.Nothing,
        networkInterfaceOptions = Prelude.Nothing,
        verifiedAccessGroupId = Prelude.Nothing,
        verifiedAccessEndpointId =
          pVerifiedAccessEndpointId_
      }

-- | A unique, case-sensitive token that you provide to ensure idempotency of
-- your modification request. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/Run_Instance_Idempotency.html Ensuring Idempotency>.
modifyVerifiedAccessEndpoint_clientToken :: Lens.Lens' ModifyVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessEndpoint_clientToken = Lens.lens (\ModifyVerifiedAccessEndpoint' {clientToken} -> clientToken) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {clientToken = a} :: ModifyVerifiedAccessEndpoint)

-- | A description for the Amazon Web Services Verified Access endpoint.
modifyVerifiedAccessEndpoint_description :: Lens.Lens' ModifyVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessEndpoint_description = Lens.lens (\ModifyVerifiedAccessEndpoint' {description} -> description) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {description = a} :: ModifyVerifiedAccessEndpoint)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyVerifiedAccessEndpoint_dryRun :: Lens.Lens' ModifyVerifiedAccessEndpoint (Prelude.Maybe Prelude.Bool)
modifyVerifiedAccessEndpoint_dryRun = Lens.lens (\ModifyVerifiedAccessEndpoint' {dryRun} -> dryRun) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {dryRun = a} :: ModifyVerifiedAccessEndpoint)

-- | The load balancer details if creating the Amazon Web Services Verified
-- Access endpoint as @load-balancer@type.
modifyVerifiedAccessEndpoint_loadBalancerOptions :: Lens.Lens' ModifyVerifiedAccessEndpoint (Prelude.Maybe ModifyVerifiedAccessEndpointLoadBalancerOptions)
modifyVerifiedAccessEndpoint_loadBalancerOptions = Lens.lens (\ModifyVerifiedAccessEndpoint' {loadBalancerOptions} -> loadBalancerOptions) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {loadBalancerOptions = a} :: ModifyVerifiedAccessEndpoint)

-- | The network interface options.
modifyVerifiedAccessEndpoint_networkInterfaceOptions :: Lens.Lens' ModifyVerifiedAccessEndpoint (Prelude.Maybe ModifyVerifiedAccessEndpointEniOptions)
modifyVerifiedAccessEndpoint_networkInterfaceOptions = Lens.lens (\ModifyVerifiedAccessEndpoint' {networkInterfaceOptions} -> networkInterfaceOptions) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {networkInterfaceOptions = a} :: ModifyVerifiedAccessEndpoint)

-- | The ID of the Amazon Web Services Verified Access group.
modifyVerifiedAccessEndpoint_verifiedAccessGroupId :: Lens.Lens' ModifyVerifiedAccessEndpoint (Prelude.Maybe Prelude.Text)
modifyVerifiedAccessEndpoint_verifiedAccessGroupId = Lens.lens (\ModifyVerifiedAccessEndpoint' {verifiedAccessGroupId} -> verifiedAccessGroupId) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {verifiedAccessGroupId = a} :: ModifyVerifiedAccessEndpoint)

-- | The ID of the Amazon Web Services Verified Access endpoint.
modifyVerifiedAccessEndpoint_verifiedAccessEndpointId :: Lens.Lens' ModifyVerifiedAccessEndpoint Prelude.Text
modifyVerifiedAccessEndpoint_verifiedAccessEndpointId = Lens.lens (\ModifyVerifiedAccessEndpoint' {verifiedAccessEndpointId} -> verifiedAccessEndpointId) (\s@ModifyVerifiedAccessEndpoint' {} a -> s {verifiedAccessEndpointId = a} :: ModifyVerifiedAccessEndpoint)

instance Core.AWSRequest ModifyVerifiedAccessEndpoint where
  type
    AWSResponse ModifyVerifiedAccessEndpoint =
      ModifyVerifiedAccessEndpointResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyVerifiedAccessEndpointResponse'
            Prelude.<$> (x Data..@? "verifiedAccessEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyVerifiedAccessEndpoint
  where
  hashWithSalt _salt ModifyVerifiedAccessEndpoint' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` loadBalancerOptions
      `Prelude.hashWithSalt` networkInterfaceOptions
      `Prelude.hashWithSalt` verifiedAccessGroupId
      `Prelude.hashWithSalt` verifiedAccessEndpointId

instance Prelude.NFData ModifyVerifiedAccessEndpoint where
  rnf ModifyVerifiedAccessEndpoint' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf loadBalancerOptions
      `Prelude.seq` Prelude.rnf networkInterfaceOptions
      `Prelude.seq` Prelude.rnf verifiedAccessGroupId
      `Prelude.seq` Prelude.rnf verifiedAccessEndpointId

instance Data.ToHeaders ModifyVerifiedAccessEndpoint where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyVerifiedAccessEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyVerifiedAccessEndpoint where
  toQuery ModifyVerifiedAccessEndpoint' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyVerifiedAccessEndpoint" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2016-11-15" :: Prelude.ByteString),
        "ClientToken" Data.=: clientToken,
        "Description" Data.=: description,
        "DryRun" Data.=: dryRun,
        "LoadBalancerOptions" Data.=: loadBalancerOptions,
        "NetworkInterfaceOptions"
          Data.=: networkInterfaceOptions,
        "VerifiedAccessGroupId"
          Data.=: verifiedAccessGroupId,
        "VerifiedAccessEndpointId"
          Data.=: verifiedAccessEndpointId
      ]

-- | /See:/ 'newModifyVerifiedAccessEndpointResponse' smart constructor.
data ModifyVerifiedAccessEndpointResponse = ModifyVerifiedAccessEndpointResponse'
  { -- | The Amazon Web Services Verified Access endpoint details.
    verifiedAccessEndpoint :: Prelude.Maybe VerifiedAccessEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyVerifiedAccessEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verifiedAccessEndpoint', 'modifyVerifiedAccessEndpointResponse_verifiedAccessEndpoint' - The Amazon Web Services Verified Access endpoint details.
--
-- 'httpStatus', 'modifyVerifiedAccessEndpointResponse_httpStatus' - The response's http status code.
newModifyVerifiedAccessEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyVerifiedAccessEndpointResponse
newModifyVerifiedAccessEndpointResponse pHttpStatus_ =
  ModifyVerifiedAccessEndpointResponse'
    { verifiedAccessEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Web Services Verified Access endpoint details.
modifyVerifiedAccessEndpointResponse_verifiedAccessEndpoint :: Lens.Lens' ModifyVerifiedAccessEndpointResponse (Prelude.Maybe VerifiedAccessEndpoint)
modifyVerifiedAccessEndpointResponse_verifiedAccessEndpoint = Lens.lens (\ModifyVerifiedAccessEndpointResponse' {verifiedAccessEndpoint} -> verifiedAccessEndpoint) (\s@ModifyVerifiedAccessEndpointResponse' {} a -> s {verifiedAccessEndpoint = a} :: ModifyVerifiedAccessEndpointResponse)

-- | The response's http status code.
modifyVerifiedAccessEndpointResponse_httpStatus :: Lens.Lens' ModifyVerifiedAccessEndpointResponse Prelude.Int
modifyVerifiedAccessEndpointResponse_httpStatus = Lens.lens (\ModifyVerifiedAccessEndpointResponse' {httpStatus} -> httpStatus) (\s@ModifyVerifiedAccessEndpointResponse' {} a -> s {httpStatus = a} :: ModifyVerifiedAccessEndpointResponse)

instance
  Prelude.NFData
    ModifyVerifiedAccessEndpointResponse
  where
  rnf ModifyVerifiedAccessEndpointResponse' {..} =
    Prelude.rnf verifiedAccessEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
