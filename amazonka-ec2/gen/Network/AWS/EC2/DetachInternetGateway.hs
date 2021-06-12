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
-- Module      : Network.AWS.EC2.DetachInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches an internet gateway from a VPC, disabling connectivity between
-- the internet and the VPC. The VPC must not contain any running instances
-- with Elastic IP addresses or public IPv4 addresses.
module Network.AWS.EC2.DetachInternetGateway
  ( -- * Creating a Request
    DetachInternetGateway (..),
    newDetachInternetGateway,

    -- * Request Lenses
    detachInternetGateway_dryRun,
    detachInternetGateway_internetGatewayId,
    detachInternetGateway_vpcId,

    -- * Destructuring the Response
    DetachInternetGatewayResponse (..),
    newDetachInternetGatewayResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDetachInternetGateway' smart constructor.
data DetachInternetGateway = DetachInternetGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the internet gateway.
    internetGatewayId :: Core.Text,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'detachInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'internetGatewayId', 'detachInternetGateway_internetGatewayId' - The ID of the internet gateway.
--
-- 'vpcId', 'detachInternetGateway_vpcId' - The ID of the VPC.
newDetachInternetGateway ::
  -- | 'internetGatewayId'
  Core.Text ->
  -- | 'vpcId'
  Core.Text ->
  DetachInternetGateway
newDetachInternetGateway pInternetGatewayId_ pVpcId_ =
  DetachInternetGateway'
    { dryRun = Core.Nothing,
      internetGatewayId = pInternetGatewayId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
detachInternetGateway_dryRun :: Lens.Lens' DetachInternetGateway (Core.Maybe Core.Bool)
detachInternetGateway_dryRun = Lens.lens (\DetachInternetGateway' {dryRun} -> dryRun) (\s@DetachInternetGateway' {} a -> s {dryRun = a} :: DetachInternetGateway)

-- | The ID of the internet gateway.
detachInternetGateway_internetGatewayId :: Lens.Lens' DetachInternetGateway Core.Text
detachInternetGateway_internetGatewayId = Lens.lens (\DetachInternetGateway' {internetGatewayId} -> internetGatewayId) (\s@DetachInternetGateway' {} a -> s {internetGatewayId = a} :: DetachInternetGateway)

-- | The ID of the VPC.
detachInternetGateway_vpcId :: Lens.Lens' DetachInternetGateway Core.Text
detachInternetGateway_vpcId = Lens.lens (\DetachInternetGateway' {vpcId} -> vpcId) (\s@DetachInternetGateway' {} a -> s {vpcId = a} :: DetachInternetGateway)

instance Core.AWSRequest DetachInternetGateway where
  type
    AWSResponse DetachInternetGateway =
      DetachInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull DetachInternetGatewayResponse'

instance Core.Hashable DetachInternetGateway

instance Core.NFData DetachInternetGateway

instance Core.ToHeaders DetachInternetGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DetachInternetGateway where
  toPath = Core.const "/"

instance Core.ToQuery DetachInternetGateway where
  toQuery DetachInternetGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DetachInternetGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InternetGatewayId" Core.=: internetGatewayId,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newDetachInternetGatewayResponse' smart constructor.
data DetachInternetGatewayResponse = DetachInternetGatewayResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DetachInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDetachInternetGatewayResponse ::
  DetachInternetGatewayResponse
newDetachInternetGatewayResponse =
  DetachInternetGatewayResponse'

instance Core.NFData DetachInternetGatewayResponse
