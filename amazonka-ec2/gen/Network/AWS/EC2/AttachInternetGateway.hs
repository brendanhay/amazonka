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
-- Module      : Network.AWS.EC2.AttachInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an internet gateway or a virtual private gateway to a VPC,
-- enabling connectivity between the internet and the VPC. For more
-- information about your VPC and internet gateway, see the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide>.
module Network.AWS.EC2.AttachInternetGateway
  ( -- * Creating a Request
    AttachInternetGateway (..),
    newAttachInternetGateway,

    -- * Request Lenses
    attachInternetGateway_dryRun,
    attachInternetGateway_internetGatewayId,
    attachInternetGateway_vpcId,

    -- * Destructuring the Response
    AttachInternetGatewayResponse (..),
    newAttachInternetGatewayResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAttachInternetGateway' smart constructor.
data AttachInternetGateway = AttachInternetGateway'
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
-- Create a value of 'AttachInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'attachInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'internetGatewayId', 'attachInternetGateway_internetGatewayId' - The ID of the internet gateway.
--
-- 'vpcId', 'attachInternetGateway_vpcId' - The ID of the VPC.
newAttachInternetGateway ::
  -- | 'internetGatewayId'
  Core.Text ->
  -- | 'vpcId'
  Core.Text ->
  AttachInternetGateway
newAttachInternetGateway pInternetGatewayId_ pVpcId_ =
  AttachInternetGateway'
    { dryRun = Core.Nothing,
      internetGatewayId = pInternetGatewayId_,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
attachInternetGateway_dryRun :: Lens.Lens' AttachInternetGateway (Core.Maybe Core.Bool)
attachInternetGateway_dryRun = Lens.lens (\AttachInternetGateway' {dryRun} -> dryRun) (\s@AttachInternetGateway' {} a -> s {dryRun = a} :: AttachInternetGateway)

-- | The ID of the internet gateway.
attachInternetGateway_internetGatewayId :: Lens.Lens' AttachInternetGateway Core.Text
attachInternetGateway_internetGatewayId = Lens.lens (\AttachInternetGateway' {internetGatewayId} -> internetGatewayId) (\s@AttachInternetGateway' {} a -> s {internetGatewayId = a} :: AttachInternetGateway)

-- | The ID of the VPC.
attachInternetGateway_vpcId :: Lens.Lens' AttachInternetGateway Core.Text
attachInternetGateway_vpcId = Lens.lens (\AttachInternetGateway' {vpcId} -> vpcId) (\s@AttachInternetGateway' {} a -> s {vpcId = a} :: AttachInternetGateway)

instance Core.AWSRequest AttachInternetGateway where
  type
    AWSResponse AttachInternetGateway =
      AttachInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull AttachInternetGatewayResponse'

instance Core.Hashable AttachInternetGateway

instance Core.NFData AttachInternetGateway

instance Core.ToHeaders AttachInternetGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AttachInternetGateway where
  toPath = Core.const "/"

instance Core.ToQuery AttachInternetGateway where
  toQuery AttachInternetGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AttachInternetGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "InternetGatewayId" Core.=: internetGatewayId,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newAttachInternetGatewayResponse' smart constructor.
data AttachInternetGatewayResponse = AttachInternetGatewayResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AttachInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAttachInternetGatewayResponse ::
  AttachInternetGatewayResponse
newAttachInternetGatewayResponse =
  AttachInternetGatewayResponse'

instance Core.NFData AttachInternetGatewayResponse
