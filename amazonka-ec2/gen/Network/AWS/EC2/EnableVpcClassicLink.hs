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
-- Module      : Network.AWS.EC2.EnableVpcClassicLink
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC for ClassicLink. You can then link EC2-Classic instances
-- to your ClassicLink-enabled VPC to allow communication over private IP
-- addresses. You cannot enable your VPC for ClassicLink if any of your VPC
-- route tables have existing routes for address ranges within the
-- @10.0.0.0\/8@ IP address range, excluding local routes for VPCs in the
-- @10.0.0.0\/16@ and @10.1.0.0\/16@ IP address ranges. For more
-- information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
module Network.AWS.EC2.EnableVpcClassicLink
  ( -- * Creating a Request
    EnableVpcClassicLink (..),
    newEnableVpcClassicLink,

    -- * Request Lenses
    enableVpcClassicLink_dryRun,
    enableVpcClassicLink_vpcId,

    -- * Destructuring the Response
    EnableVpcClassicLinkResponse (..),
    newEnableVpcClassicLinkResponse,

    -- * Response Lenses
    enableVpcClassicLinkResponse_return,
    enableVpcClassicLinkResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableVpcClassicLink' smart constructor.
data EnableVpcClassicLink = EnableVpcClassicLink'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableVpcClassicLink' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'enableVpcClassicLink_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcId', 'enableVpcClassicLink_vpcId' - The ID of the VPC.
newEnableVpcClassicLink ::
  -- | 'vpcId'
  Core.Text ->
  EnableVpcClassicLink
newEnableVpcClassicLink pVpcId_ =
  EnableVpcClassicLink'
    { dryRun = Core.Nothing,
      vpcId = pVpcId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
enableVpcClassicLink_dryRun :: Lens.Lens' EnableVpcClassicLink (Core.Maybe Core.Bool)
enableVpcClassicLink_dryRun = Lens.lens (\EnableVpcClassicLink' {dryRun} -> dryRun) (\s@EnableVpcClassicLink' {} a -> s {dryRun = a} :: EnableVpcClassicLink)

-- | The ID of the VPC.
enableVpcClassicLink_vpcId :: Lens.Lens' EnableVpcClassicLink Core.Text
enableVpcClassicLink_vpcId = Lens.lens (\EnableVpcClassicLink' {vpcId} -> vpcId) (\s@EnableVpcClassicLink' {} a -> s {vpcId = a} :: EnableVpcClassicLink)

instance Core.AWSRequest EnableVpcClassicLink where
  type
    AWSResponse EnableVpcClassicLink =
      EnableVpcClassicLinkResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          EnableVpcClassicLinkResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableVpcClassicLink

instance Core.NFData EnableVpcClassicLink

instance Core.ToHeaders EnableVpcClassicLink where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EnableVpcClassicLink where
  toPath = Core.const "/"

instance Core.ToQuery EnableVpcClassicLink where
  toQuery EnableVpcClassicLink' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("EnableVpcClassicLink" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newEnableVpcClassicLinkResponse' smart constructor.
data EnableVpcClassicLinkResponse = EnableVpcClassicLinkResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableVpcClassicLinkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'enableVpcClassicLinkResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'enableVpcClassicLinkResponse_httpStatus' - The response's http status code.
newEnableVpcClassicLinkResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableVpcClassicLinkResponse
newEnableVpcClassicLinkResponse pHttpStatus_ =
  EnableVpcClassicLinkResponse'
    { return' =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
enableVpcClassicLinkResponse_return :: Lens.Lens' EnableVpcClassicLinkResponse (Core.Maybe Core.Bool)
enableVpcClassicLinkResponse_return = Lens.lens (\EnableVpcClassicLinkResponse' {return'} -> return') (\s@EnableVpcClassicLinkResponse' {} a -> s {return' = a} :: EnableVpcClassicLinkResponse)

-- | The response's http status code.
enableVpcClassicLinkResponse_httpStatus :: Lens.Lens' EnableVpcClassicLinkResponse Core.Int
enableVpcClassicLinkResponse_httpStatus = Lens.lens (\EnableVpcClassicLinkResponse' {httpStatus} -> httpStatus) (\s@EnableVpcClassicLinkResponse' {} a -> s {httpStatus = a} :: EnableVpcClassicLinkResponse)

instance Core.NFData EnableVpcClassicLinkResponse
