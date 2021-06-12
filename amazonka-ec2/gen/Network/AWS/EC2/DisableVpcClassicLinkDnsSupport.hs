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
-- Module      : Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables ClassicLink DNS support for a VPC. If disabled, DNS hostnames
-- resolve to public IP addresses when addressed between a linked
-- EC2-Classic instance and instances in the VPC to which it\'s linked. For
-- more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You must specify a VPC ID in the request.
module Network.AWS.EC2.DisableVpcClassicLinkDnsSupport
  ( -- * Creating a Request
    DisableVpcClassicLinkDnsSupport (..),
    newDisableVpcClassicLinkDnsSupport,

    -- * Request Lenses
    disableVpcClassicLinkDnsSupport_vpcId,

    -- * Destructuring the Response
    DisableVpcClassicLinkDnsSupportResponse (..),
    newDisableVpcClassicLinkDnsSupportResponse,

    -- * Response Lenses
    disableVpcClassicLinkDnsSupportResponse_return,
    disableVpcClassicLinkDnsSupportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableVpcClassicLinkDnsSupport' smart constructor.
data DisableVpcClassicLinkDnsSupport = DisableVpcClassicLinkDnsSupport'
  { -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableVpcClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'disableVpcClassicLinkDnsSupport_vpcId' - The ID of the VPC.
newDisableVpcClassicLinkDnsSupport ::
  DisableVpcClassicLinkDnsSupport
newDisableVpcClassicLinkDnsSupport =
  DisableVpcClassicLinkDnsSupport'
    { vpcId =
        Core.Nothing
    }

-- | The ID of the VPC.
disableVpcClassicLinkDnsSupport_vpcId :: Lens.Lens' DisableVpcClassicLinkDnsSupport (Core.Maybe Core.Text)
disableVpcClassicLinkDnsSupport_vpcId = Lens.lens (\DisableVpcClassicLinkDnsSupport' {vpcId} -> vpcId) (\s@DisableVpcClassicLinkDnsSupport' {} a -> s {vpcId = a} :: DisableVpcClassicLinkDnsSupport)

instance
  Core.AWSRequest
    DisableVpcClassicLinkDnsSupport
  where
  type
    AWSResponse DisableVpcClassicLinkDnsSupport =
      DisableVpcClassicLinkDnsSupportResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DisableVpcClassicLinkDnsSupportResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DisableVpcClassicLinkDnsSupport

instance Core.NFData DisableVpcClassicLinkDnsSupport

instance
  Core.ToHeaders
    DisableVpcClassicLinkDnsSupport
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DisableVpcClassicLinkDnsSupport where
  toPath = Core.const "/"

instance Core.ToQuery DisableVpcClassicLinkDnsSupport where
  toQuery DisableVpcClassicLinkDnsSupport' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DisableVpcClassicLinkDnsSupport" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newDisableVpcClassicLinkDnsSupportResponse' smart constructor.
data DisableVpcClassicLinkDnsSupportResponse = DisableVpcClassicLinkDnsSupportResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisableVpcClassicLinkDnsSupportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'disableVpcClassicLinkDnsSupportResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'disableVpcClassicLinkDnsSupportResponse_httpStatus' - The response's http status code.
newDisableVpcClassicLinkDnsSupportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DisableVpcClassicLinkDnsSupportResponse
newDisableVpcClassicLinkDnsSupportResponse
  pHttpStatus_ =
    DisableVpcClassicLinkDnsSupportResponse'
      { return' =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
disableVpcClassicLinkDnsSupportResponse_return :: Lens.Lens' DisableVpcClassicLinkDnsSupportResponse (Core.Maybe Core.Bool)
disableVpcClassicLinkDnsSupportResponse_return = Lens.lens (\DisableVpcClassicLinkDnsSupportResponse' {return'} -> return') (\s@DisableVpcClassicLinkDnsSupportResponse' {} a -> s {return' = a} :: DisableVpcClassicLinkDnsSupportResponse)

-- | The response's http status code.
disableVpcClassicLinkDnsSupportResponse_httpStatus :: Lens.Lens' DisableVpcClassicLinkDnsSupportResponse Core.Int
disableVpcClassicLinkDnsSupportResponse_httpStatus = Lens.lens (\DisableVpcClassicLinkDnsSupportResponse' {httpStatus} -> httpStatus) (\s@DisableVpcClassicLinkDnsSupportResponse' {} a -> s {httpStatus = a} :: DisableVpcClassicLinkDnsSupportResponse)

instance
  Core.NFData
    DisableVpcClassicLinkDnsSupportResponse
