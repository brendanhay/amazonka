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
-- Module      : Network.AWS.EC2.EnableVpcClassicLinkDnsSupport
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables a VPC to support DNS hostname resolution for ClassicLink. If
-- enabled, the DNS hostname of a linked EC2-Classic instance resolves to
-- its private IP address when addressed from an instance in the VPC to
-- which it\'s linked. Similarly, the DNS hostname of an instance in a VPC
-- resolves to its private IP address when addressed from a linked
-- EC2-Classic instance. For more information, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/vpc-classiclink.html ClassicLink>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- You must specify a VPC ID in the request.
module Network.AWS.EC2.EnableVpcClassicLinkDnsSupport
  ( -- * Creating a Request
    EnableVpcClassicLinkDnsSupport (..),
    newEnableVpcClassicLinkDnsSupport,

    -- * Request Lenses
    enableVpcClassicLinkDnsSupport_vpcId,

    -- * Destructuring the Response
    EnableVpcClassicLinkDnsSupportResponse (..),
    newEnableVpcClassicLinkDnsSupportResponse,

    -- * Response Lenses
    enableVpcClassicLinkDnsSupportResponse_return,
    enableVpcClassicLinkDnsSupportResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableVpcClassicLinkDnsSupport' smart constructor.
data EnableVpcClassicLinkDnsSupport = EnableVpcClassicLinkDnsSupport'
  { -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableVpcClassicLinkDnsSupport' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcId', 'enableVpcClassicLinkDnsSupport_vpcId' - The ID of the VPC.
newEnableVpcClassicLinkDnsSupport ::
  EnableVpcClassicLinkDnsSupport
newEnableVpcClassicLinkDnsSupport =
  EnableVpcClassicLinkDnsSupport'
    { vpcId =
        Core.Nothing
    }

-- | The ID of the VPC.
enableVpcClassicLinkDnsSupport_vpcId :: Lens.Lens' EnableVpcClassicLinkDnsSupport (Core.Maybe Core.Text)
enableVpcClassicLinkDnsSupport_vpcId = Lens.lens (\EnableVpcClassicLinkDnsSupport' {vpcId} -> vpcId) (\s@EnableVpcClassicLinkDnsSupport' {} a -> s {vpcId = a} :: EnableVpcClassicLinkDnsSupport)

instance
  Core.AWSRequest
    EnableVpcClassicLinkDnsSupport
  where
  type
    AWSResponse EnableVpcClassicLinkDnsSupport =
      EnableVpcClassicLinkDnsSupportResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          EnableVpcClassicLinkDnsSupportResponse'
            Core.<$> (x Core..@? "return")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable EnableVpcClassicLinkDnsSupport

instance Core.NFData EnableVpcClassicLinkDnsSupport

instance
  Core.ToHeaders
    EnableVpcClassicLinkDnsSupport
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath EnableVpcClassicLinkDnsSupport where
  toPath = Core.const "/"

instance Core.ToQuery EnableVpcClassicLinkDnsSupport where
  toQuery EnableVpcClassicLinkDnsSupport' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "EnableVpcClassicLinkDnsSupport" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "VpcId" Core.=: vpcId
      ]

-- | /See:/ 'newEnableVpcClassicLinkDnsSupportResponse' smart constructor.
data EnableVpcClassicLinkDnsSupportResponse = EnableVpcClassicLinkDnsSupportResponse'
  { -- | Returns @true@ if the request succeeds; otherwise, it returns an error.
    return' :: Core.Maybe Core.Bool,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EnableVpcClassicLinkDnsSupportResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'return'', 'enableVpcClassicLinkDnsSupportResponse_return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- 'httpStatus', 'enableVpcClassicLinkDnsSupportResponse_httpStatus' - The response's http status code.
newEnableVpcClassicLinkDnsSupportResponse ::
  -- | 'httpStatus'
  Core.Int ->
  EnableVpcClassicLinkDnsSupportResponse
newEnableVpcClassicLinkDnsSupportResponse
  pHttpStatus_ =
    EnableVpcClassicLinkDnsSupportResponse'
      { return' =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
enableVpcClassicLinkDnsSupportResponse_return :: Lens.Lens' EnableVpcClassicLinkDnsSupportResponse (Core.Maybe Core.Bool)
enableVpcClassicLinkDnsSupportResponse_return = Lens.lens (\EnableVpcClassicLinkDnsSupportResponse' {return'} -> return') (\s@EnableVpcClassicLinkDnsSupportResponse' {} a -> s {return' = a} :: EnableVpcClassicLinkDnsSupportResponse)

-- | The response's http status code.
enableVpcClassicLinkDnsSupportResponse_httpStatus :: Lens.Lens' EnableVpcClassicLinkDnsSupportResponse Core.Int
enableVpcClassicLinkDnsSupportResponse_httpStatus = Lens.lens (\EnableVpcClassicLinkDnsSupportResponse' {httpStatus} -> httpStatus) (\s@EnableVpcClassicLinkDnsSupportResponse' {} a -> s {httpStatus = a} :: EnableVpcClassicLinkDnsSupportResponse)

instance
  Core.NFData
    EnableVpcClassicLinkDnsSupportResponse
