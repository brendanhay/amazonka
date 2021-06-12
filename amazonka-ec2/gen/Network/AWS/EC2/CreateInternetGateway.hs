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
-- Module      : Network.AWS.EC2.CreateInternetGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an internet gateway for use with a VPC. After creating the
-- internet gateway, you attach it to a VPC using AttachInternetGateway.
--
-- For more information about your VPC and internet gateway, see the
-- <https://docs.aws.amazon.com/vpc/latest/userguide/ Amazon Virtual Private Cloud User Guide>.
module Network.AWS.EC2.CreateInternetGateway
  ( -- * Creating a Request
    CreateInternetGateway (..),
    newCreateInternetGateway,

    -- * Request Lenses
    createInternetGateway_tagSpecifications,
    createInternetGateway_dryRun,

    -- * Destructuring the Response
    CreateInternetGatewayResponse (..),
    newCreateInternetGatewayResponse,

    -- * Response Lenses
    createInternetGatewayResponse_internetGateway,
    createInternetGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateInternetGateway' smart constructor.
data CreateInternetGateway = CreateInternetGateway'
  { -- | The tags to assign to the internet gateway.
    tagSpecifications :: Core.Maybe [TagSpecification],
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInternetGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagSpecifications', 'createInternetGateway_tagSpecifications' - The tags to assign to the internet gateway.
--
-- 'dryRun', 'createInternetGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newCreateInternetGateway ::
  CreateInternetGateway
newCreateInternetGateway =
  CreateInternetGateway'
    { tagSpecifications =
        Core.Nothing,
      dryRun = Core.Nothing
    }

-- | The tags to assign to the internet gateway.
createInternetGateway_tagSpecifications :: Lens.Lens' CreateInternetGateway (Core.Maybe [TagSpecification])
createInternetGateway_tagSpecifications = Lens.lens (\CreateInternetGateway' {tagSpecifications} -> tagSpecifications) (\s@CreateInternetGateway' {} a -> s {tagSpecifications = a} :: CreateInternetGateway) Core.. Lens.mapping Lens._Coerce

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
createInternetGateway_dryRun :: Lens.Lens' CreateInternetGateway (Core.Maybe Core.Bool)
createInternetGateway_dryRun = Lens.lens (\CreateInternetGateway' {dryRun} -> dryRun) (\s@CreateInternetGateway' {} a -> s {dryRun = a} :: CreateInternetGateway)

instance Core.AWSRequest CreateInternetGateway where
  type
    AWSResponse CreateInternetGateway =
      CreateInternetGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CreateInternetGatewayResponse'
            Core.<$> (x Core..@? "internetGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateInternetGateway

instance Core.NFData CreateInternetGateway

instance Core.ToHeaders CreateInternetGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CreateInternetGateway where
  toPath = Core.const "/"

instance Core.ToQuery CreateInternetGateway where
  toQuery CreateInternetGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CreateInternetGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        Core.toQuery
          ( Core.toQueryList "TagSpecification"
              Core.<$> tagSpecifications
          ),
        "DryRun" Core.=: dryRun
      ]

-- | /See:/ 'newCreateInternetGatewayResponse' smart constructor.
data CreateInternetGatewayResponse = CreateInternetGatewayResponse'
  { -- | Information about the internet gateway.
    internetGateway :: Core.Maybe InternetGateway,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateInternetGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'internetGateway', 'createInternetGatewayResponse_internetGateway' - Information about the internet gateway.
--
-- 'httpStatus', 'createInternetGatewayResponse_httpStatus' - The response's http status code.
newCreateInternetGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateInternetGatewayResponse
newCreateInternetGatewayResponse pHttpStatus_ =
  CreateInternetGatewayResponse'
    { internetGateway =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the internet gateway.
createInternetGatewayResponse_internetGateway :: Lens.Lens' CreateInternetGatewayResponse (Core.Maybe InternetGateway)
createInternetGatewayResponse_internetGateway = Lens.lens (\CreateInternetGatewayResponse' {internetGateway} -> internetGateway) (\s@CreateInternetGatewayResponse' {} a -> s {internetGateway = a} :: CreateInternetGatewayResponse)

-- | The response's http status code.
createInternetGatewayResponse_httpStatus :: Lens.Lens' CreateInternetGatewayResponse Core.Int
createInternetGatewayResponse_httpStatus = Lens.lens (\CreateInternetGatewayResponse' {httpStatus} -> httpStatus) (\s@CreateInternetGatewayResponse' {} a -> s {httpStatus = a} :: CreateInternetGatewayResponse)

instance Core.NFData CreateInternetGatewayResponse
