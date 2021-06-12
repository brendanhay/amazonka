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
-- Module      : Network.AWS.EC2.ModifyTransitGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified transit gateway. When you modify a transit
-- gateway, the modified options are applied to new transit gateway
-- attachments only. Your existing transit gateway attachments are not
-- modified.
module Network.AWS.EC2.ModifyTransitGateway
  ( -- * Creating a Request
    ModifyTransitGateway (..),
    newModifyTransitGateway,

    -- * Request Lenses
    modifyTransitGateway_dryRun,
    modifyTransitGateway_options,
    modifyTransitGateway_description,
    modifyTransitGateway_transitGatewayId,

    -- * Destructuring the Response
    ModifyTransitGatewayResponse (..),
    newModifyTransitGatewayResponse,

    -- * Response Lenses
    modifyTransitGatewayResponse_transitGateway,
    modifyTransitGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyTransitGateway' smart constructor.
data ModifyTransitGateway = ModifyTransitGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The options to modify.
    options :: Core.Maybe ModifyTransitGatewayOptions,
    -- | The description for the transit gateway.
    description :: Core.Maybe Core.Text,
    -- | The ID of the transit gateway.
    transitGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTransitGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'modifyTransitGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'options', 'modifyTransitGateway_options' - The options to modify.
--
-- 'description', 'modifyTransitGateway_description' - The description for the transit gateway.
--
-- 'transitGatewayId', 'modifyTransitGateway_transitGatewayId' - The ID of the transit gateway.
newModifyTransitGateway ::
  -- | 'transitGatewayId'
  Core.Text ->
  ModifyTransitGateway
newModifyTransitGateway pTransitGatewayId_ =
  ModifyTransitGateway'
    { dryRun = Core.Nothing,
      options = Core.Nothing,
      description = Core.Nothing,
      transitGatewayId = pTransitGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyTransitGateway_dryRun :: Lens.Lens' ModifyTransitGateway (Core.Maybe Core.Bool)
modifyTransitGateway_dryRun = Lens.lens (\ModifyTransitGateway' {dryRun} -> dryRun) (\s@ModifyTransitGateway' {} a -> s {dryRun = a} :: ModifyTransitGateway)

-- | The options to modify.
modifyTransitGateway_options :: Lens.Lens' ModifyTransitGateway (Core.Maybe ModifyTransitGatewayOptions)
modifyTransitGateway_options = Lens.lens (\ModifyTransitGateway' {options} -> options) (\s@ModifyTransitGateway' {} a -> s {options = a} :: ModifyTransitGateway)

-- | The description for the transit gateway.
modifyTransitGateway_description :: Lens.Lens' ModifyTransitGateway (Core.Maybe Core.Text)
modifyTransitGateway_description = Lens.lens (\ModifyTransitGateway' {description} -> description) (\s@ModifyTransitGateway' {} a -> s {description = a} :: ModifyTransitGateway)

-- | The ID of the transit gateway.
modifyTransitGateway_transitGatewayId :: Lens.Lens' ModifyTransitGateway Core.Text
modifyTransitGateway_transitGatewayId = Lens.lens (\ModifyTransitGateway' {transitGatewayId} -> transitGatewayId) (\s@ModifyTransitGateway' {} a -> s {transitGatewayId = a} :: ModifyTransitGateway)

instance Core.AWSRequest ModifyTransitGateway where
  type
    AWSResponse ModifyTransitGateway =
      ModifyTransitGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyTransitGatewayResponse'
            Core.<$> (x Core..@? "transitGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ModifyTransitGateway

instance Core.NFData ModifyTransitGateway

instance Core.ToHeaders ModifyTransitGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ModifyTransitGateway where
  toPath = Core.const "/"

instance Core.ToQuery ModifyTransitGateway where
  toQuery ModifyTransitGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ModifyTransitGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "Options" Core.=: options,
        "Description" Core.=: description,
        "TransitGatewayId" Core.=: transitGatewayId
      ]

-- | /See:/ 'newModifyTransitGatewayResponse' smart constructor.
data ModifyTransitGatewayResponse = ModifyTransitGatewayResponse'
  { transitGateway :: Core.Maybe TransitGateway,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ModifyTransitGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGateway', 'modifyTransitGatewayResponse_transitGateway' - Undocumented member.
--
-- 'httpStatus', 'modifyTransitGatewayResponse_httpStatus' - The response's http status code.
newModifyTransitGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ModifyTransitGatewayResponse
newModifyTransitGatewayResponse pHttpStatus_ =
  ModifyTransitGatewayResponse'
    { transitGateway =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
modifyTransitGatewayResponse_transitGateway :: Lens.Lens' ModifyTransitGatewayResponse (Core.Maybe TransitGateway)
modifyTransitGatewayResponse_transitGateway = Lens.lens (\ModifyTransitGatewayResponse' {transitGateway} -> transitGateway) (\s@ModifyTransitGatewayResponse' {} a -> s {transitGateway = a} :: ModifyTransitGatewayResponse)

-- | The response's http status code.
modifyTransitGatewayResponse_httpStatus :: Lens.Lens' ModifyTransitGatewayResponse Core.Int
modifyTransitGatewayResponse_httpStatus = Lens.lens (\ModifyTransitGatewayResponse' {httpStatus} -> httpStatus) (\s@ModifyTransitGatewayResponse' {} a -> s {httpStatus = a} :: ModifyTransitGatewayResponse)

instance Core.NFData ModifyTransitGatewayResponse
