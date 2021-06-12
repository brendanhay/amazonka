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
-- Module      : Network.AWS.AlexaBusiness.UpdateGatewayGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the details of a gateway group. If any optional field is not
-- provided, the existing corresponding value is left unmodified.
module Network.AWS.AlexaBusiness.UpdateGatewayGroup
  ( -- * Creating a Request
    UpdateGatewayGroup (..),
    newUpdateGatewayGroup,

    -- * Request Lenses
    updateGatewayGroup_name,
    updateGatewayGroup_description,
    updateGatewayGroup_gatewayGroupArn,

    -- * Destructuring the Response
    UpdateGatewayGroupResponse (..),
    newUpdateGatewayGroupResponse,

    -- * Response Lenses
    updateGatewayGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGatewayGroup' smart constructor.
data UpdateGatewayGroup = UpdateGatewayGroup'
  { -- | The updated name of the gateway group.
    name :: Core.Maybe Core.Text,
    -- | The updated description of the gateway group.
    description :: Core.Maybe Core.Text,
    -- | The ARN of the gateway group to update.
    gatewayGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateGatewayGroup_name' - The updated name of the gateway group.
--
-- 'description', 'updateGatewayGroup_description' - The updated description of the gateway group.
--
-- 'gatewayGroupArn', 'updateGatewayGroup_gatewayGroupArn' - The ARN of the gateway group to update.
newUpdateGatewayGroup ::
  -- | 'gatewayGroupArn'
  Core.Text ->
  UpdateGatewayGroup
newUpdateGatewayGroup pGatewayGroupArn_ =
  UpdateGatewayGroup'
    { name = Core.Nothing,
      description = Core.Nothing,
      gatewayGroupArn = pGatewayGroupArn_
    }

-- | The updated name of the gateway group.
updateGatewayGroup_name :: Lens.Lens' UpdateGatewayGroup (Core.Maybe Core.Text)
updateGatewayGroup_name = Lens.lens (\UpdateGatewayGroup' {name} -> name) (\s@UpdateGatewayGroup' {} a -> s {name = a} :: UpdateGatewayGroup)

-- | The updated description of the gateway group.
updateGatewayGroup_description :: Lens.Lens' UpdateGatewayGroup (Core.Maybe Core.Text)
updateGatewayGroup_description = Lens.lens (\UpdateGatewayGroup' {description} -> description) (\s@UpdateGatewayGroup' {} a -> s {description = a} :: UpdateGatewayGroup)

-- | The ARN of the gateway group to update.
updateGatewayGroup_gatewayGroupArn :: Lens.Lens' UpdateGatewayGroup Core.Text
updateGatewayGroup_gatewayGroupArn = Lens.lens (\UpdateGatewayGroup' {gatewayGroupArn} -> gatewayGroupArn) (\s@UpdateGatewayGroup' {} a -> s {gatewayGroupArn = a} :: UpdateGatewayGroup)

instance Core.AWSRequest UpdateGatewayGroup where
  type
    AWSResponse UpdateGatewayGroup =
      UpdateGatewayGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateGatewayGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateGatewayGroup

instance Core.NFData UpdateGatewayGroup

instance Core.ToHeaders UpdateGatewayGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.UpdateGatewayGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateGatewayGroup where
  toJSON UpdateGatewayGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Description" Core..=) Core.<$> description,
            Core.Just
              ("GatewayGroupArn" Core..= gatewayGroupArn)
          ]
      )

instance Core.ToPath UpdateGatewayGroup where
  toPath = Core.const "/"

instance Core.ToQuery UpdateGatewayGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateGatewayGroupResponse' smart constructor.
data UpdateGatewayGroupResponse = UpdateGatewayGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateGatewayGroupResponse_httpStatus' - The response's http status code.
newUpdateGatewayGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateGatewayGroupResponse
newUpdateGatewayGroupResponse pHttpStatus_ =
  UpdateGatewayGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateGatewayGroupResponse_httpStatus :: Lens.Lens' UpdateGatewayGroupResponse Core.Int
updateGatewayGroupResponse_httpStatus = Lens.lens (\UpdateGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@UpdateGatewayGroupResponse' {} a -> s {httpStatus = a} :: UpdateGatewayGroupResponse)

instance Core.NFData UpdateGatewayGroupResponse
