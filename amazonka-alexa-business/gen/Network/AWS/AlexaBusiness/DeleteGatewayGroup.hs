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
-- Module      : Network.AWS.AlexaBusiness.DeleteGatewayGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway group.
module Network.AWS.AlexaBusiness.DeleteGatewayGroup
  ( -- * Creating a Request
    DeleteGatewayGroup (..),
    newDeleteGatewayGroup,

    -- * Request Lenses
    deleteGatewayGroup_gatewayGroupArn,

    -- * Destructuring the Response
    DeleteGatewayGroupResponse (..),
    newDeleteGatewayGroupResponse,

    -- * Response Lenses
    deleteGatewayGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteGatewayGroup' smart constructor.
data DeleteGatewayGroup = DeleteGatewayGroup'
  { -- | The ARN of the gateway group to delete.
    gatewayGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroupArn', 'deleteGatewayGroup_gatewayGroupArn' - The ARN of the gateway group to delete.
newDeleteGatewayGroup ::
  -- | 'gatewayGroupArn'
  Core.Text ->
  DeleteGatewayGroup
newDeleteGatewayGroup pGatewayGroupArn_ =
  DeleteGatewayGroup'
    { gatewayGroupArn =
        pGatewayGroupArn_
    }

-- | The ARN of the gateway group to delete.
deleteGatewayGroup_gatewayGroupArn :: Lens.Lens' DeleteGatewayGroup Core.Text
deleteGatewayGroup_gatewayGroupArn = Lens.lens (\DeleteGatewayGroup' {gatewayGroupArn} -> gatewayGroupArn) (\s@DeleteGatewayGroup' {} a -> s {gatewayGroupArn = a} :: DeleteGatewayGroup)

instance Core.AWSRequest DeleteGatewayGroup where
  type
    AWSResponse DeleteGatewayGroup =
      DeleteGatewayGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteGatewayGroupResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteGatewayGroup

instance Core.NFData DeleteGatewayGroup

instance Core.ToHeaders DeleteGatewayGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.DeleteGatewayGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteGatewayGroup where
  toJSON DeleteGatewayGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("GatewayGroupArn" Core..= gatewayGroupArn)
          ]
      )

instance Core.ToPath DeleteGatewayGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteGatewayGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteGatewayGroupResponse' smart constructor.
data DeleteGatewayGroupResponse = DeleteGatewayGroupResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteGatewayGroupResponse_httpStatus' - The response's http status code.
newDeleteGatewayGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteGatewayGroupResponse
newDeleteGatewayGroupResponse pHttpStatus_ =
  DeleteGatewayGroupResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteGatewayGroupResponse_httpStatus :: Lens.Lens' DeleteGatewayGroupResponse Core.Int
deleteGatewayGroupResponse_httpStatus = Lens.lens (\DeleteGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@DeleteGatewayGroupResponse' {} a -> s {httpStatus = a} :: DeleteGatewayGroupResponse)

instance Core.NFData DeleteGatewayGroupResponse
