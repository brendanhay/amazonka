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
-- Module      : Network.AWS.AlexaBusiness.GetGatewayGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway group.
module Network.AWS.AlexaBusiness.GetGatewayGroup
  ( -- * Creating a Request
    GetGatewayGroup (..),
    newGetGatewayGroup,

    -- * Request Lenses
    getGatewayGroup_gatewayGroupArn,

    -- * Destructuring the Response
    GetGatewayGroupResponse (..),
    newGetGatewayGroupResponse,

    -- * Response Lenses
    getGatewayGroupResponse_gatewayGroup,
    getGatewayGroupResponse_httpStatus,
  )
where

import Network.AWS.AlexaBusiness.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetGatewayGroup' smart constructor.
data GetGatewayGroup = GetGatewayGroup'
  { -- | The ARN of the gateway group to get.
    gatewayGroupArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroupArn', 'getGatewayGroup_gatewayGroupArn' - The ARN of the gateway group to get.
newGetGatewayGroup ::
  -- | 'gatewayGroupArn'
  Core.Text ->
  GetGatewayGroup
newGetGatewayGroup pGatewayGroupArn_ =
  GetGatewayGroup'
    { gatewayGroupArn =
        pGatewayGroupArn_
    }

-- | The ARN of the gateway group to get.
getGatewayGroup_gatewayGroupArn :: Lens.Lens' GetGatewayGroup Core.Text
getGatewayGroup_gatewayGroupArn = Lens.lens (\GetGatewayGroup' {gatewayGroupArn} -> gatewayGroupArn) (\s@GetGatewayGroup' {} a -> s {gatewayGroupArn = a} :: GetGatewayGroup)

instance Core.AWSRequest GetGatewayGroup where
  type
    AWSResponse GetGatewayGroup =
      GetGatewayGroupResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayGroupResponse'
            Core.<$> (x Core..?> "GatewayGroup")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetGatewayGroup

instance Core.NFData GetGatewayGroup

instance Core.ToHeaders GetGatewayGroup where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetGatewayGroup" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetGatewayGroup where
  toJSON GetGatewayGroup' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("GatewayGroupArn" Core..= gatewayGroupArn)
          ]
      )

instance Core.ToPath GetGatewayGroup where
  toPath = Core.const "/"

instance Core.ToQuery GetGatewayGroup where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetGatewayGroupResponse' smart constructor.
data GetGatewayGroupResponse = GetGatewayGroupResponse'
  { gatewayGroup :: Core.Maybe GatewayGroup,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroup', 'getGatewayGroupResponse_gatewayGroup' - Undocumented member.
--
-- 'httpStatus', 'getGatewayGroupResponse_httpStatus' - The response's http status code.
newGetGatewayGroupResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetGatewayGroupResponse
newGetGatewayGroupResponse pHttpStatus_ =
  GetGatewayGroupResponse'
    { gatewayGroup =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getGatewayGroupResponse_gatewayGroup :: Lens.Lens' GetGatewayGroupResponse (Core.Maybe GatewayGroup)
getGatewayGroupResponse_gatewayGroup = Lens.lens (\GetGatewayGroupResponse' {gatewayGroup} -> gatewayGroup) (\s@GetGatewayGroupResponse' {} a -> s {gatewayGroup = a} :: GetGatewayGroupResponse)

-- | The response's http status code.
getGatewayGroupResponse_httpStatus :: Lens.Lens' GetGatewayGroupResponse Core.Int
getGatewayGroupResponse_httpStatus = Lens.lens (\GetGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@GetGatewayGroupResponse' {} a -> s {httpStatus = a} :: GetGatewayGroupResponse)

instance Core.NFData GetGatewayGroupResponse
