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
-- Module      : Network.AWS.EC2.CancelSpotInstanceRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels one or more Spot Instance requests.
--
-- Canceling a Spot Instance request does not terminate running Spot
-- Instances associated with the request.
module Network.AWS.EC2.CancelSpotInstanceRequests
  ( -- * Creating a Request
    CancelSpotInstanceRequests (..),
    newCancelSpotInstanceRequests,

    -- * Request Lenses
    cancelSpotInstanceRequests_dryRun,
    cancelSpotInstanceRequests_spotInstanceRequestIds,

    -- * Destructuring the Response
    CancelSpotInstanceRequestsResponse (..),
    newCancelSpotInstanceRequestsResponse,

    -- * Response Lenses
    cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests,
    cancelSpotInstanceRequestsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelSpotInstanceRequests.
--
-- /See:/ 'newCancelSpotInstanceRequests' smart constructor.
data CancelSpotInstanceRequests = CancelSpotInstanceRequests'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more Spot Instance request IDs.
    spotInstanceRequestIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelSpotInstanceRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelSpotInstanceRequests_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'spotInstanceRequestIds', 'cancelSpotInstanceRequests_spotInstanceRequestIds' - One or more Spot Instance request IDs.
newCancelSpotInstanceRequests ::
  CancelSpotInstanceRequests
newCancelSpotInstanceRequests =
  CancelSpotInstanceRequests'
    { dryRun = Core.Nothing,
      spotInstanceRequestIds = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelSpotInstanceRequests_dryRun :: Lens.Lens' CancelSpotInstanceRequests (Core.Maybe Core.Bool)
cancelSpotInstanceRequests_dryRun = Lens.lens (\CancelSpotInstanceRequests' {dryRun} -> dryRun) (\s@CancelSpotInstanceRequests' {} a -> s {dryRun = a} :: CancelSpotInstanceRequests)

-- | One or more Spot Instance request IDs.
cancelSpotInstanceRequests_spotInstanceRequestIds :: Lens.Lens' CancelSpotInstanceRequests [Core.Text]
cancelSpotInstanceRequests_spotInstanceRequestIds = Lens.lens (\CancelSpotInstanceRequests' {spotInstanceRequestIds} -> spotInstanceRequestIds) (\s@CancelSpotInstanceRequests' {} a -> s {spotInstanceRequestIds = a} :: CancelSpotInstanceRequests) Core.. Lens._Coerce

instance Core.AWSRequest CancelSpotInstanceRequests where
  type
    AWSResponse CancelSpotInstanceRequests =
      CancelSpotInstanceRequestsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelSpotInstanceRequestsResponse'
            Core.<$> ( x Core..@? "spotInstanceRequestSet"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CancelSpotInstanceRequests

instance Core.NFData CancelSpotInstanceRequests

instance Core.ToHeaders CancelSpotInstanceRequests where
  toHeaders = Core.const Core.mempty

instance Core.ToPath CancelSpotInstanceRequests where
  toPath = Core.const "/"

instance Core.ToQuery CancelSpotInstanceRequests where
  toQuery CancelSpotInstanceRequests' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("CancelSpotInstanceRequests" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList
          "SpotInstanceRequestId"
          spotInstanceRequestIds
      ]

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'newCancelSpotInstanceRequestsResponse' smart constructor.
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
  { -- | One or more Spot Instance requests.
    cancelledSpotInstanceRequests :: Core.Maybe [CancelledSpotInstanceRequest],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CancelSpotInstanceRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelledSpotInstanceRequests', 'cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests' - One or more Spot Instance requests.
--
-- 'httpStatus', 'cancelSpotInstanceRequestsResponse_httpStatus' - The response's http status code.
newCancelSpotInstanceRequestsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CancelSpotInstanceRequestsResponse
newCancelSpotInstanceRequestsResponse pHttpStatus_ =
  CancelSpotInstanceRequestsResponse'
    { cancelledSpotInstanceRequests =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One or more Spot Instance requests.
cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests :: Lens.Lens' CancelSpotInstanceRequestsResponse (Core.Maybe [CancelledSpotInstanceRequest])
cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests = Lens.lens (\CancelSpotInstanceRequestsResponse' {cancelledSpotInstanceRequests} -> cancelledSpotInstanceRequests) (\s@CancelSpotInstanceRequestsResponse' {} a -> s {cancelledSpotInstanceRequests = a} :: CancelSpotInstanceRequestsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
cancelSpotInstanceRequestsResponse_httpStatus :: Lens.Lens' CancelSpotInstanceRequestsResponse Core.Int
cancelSpotInstanceRequestsResponse_httpStatus = Lens.lens (\CancelSpotInstanceRequestsResponse' {httpStatus} -> httpStatus) (\s@CancelSpotInstanceRequestsResponse' {} a -> s {httpStatus = a} :: CancelSpotInstanceRequestsResponse)

instance
  Core.NFData
    CancelSpotInstanceRequestsResponse
