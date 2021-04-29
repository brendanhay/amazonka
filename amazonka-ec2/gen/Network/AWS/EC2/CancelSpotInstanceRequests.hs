{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | One or more Spot Instance request IDs.
    spotInstanceRequestIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { dryRun =
        Prelude.Nothing,
      spotInstanceRequestIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelSpotInstanceRequests_dryRun :: Lens.Lens' CancelSpotInstanceRequests (Prelude.Maybe Prelude.Bool)
cancelSpotInstanceRequests_dryRun = Lens.lens (\CancelSpotInstanceRequests' {dryRun} -> dryRun) (\s@CancelSpotInstanceRequests' {} a -> s {dryRun = a} :: CancelSpotInstanceRequests)

-- | One or more Spot Instance request IDs.
cancelSpotInstanceRequests_spotInstanceRequestIds :: Lens.Lens' CancelSpotInstanceRequests [Prelude.Text]
cancelSpotInstanceRequests_spotInstanceRequestIds = Lens.lens (\CancelSpotInstanceRequests' {spotInstanceRequestIds} -> spotInstanceRequestIds) (\s@CancelSpotInstanceRequests' {} a -> s {spotInstanceRequestIds = a} :: CancelSpotInstanceRequests) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    CancelSpotInstanceRequests
  where
  type
    Rs CancelSpotInstanceRequests =
      CancelSpotInstanceRequestsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelSpotInstanceRequestsResponse'
            Prelude.<$> ( x Prelude..@? "spotInstanceRequestSet"
                            Prelude..!@ Prelude.mempty
                            Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSpotInstanceRequests

instance Prelude.NFData CancelSpotInstanceRequests

instance Prelude.ToHeaders CancelSpotInstanceRequests where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath CancelSpotInstanceRequests where
  toPath = Prelude.const "/"

instance Prelude.ToQuery CancelSpotInstanceRequests where
  toQuery CancelSpotInstanceRequests' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("CancelSpotInstanceRequests" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Prelude.=: dryRun,
        Prelude.toQueryList
          "SpotInstanceRequestId"
          spotInstanceRequestIds
      ]

-- | Contains the output of CancelSpotInstanceRequests.
--
-- /See:/ 'newCancelSpotInstanceRequestsResponse' smart constructor.
data CancelSpotInstanceRequestsResponse = CancelSpotInstanceRequestsResponse'
  { -- | One or more Spot Instance requests.
    cancelledSpotInstanceRequests :: Prelude.Maybe [CancelledSpotInstanceRequest],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  CancelSpotInstanceRequestsResponse
newCancelSpotInstanceRequestsResponse pHttpStatus_ =
  CancelSpotInstanceRequestsResponse'
    { cancelledSpotInstanceRequests =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | One or more Spot Instance requests.
cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests :: Lens.Lens' CancelSpotInstanceRequestsResponse (Prelude.Maybe [CancelledSpotInstanceRequest])
cancelSpotInstanceRequestsResponse_cancelledSpotInstanceRequests = Lens.lens (\CancelSpotInstanceRequestsResponse' {cancelledSpotInstanceRequests} -> cancelledSpotInstanceRequests) (\s@CancelSpotInstanceRequestsResponse' {} a -> s {cancelledSpotInstanceRequests = a} :: CancelSpotInstanceRequestsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
cancelSpotInstanceRequestsResponse_httpStatus :: Lens.Lens' CancelSpotInstanceRequestsResponse Prelude.Int
cancelSpotInstanceRequestsResponse_httpStatus = Lens.lens (\CancelSpotInstanceRequestsResponse' {httpStatus} -> httpStatus) (\s@CancelSpotInstanceRequestsResponse' {} a -> s {httpStatus = a} :: CancelSpotInstanceRequestsResponse)

instance
  Prelude.NFData
    CancelSpotInstanceRequestsResponse
