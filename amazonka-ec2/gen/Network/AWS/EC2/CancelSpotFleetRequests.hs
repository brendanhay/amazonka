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
-- Module      : Network.AWS.EC2.CancelSpotFleetRequests
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified Spot Fleet requests.
--
-- After you cancel a Spot Fleet request, the Spot Fleet launches no new
-- Spot Instances. You must specify whether the Spot Fleet should also
-- terminate its Spot Instances. If you terminate the instances, the Spot
-- Fleet request enters the @cancelled_terminating@ state. Otherwise, the
-- Spot Fleet request enters the @cancelled_running@ state and the
-- instances continue to run until they are interrupted or you terminate
-- them manually.
module Network.AWS.EC2.CancelSpotFleetRequests
  ( -- * Creating a Request
    CancelSpotFleetRequests (..),
    newCancelSpotFleetRequests,

    -- * Request Lenses
    cancelSpotFleetRequests_dryRun,
    cancelSpotFleetRequests_spotFleetRequestIds,
    cancelSpotFleetRequests_terminateInstances,

    -- * Destructuring the Response
    CancelSpotFleetRequestsResponse (..),
    newCancelSpotFleetRequestsResponse,

    -- * Response Lenses
    cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests,
    cancelSpotFleetRequestsResponse_successfulFleetRequests,
    cancelSpotFleetRequestsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for CancelSpotFleetRequests.
--
-- /See:/ 'newCancelSpotFleetRequests' smart constructor.
data CancelSpotFleetRequests = CancelSpotFleetRequests'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The IDs of the Spot Fleet requests.
    spotFleetRequestIds :: [Prelude.Text],
    -- | Indicates whether to terminate instances for a Spot Fleet request if it
    -- is canceled successfully.
    terminateInstances :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequests' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'cancelSpotFleetRequests_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'spotFleetRequestIds', 'cancelSpotFleetRequests_spotFleetRequestIds' - The IDs of the Spot Fleet requests.
--
-- 'terminateInstances', 'cancelSpotFleetRequests_terminateInstances' - Indicates whether to terminate instances for a Spot Fleet request if it
-- is canceled successfully.
newCancelSpotFleetRequests ::
  -- | 'terminateInstances'
  Prelude.Bool ->
  CancelSpotFleetRequests
newCancelSpotFleetRequests pTerminateInstances_ =
  CancelSpotFleetRequests'
    { dryRun = Prelude.Nothing,
      spotFleetRequestIds = Prelude.mempty,
      terminateInstances = pTerminateInstances_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cancelSpotFleetRequests_dryRun :: Lens.Lens' CancelSpotFleetRequests (Prelude.Maybe Prelude.Bool)
cancelSpotFleetRequests_dryRun = Lens.lens (\CancelSpotFleetRequests' {dryRun} -> dryRun) (\s@CancelSpotFleetRequests' {} a -> s {dryRun = a} :: CancelSpotFleetRequests)

-- | The IDs of the Spot Fleet requests.
cancelSpotFleetRequests_spotFleetRequestIds :: Lens.Lens' CancelSpotFleetRequests [Prelude.Text]
cancelSpotFleetRequests_spotFleetRequestIds = Lens.lens (\CancelSpotFleetRequests' {spotFleetRequestIds} -> spotFleetRequestIds) (\s@CancelSpotFleetRequests' {} a -> s {spotFleetRequestIds = a} :: CancelSpotFleetRequests) Prelude.. Lens._Coerce

-- | Indicates whether to terminate instances for a Spot Fleet request if it
-- is canceled successfully.
cancelSpotFleetRequests_terminateInstances :: Lens.Lens' CancelSpotFleetRequests Prelude.Bool
cancelSpotFleetRequests_terminateInstances = Lens.lens (\CancelSpotFleetRequests' {terminateInstances} -> terminateInstances) (\s@CancelSpotFleetRequests' {} a -> s {terminateInstances = a} :: CancelSpotFleetRequests)

instance Core.AWSRequest CancelSpotFleetRequests where
  type
    AWSResponse CancelSpotFleetRequests =
      CancelSpotFleetRequestsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          CancelSpotFleetRequestsResponse'
            Prelude.<$> ( x Core..@? "unsuccessfulFleetRequestSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> ( x Core..@? "successfulFleetRequestSet"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelSpotFleetRequests

instance Prelude.NFData CancelSpotFleetRequests

instance Core.ToHeaders CancelSpotFleetRequests where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CancelSpotFleetRequests where
  toPath = Prelude.const "/"

instance Core.ToQuery CancelSpotFleetRequests where
  toQuery CancelSpotFleetRequests' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CancelSpotFleetRequests" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList
          "SpotFleetRequestId"
          spotFleetRequestIds,
        "TerminateInstances" Core.=: terminateInstances
      ]

-- | Contains the output of CancelSpotFleetRequests.
--
-- /See:/ 'newCancelSpotFleetRequestsResponse' smart constructor.
data CancelSpotFleetRequestsResponse = CancelSpotFleetRequestsResponse'
  { -- | Information about the Spot Fleet requests that are not successfully
    -- canceled.
    unsuccessfulFleetRequests :: Prelude.Maybe [CancelSpotFleetRequestsErrorItem],
    -- | Information about the Spot Fleet requests that are successfully
    -- canceled.
    successfulFleetRequests :: Prelude.Maybe [CancelSpotFleetRequestsSuccessItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelSpotFleetRequestsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessfulFleetRequests', 'cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests' - Information about the Spot Fleet requests that are not successfully
-- canceled.
--
-- 'successfulFleetRequests', 'cancelSpotFleetRequestsResponse_successfulFleetRequests' - Information about the Spot Fleet requests that are successfully
-- canceled.
--
-- 'httpStatus', 'cancelSpotFleetRequestsResponse_httpStatus' - The response's http status code.
newCancelSpotFleetRequestsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelSpotFleetRequestsResponse
newCancelSpotFleetRequestsResponse pHttpStatus_ =
  CancelSpotFleetRequestsResponse'
    { unsuccessfulFleetRequests =
        Prelude.Nothing,
      successfulFleetRequests = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Spot Fleet requests that are not successfully
-- canceled.
cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests :: Lens.Lens' CancelSpotFleetRequestsResponse (Prelude.Maybe [CancelSpotFleetRequestsErrorItem])
cancelSpotFleetRequestsResponse_unsuccessfulFleetRequests = Lens.lens (\CancelSpotFleetRequestsResponse' {unsuccessfulFleetRequests} -> unsuccessfulFleetRequests) (\s@CancelSpotFleetRequestsResponse' {} a -> s {unsuccessfulFleetRequests = a} :: CancelSpotFleetRequestsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | Information about the Spot Fleet requests that are successfully
-- canceled.
cancelSpotFleetRequestsResponse_successfulFleetRequests :: Lens.Lens' CancelSpotFleetRequestsResponse (Prelude.Maybe [CancelSpotFleetRequestsSuccessItem])
cancelSpotFleetRequestsResponse_successfulFleetRequests = Lens.lens (\CancelSpotFleetRequestsResponse' {successfulFleetRequests} -> successfulFleetRequests) (\s@CancelSpotFleetRequestsResponse' {} a -> s {successfulFleetRequests = a} :: CancelSpotFleetRequestsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
cancelSpotFleetRequestsResponse_httpStatus :: Lens.Lens' CancelSpotFleetRequestsResponse Prelude.Int
cancelSpotFleetRequestsResponse_httpStatus = Lens.lens (\CancelSpotFleetRequestsResponse' {httpStatus} -> httpStatus) (\s@CancelSpotFleetRequestsResponse' {} a -> s {httpStatus = a} :: CancelSpotFleetRequestsResponse)

instance
  Prelude.NFData
    CancelSpotFleetRequestsResponse
