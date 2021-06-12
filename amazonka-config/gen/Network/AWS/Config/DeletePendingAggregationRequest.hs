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
-- Module      : Network.AWS.Config.DeletePendingAggregationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes pending authorization requests for a specified aggregator
-- account in a specified region.
module Network.AWS.Config.DeletePendingAggregationRequest
  ( -- * Creating a Request
    DeletePendingAggregationRequest (..),
    newDeletePendingAggregationRequest,

    -- * Request Lenses
    deletePendingAggregationRequest_requesterAccountId,
    deletePendingAggregationRequest_requesterAwsRegion,

    -- * Destructuring the Response
    DeletePendingAggregationRequestResponse (..),
    newDeletePendingAggregationRequestResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Core.Text,
    -- | The region requesting to aggregate data.
    requesterAwsRegion :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePendingAggregationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterAccountId', 'deletePendingAggregationRequest_requesterAccountId' - The 12-digit account ID of the account requesting to aggregate data.
--
-- 'requesterAwsRegion', 'deletePendingAggregationRequest_requesterAwsRegion' - The region requesting to aggregate data.
newDeletePendingAggregationRequest ::
  -- | 'requesterAccountId'
  Core.Text ->
  -- | 'requesterAwsRegion'
  Core.Text ->
  DeletePendingAggregationRequest
newDeletePendingAggregationRequest
  pRequesterAccountId_
  pRequesterAwsRegion_ =
    DeletePendingAggregationRequest'
      { requesterAccountId =
          pRequesterAccountId_,
        requesterAwsRegion = pRequesterAwsRegion_
      }

-- | The 12-digit account ID of the account requesting to aggregate data.
deletePendingAggregationRequest_requesterAccountId :: Lens.Lens' DeletePendingAggregationRequest Core.Text
deletePendingAggregationRequest_requesterAccountId = Lens.lens (\DeletePendingAggregationRequest' {requesterAccountId} -> requesterAccountId) (\s@DeletePendingAggregationRequest' {} a -> s {requesterAccountId = a} :: DeletePendingAggregationRequest)

-- | The region requesting to aggregate data.
deletePendingAggregationRequest_requesterAwsRegion :: Lens.Lens' DeletePendingAggregationRequest Core.Text
deletePendingAggregationRequest_requesterAwsRegion = Lens.lens (\DeletePendingAggregationRequest' {requesterAwsRegion} -> requesterAwsRegion) (\s@DeletePendingAggregationRequest' {} a -> s {requesterAwsRegion = a} :: DeletePendingAggregationRequest)

instance
  Core.AWSRequest
    DeletePendingAggregationRequest
  where
  type
    AWSResponse DeletePendingAggregationRequest =
      DeletePendingAggregationRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeletePendingAggregationRequestResponse'

instance
  Core.Hashable
    DeletePendingAggregationRequest

instance Core.NFData DeletePendingAggregationRequest

instance
  Core.ToHeaders
    DeletePendingAggregationRequest
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeletePendingAggregationRequest" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeletePendingAggregationRequest where
  toJSON DeletePendingAggregationRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("RequesterAccountId" Core..= requesterAccountId),
            Core.Just
              ("RequesterAwsRegion" Core..= requesterAwsRegion)
          ]
      )

instance Core.ToPath DeletePendingAggregationRequest where
  toPath = Core.const "/"

instance Core.ToQuery DeletePendingAggregationRequest where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse = DeletePendingAggregationRequestResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeletePendingAggregationRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePendingAggregationRequestResponse ::
  DeletePendingAggregationRequestResponse
newDeletePendingAggregationRequestResponse =
  DeletePendingAggregationRequestResponse'

instance
  Core.NFData
    DeletePendingAggregationRequestResponse
