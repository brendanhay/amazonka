{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DeletePendingAggregationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes pending authorization requests for a specified aggregator account in a specified region.
module Network.AWS.Config.DeletePendingAggregationRequest
  ( -- * Creating a request
    DeletePendingAggregationRequest (..),
    mkDeletePendingAggregationRequest,

    -- ** Request lenses
    dparRequesterAccountId,
    dparRequesterAWSRegion,

    -- * Destructuring the response
    DeletePendingAggregationRequestResponse (..),
    mkDeletePendingAggregationRequestResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Lude.Text,
    -- | The region requesting to aggregate data.
    requesterAWSRegion :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePendingAggregationRequest' with the minimum fields required to make a request.
--
-- * 'requesterAccountId' - The 12-digit account ID of the account requesting to aggregate data.
-- * 'requesterAWSRegion' - The region requesting to aggregate data.
mkDeletePendingAggregationRequest ::
  -- | 'requesterAccountId'
  Lude.Text ->
  -- | 'requesterAWSRegion'
  Lude.Text ->
  DeletePendingAggregationRequest
mkDeletePendingAggregationRequest
  pRequesterAccountId_
  pRequesterAWSRegion_ =
    DeletePendingAggregationRequest'
      { requesterAccountId =
          pRequesterAccountId_,
        requesterAWSRegion = pRequesterAWSRegion_
      }

-- | The 12-digit account ID of the account requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparRequesterAccountId :: Lens.Lens' DeletePendingAggregationRequest Lude.Text
dparRequesterAccountId = Lens.lens (requesterAccountId :: DeletePendingAggregationRequest -> Lude.Text) (\s a -> s {requesterAccountId = a} :: DeletePendingAggregationRequest)
{-# DEPRECATED dparRequesterAccountId "Use generic-lens or generic-optics with 'requesterAccountId' instead." #-}

-- | The region requesting to aggregate data.
--
-- /Note:/ Consider using 'requesterAWSRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dparRequesterAWSRegion :: Lens.Lens' DeletePendingAggregationRequest Lude.Text
dparRequesterAWSRegion = Lens.lens (requesterAWSRegion :: DeletePendingAggregationRequest -> Lude.Text) (\s a -> s {requesterAWSRegion = a} :: DeletePendingAggregationRequest)
{-# DEPRECATED dparRequesterAWSRegion "Use generic-lens or generic-optics with 'requesterAWSRegion' instead." #-}

instance Lude.AWSRequest DeletePendingAggregationRequest where
  type
    Rs DeletePendingAggregationRequest =
      DeletePendingAggregationRequestResponse
  request = Req.postJSON configService
  response = Res.receiveNull DeletePendingAggregationRequestResponse'

instance Lude.ToHeaders DeletePendingAggregationRequest where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StarlingDoveService.DeletePendingAggregationRequest" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeletePendingAggregationRequest where
  toJSON DeletePendingAggregationRequest' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("RequesterAccountId" Lude..= requesterAccountId),
            Lude.Just ("RequesterAwsRegion" Lude..= requesterAWSRegion)
          ]
      )

instance Lude.ToPath DeletePendingAggregationRequest where
  toPath = Lude.const "/"

instance Lude.ToQuery DeletePendingAggregationRequest where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse = DeletePendingAggregationRequestResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeletePendingAggregationRequestResponse' with the minimum fields required to make a request.
mkDeletePendingAggregationRequestResponse ::
  DeletePendingAggregationRequestResponse
mkDeletePendingAggregationRequestResponse =
  DeletePendingAggregationRequestResponse'
