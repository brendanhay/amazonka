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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Prelude.Text,
    -- | The region requesting to aggregate data.
    requesterAwsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'requesterAwsRegion'
  Prelude.Text ->
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
deletePendingAggregationRequest_requesterAccountId :: Lens.Lens' DeletePendingAggregationRequest Prelude.Text
deletePendingAggregationRequest_requesterAccountId = Lens.lens (\DeletePendingAggregationRequest' {requesterAccountId} -> requesterAccountId) (\s@DeletePendingAggregationRequest' {} a -> s {requesterAccountId = a} :: DeletePendingAggregationRequest)

-- | The region requesting to aggregate data.
deletePendingAggregationRequest_requesterAwsRegion :: Lens.Lens' DeletePendingAggregationRequest Prelude.Text
deletePendingAggregationRequest_requesterAwsRegion = Lens.lens (\DeletePendingAggregationRequest' {requesterAwsRegion} -> requesterAwsRegion) (\s@DeletePendingAggregationRequest' {} a -> s {requesterAwsRegion = a} :: DeletePendingAggregationRequest)

instance
  Prelude.AWSRequest
    DeletePendingAggregationRequest
  where
  type
    Rs DeletePendingAggregationRequest =
      DeletePendingAggregationRequestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull
      DeletePendingAggregationRequestResponse'

instance
  Prelude.Hashable
    DeletePendingAggregationRequest

instance
  Prelude.NFData
    DeletePendingAggregationRequest

instance
  Prelude.ToHeaders
    DeletePendingAggregationRequest
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DeletePendingAggregationRequest" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    DeletePendingAggregationRequest
  where
  toJSON DeletePendingAggregationRequest' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RequesterAccountId" Prelude..= requesterAccountId),
            Prelude.Just
              ( "RequesterAwsRegion"
                  Prelude..= requesterAwsRegion
              )
          ]
      )

instance
  Prelude.ToPath
    DeletePendingAggregationRequest
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    DeletePendingAggregationRequest
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse = DeletePendingAggregationRequestResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeletePendingAggregationRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeletePendingAggregationRequestResponse ::
  DeletePendingAggregationRequestResponse
newDeletePendingAggregationRequestResponse =
  DeletePendingAggregationRequestResponse'

instance
  Prelude.NFData
    DeletePendingAggregationRequestResponse
