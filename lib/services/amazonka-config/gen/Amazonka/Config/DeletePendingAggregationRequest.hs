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
-- Module      : Amazonka.Config.DeletePendingAggregationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes pending authorization requests for a specified aggregator
-- account in a specified region.
module Amazonka.Config.DeletePendingAggregationRequest
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeletePendingAggregationRequest' smart constructor.
data DeletePendingAggregationRequest = DeletePendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Prelude.Text,
    -- | The region requesting to aggregate data.
    requesterAwsRegion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Core.AWSRequest
    DeletePendingAggregationRequest
  where
  type
    AWSResponse DeletePendingAggregationRequest =
      DeletePendingAggregationRequestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull
      DeletePendingAggregationRequestResponse'

instance
  Prelude.Hashable
    DeletePendingAggregationRequest
  where
  hashWithSalt
    _salt
    DeletePendingAggregationRequest' {..} =
      _salt `Prelude.hashWithSalt` requesterAccountId
        `Prelude.hashWithSalt` requesterAwsRegion

instance
  Prelude.NFData
    DeletePendingAggregationRequest
  where
  rnf DeletePendingAggregationRequest' {..} =
    Prelude.rnf requesterAccountId
      `Prelude.seq` Prelude.rnf requesterAwsRegion

instance
  Core.ToHeaders
    DeletePendingAggregationRequest
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeletePendingAggregationRequest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeletePendingAggregationRequest where
  toJSON DeletePendingAggregationRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("RequesterAccountId" Core..= requesterAccountId),
            Prelude.Just
              ("RequesterAwsRegion" Core..= requesterAwsRegion)
          ]
      )

instance Core.ToPath DeletePendingAggregationRequest where
  toPath = Prelude.const "/"

instance Core.ToQuery DeletePendingAggregationRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeletePendingAggregationRequestResponse' smart constructor.
data DeletePendingAggregationRequestResponse = DeletePendingAggregationRequestResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf _ = ()
