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
-- Module      : Network.AWS.MechanicalTurk.UpdateHITReviewStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITReviewStatus@ operation updates the status of a HIT. If
-- the status is Reviewable, this operation can update the status to
-- Reviewing, or it can revert a Reviewing HIT back to the Reviewable
-- status.
module Network.AWS.MechanicalTurk.UpdateHITReviewStatus
  ( -- * Creating a Request
    UpdateHITReviewStatus (..),
    newUpdateHITReviewStatus,

    -- * Request Lenses
    updateHITReviewStatus_revert,
    updateHITReviewStatus_hITId,

    -- * Destructuring the Response
    UpdateHITReviewStatusResponse (..),
    newUpdateHITReviewStatusResponse,

    -- * Response Lenses
    updateHITReviewStatusResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateHITReviewStatus' smart constructor.
data UpdateHITReviewStatus = UpdateHITReviewStatus'
  { -- | Specifies how to update the HIT status. Default is @False@.
    --
    -- -   Setting this to false will only transition a HIT from @Reviewable@
    --     to @Reviewing@
    --
    -- -   Setting this to true will only transition a HIT from @Reviewing@ to
    --     @Reviewable@
    revert :: Core.Maybe Core.Bool,
    -- | The ID of the HIT to update.
    hITId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateHITReviewStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revert', 'updateHITReviewStatus_revert' - Specifies how to update the HIT status. Default is @False@.
--
-- -   Setting this to false will only transition a HIT from @Reviewable@
--     to @Reviewing@
--
-- -   Setting this to true will only transition a HIT from @Reviewing@ to
--     @Reviewable@
--
-- 'hITId', 'updateHITReviewStatus_hITId' - The ID of the HIT to update.
newUpdateHITReviewStatus ::
  -- | 'hITId'
  Core.Text ->
  UpdateHITReviewStatus
newUpdateHITReviewStatus pHITId_ =
  UpdateHITReviewStatus'
    { revert = Core.Nothing,
      hITId = pHITId_
    }

-- | Specifies how to update the HIT status. Default is @False@.
--
-- -   Setting this to false will only transition a HIT from @Reviewable@
--     to @Reviewing@
--
-- -   Setting this to true will only transition a HIT from @Reviewing@ to
--     @Reviewable@
updateHITReviewStatus_revert :: Lens.Lens' UpdateHITReviewStatus (Core.Maybe Core.Bool)
updateHITReviewStatus_revert = Lens.lens (\UpdateHITReviewStatus' {revert} -> revert) (\s@UpdateHITReviewStatus' {} a -> s {revert = a} :: UpdateHITReviewStatus)

-- | The ID of the HIT to update.
updateHITReviewStatus_hITId :: Lens.Lens' UpdateHITReviewStatus Core.Text
updateHITReviewStatus_hITId = Lens.lens (\UpdateHITReviewStatus' {hITId} -> hITId) (\s@UpdateHITReviewStatus' {} a -> s {hITId = a} :: UpdateHITReviewStatus)

instance Core.AWSRequest UpdateHITReviewStatus where
  type
    AWSResponse UpdateHITReviewStatus =
      UpdateHITReviewStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateHITReviewStatusResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateHITReviewStatus

instance Core.NFData UpdateHITReviewStatus

instance Core.ToHeaders UpdateHITReviewStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.UpdateHITReviewStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateHITReviewStatus where
  toJSON UpdateHITReviewStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Revert" Core..=) Core.<$> revert,
            Core.Just ("HITId" Core..= hITId)
          ]
      )

instance Core.ToPath UpdateHITReviewStatus where
  toPath = Core.const "/"

instance Core.ToQuery UpdateHITReviewStatus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateHITReviewStatusResponse' smart constructor.
data UpdateHITReviewStatusResponse = UpdateHITReviewStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateHITReviewStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateHITReviewStatusResponse_httpStatus' - The response's http status code.
newUpdateHITReviewStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateHITReviewStatusResponse
newUpdateHITReviewStatusResponse pHttpStatus_ =
  UpdateHITReviewStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateHITReviewStatusResponse_httpStatus :: Lens.Lens' UpdateHITReviewStatusResponse Core.Int
updateHITReviewStatusResponse_httpStatus = Lens.lens (\UpdateHITReviewStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateHITReviewStatusResponse' {} a -> s {httpStatus = a} :: UpdateHITReviewStatusResponse)

instance Core.NFData UpdateHITReviewStatusResponse
