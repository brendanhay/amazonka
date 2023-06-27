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
-- Module      : Amazonka.MechanicalTurk.UpdateHITReviewStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateHITReviewStatus@ operation updates the status of a HIT. If
-- the status is Reviewable, this operation can update the status to
-- Reviewing, or it can revert a Reviewing HIT back to the Reviewable
-- status.
module Amazonka.MechanicalTurk.UpdateHITReviewStatus
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateHITReviewStatus' smart constructor.
data UpdateHITReviewStatus = UpdateHITReviewStatus'
  { -- | Specifies how to update the HIT status. Default is @False@.
    --
    -- -   Setting this to false will only transition a HIT from @Reviewable@
    --     to @Reviewing@
    --
    -- -   Setting this to true will only transition a HIT from @Reviewing@ to
    --     @Reviewable@
    revert :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the HIT to update.
    hITId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateHITReviewStatus
newUpdateHITReviewStatus pHITId_ =
  UpdateHITReviewStatus'
    { revert = Prelude.Nothing,
      hITId = pHITId_
    }

-- | Specifies how to update the HIT status. Default is @False@.
--
-- -   Setting this to false will only transition a HIT from @Reviewable@
--     to @Reviewing@
--
-- -   Setting this to true will only transition a HIT from @Reviewing@ to
--     @Reviewable@
updateHITReviewStatus_revert :: Lens.Lens' UpdateHITReviewStatus (Prelude.Maybe Prelude.Bool)
updateHITReviewStatus_revert = Lens.lens (\UpdateHITReviewStatus' {revert} -> revert) (\s@UpdateHITReviewStatus' {} a -> s {revert = a} :: UpdateHITReviewStatus)

-- | The ID of the HIT to update.
updateHITReviewStatus_hITId :: Lens.Lens' UpdateHITReviewStatus Prelude.Text
updateHITReviewStatus_hITId = Lens.lens (\UpdateHITReviewStatus' {hITId} -> hITId) (\s@UpdateHITReviewStatus' {} a -> s {hITId = a} :: UpdateHITReviewStatus)

instance Core.AWSRequest UpdateHITReviewStatus where
  type
    AWSResponse UpdateHITReviewStatus =
      UpdateHITReviewStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateHITReviewStatusResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateHITReviewStatus where
  hashWithSalt _salt UpdateHITReviewStatus' {..} =
    _salt
      `Prelude.hashWithSalt` revert
      `Prelude.hashWithSalt` hITId

instance Prelude.NFData UpdateHITReviewStatus where
  rnf UpdateHITReviewStatus' {..} =
    Prelude.rnf revert `Prelude.seq` Prelude.rnf hITId

instance Data.ToHeaders UpdateHITReviewStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.UpdateHITReviewStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateHITReviewStatus where
  toJSON UpdateHITReviewStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Revert" Data..=) Prelude.<$> revert,
            Prelude.Just ("HITId" Data..= hITId)
          ]
      )

instance Data.ToPath UpdateHITReviewStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateHITReviewStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHITReviewStatusResponse' smart constructor.
data UpdateHITReviewStatusResponse = UpdateHITReviewStatusResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateHITReviewStatusResponse
newUpdateHITReviewStatusResponse pHttpStatus_ =
  UpdateHITReviewStatusResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateHITReviewStatusResponse_httpStatus :: Lens.Lens' UpdateHITReviewStatusResponse Prelude.Int
updateHITReviewStatusResponse_httpStatus = Lens.lens (\UpdateHITReviewStatusResponse' {httpStatus} -> httpStatus) (\s@UpdateHITReviewStatusResponse' {} a -> s {httpStatus = a} :: UpdateHITReviewStatusResponse)

instance Prelude.NFData UpdateHITReviewStatusResponse where
  rnf UpdateHITReviewStatusResponse' {..} =
    Prelude.rnf httpStatus
