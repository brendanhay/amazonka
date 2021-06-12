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
-- Module      : Network.AWS.MechanicalTurk.UpdateExpirationForHIT
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateExpirationForHIT@ operation allows you update the expiration
-- time of a HIT. If you update it to a time in the past, the HIT will be
-- immediately expired.
module Network.AWS.MechanicalTurk.UpdateExpirationForHIT
  ( -- * Creating a Request
    UpdateExpirationForHIT (..),
    newUpdateExpirationForHIT,

    -- * Request Lenses
    updateExpirationForHIT_hITId,
    updateExpirationForHIT_expireAt,

    -- * Destructuring the Response
    UpdateExpirationForHITResponse (..),
    newUpdateExpirationForHITResponse,

    -- * Response Lenses
    updateExpirationForHITResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { -- | The HIT to update.
    hITId :: Core.Text,
    -- | The date and time at which you want the HIT to expire
    expireAt :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateExpirationForHIT' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hITId', 'updateExpirationForHIT_hITId' - The HIT to update.
--
-- 'expireAt', 'updateExpirationForHIT_expireAt' - The date and time at which you want the HIT to expire
newUpdateExpirationForHIT ::
  -- | 'hITId'
  Core.Text ->
  -- | 'expireAt'
  Core.UTCTime ->
  UpdateExpirationForHIT
newUpdateExpirationForHIT pHITId_ pExpireAt_ =
  UpdateExpirationForHIT'
    { hITId = pHITId_,
      expireAt = Core._Time Lens.# pExpireAt_
    }

-- | The HIT to update.
updateExpirationForHIT_hITId :: Lens.Lens' UpdateExpirationForHIT Core.Text
updateExpirationForHIT_hITId = Lens.lens (\UpdateExpirationForHIT' {hITId} -> hITId) (\s@UpdateExpirationForHIT' {} a -> s {hITId = a} :: UpdateExpirationForHIT)

-- | The date and time at which you want the HIT to expire
updateExpirationForHIT_expireAt :: Lens.Lens' UpdateExpirationForHIT Core.UTCTime
updateExpirationForHIT_expireAt = Lens.lens (\UpdateExpirationForHIT' {expireAt} -> expireAt) (\s@UpdateExpirationForHIT' {} a -> s {expireAt = a} :: UpdateExpirationForHIT) Core.. Core._Time

instance Core.AWSRequest UpdateExpirationForHIT where
  type
    AWSResponse UpdateExpirationForHIT =
      UpdateExpirationForHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateExpirationForHITResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateExpirationForHIT

instance Core.NFData UpdateExpirationForHIT

instance Core.ToHeaders UpdateExpirationForHIT where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.UpdateExpirationForHIT" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateExpirationForHIT where
  toJSON UpdateExpirationForHIT' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("HITId" Core..= hITId),
            Core.Just ("ExpireAt" Core..= expireAt)
          ]
      )

instance Core.ToPath UpdateExpirationForHIT where
  toPath = Core.const "/"

instance Core.ToQuery UpdateExpirationForHIT where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateExpirationForHITResponse' smart constructor.
data UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateExpirationForHITResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateExpirationForHITResponse_httpStatus' - The response's http status code.
newUpdateExpirationForHITResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateExpirationForHITResponse
newUpdateExpirationForHITResponse pHttpStatus_ =
  UpdateExpirationForHITResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateExpirationForHITResponse_httpStatus :: Lens.Lens' UpdateExpirationForHITResponse Core.Int
updateExpirationForHITResponse_httpStatus = Lens.lens (\UpdateExpirationForHITResponse' {httpStatus} -> httpStatus) (\s@UpdateExpirationForHITResponse' {} a -> s {httpStatus = a} :: UpdateExpirationForHITResponse)

instance Core.NFData UpdateExpirationForHITResponse
