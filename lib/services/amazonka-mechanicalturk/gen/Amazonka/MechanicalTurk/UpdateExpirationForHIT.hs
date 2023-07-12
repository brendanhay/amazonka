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
-- Module      : Amazonka.MechanicalTurk.UpdateExpirationForHIT
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @UpdateExpirationForHIT@ operation allows you update the expiration
-- time of a HIT. If you update it to a time in the past, the HIT will be
-- immediately expired.
module Amazonka.MechanicalTurk.UpdateExpirationForHIT
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { -- | The HIT to update.
    hITId :: Prelude.Text,
    -- | The date and time at which you want the HIT to expire
    expireAt :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'expireAt'
  Prelude.UTCTime ->
  UpdateExpirationForHIT
newUpdateExpirationForHIT pHITId_ pExpireAt_ =
  UpdateExpirationForHIT'
    { hITId = pHITId_,
      expireAt = Data._Time Lens.# pExpireAt_
    }

-- | The HIT to update.
updateExpirationForHIT_hITId :: Lens.Lens' UpdateExpirationForHIT Prelude.Text
updateExpirationForHIT_hITId = Lens.lens (\UpdateExpirationForHIT' {hITId} -> hITId) (\s@UpdateExpirationForHIT' {} a -> s {hITId = a} :: UpdateExpirationForHIT)

-- | The date and time at which you want the HIT to expire
updateExpirationForHIT_expireAt :: Lens.Lens' UpdateExpirationForHIT Prelude.UTCTime
updateExpirationForHIT_expireAt = Lens.lens (\UpdateExpirationForHIT' {expireAt} -> expireAt) (\s@UpdateExpirationForHIT' {} a -> s {expireAt = a} :: UpdateExpirationForHIT) Prelude.. Data._Time

instance Core.AWSRequest UpdateExpirationForHIT where
  type
    AWSResponse UpdateExpirationForHIT =
      UpdateExpirationForHITResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateExpirationForHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateExpirationForHIT where
  hashWithSalt _salt UpdateExpirationForHIT' {..} =
    _salt
      `Prelude.hashWithSalt` hITId
      `Prelude.hashWithSalt` expireAt

instance Prelude.NFData UpdateExpirationForHIT where
  rnf UpdateExpirationForHIT' {..} =
    Prelude.rnf hITId
      `Prelude.seq` Prelude.rnf expireAt

instance Data.ToHeaders UpdateExpirationForHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.UpdateExpirationForHIT" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateExpirationForHIT where
  toJSON UpdateExpirationForHIT' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HITId" Data..= hITId),
            Prelude.Just ("ExpireAt" Data..= expireAt)
          ]
      )

instance Data.ToPath UpdateExpirationForHIT where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateExpirationForHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateExpirationForHITResponse' smart constructor.
data UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateExpirationForHITResponse
newUpdateExpirationForHITResponse pHttpStatus_ =
  UpdateExpirationForHITResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateExpirationForHITResponse_httpStatus :: Lens.Lens' UpdateExpirationForHITResponse Prelude.Int
updateExpirationForHITResponse_httpStatus = Lens.lens (\UpdateExpirationForHITResponse' {httpStatus} -> httpStatus) (\s@UpdateExpirationForHITResponse' {} a -> s {httpStatus = a} :: UpdateExpirationForHITResponse)

instance
  Prelude.NFData
    UpdateExpirationForHITResponse
  where
  rnf UpdateExpirationForHITResponse' {..} =
    Prelude.rnf httpStatus
