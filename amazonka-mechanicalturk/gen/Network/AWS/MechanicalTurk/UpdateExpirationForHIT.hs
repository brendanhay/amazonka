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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateExpirationForHIT' smart constructor.
data UpdateExpirationForHIT = UpdateExpirationForHIT'
  { -- | The HIT to update.
    hITId :: Prelude.Text,
    -- | The date and time at which you want the HIT to expire
    expireAt :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
      expireAt = Prelude._Time Lens.# pExpireAt_
    }

-- | The HIT to update.
updateExpirationForHIT_hITId :: Lens.Lens' UpdateExpirationForHIT Prelude.Text
updateExpirationForHIT_hITId = Lens.lens (\UpdateExpirationForHIT' {hITId} -> hITId) (\s@UpdateExpirationForHIT' {} a -> s {hITId = a} :: UpdateExpirationForHIT)

-- | The date and time at which you want the HIT to expire
updateExpirationForHIT_expireAt :: Lens.Lens' UpdateExpirationForHIT Prelude.UTCTime
updateExpirationForHIT_expireAt = Lens.lens (\UpdateExpirationForHIT' {expireAt} -> expireAt) (\s@UpdateExpirationForHIT' {} a -> s {expireAt = a} :: UpdateExpirationForHIT) Prelude.. Prelude._Time

instance Prelude.AWSRequest UpdateExpirationForHIT where
  type
    Rs UpdateExpirationForHIT =
      UpdateExpirationForHITResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateExpirationForHITResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateExpirationForHIT

instance Prelude.NFData UpdateExpirationForHIT

instance Prelude.ToHeaders UpdateExpirationForHIT where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.UpdateExpirationForHIT" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateExpirationForHIT where
  toJSON UpdateExpirationForHIT' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("HITId" Prelude..= hITId),
            Prelude.Just ("ExpireAt" Prelude..= expireAt)
          ]
      )

instance Prelude.ToPath UpdateExpirationForHIT where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateExpirationForHIT where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateExpirationForHITResponse' smart constructor.
data UpdateExpirationForHITResponse = UpdateExpirationForHITResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
