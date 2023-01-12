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
-- Module      : Amazonka.CloudControl.CancelResourceRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified resource operation request. For more information,
-- see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-manage-requests.html#resource-operations-manage-requests-cancel Canceling resource operation requests>
-- in the /Amazon Web Services Cloud Control API User Guide/.
--
-- Only resource operations requests with a status of @PENDING@ or
-- @IN_PROGRESS@ can be canceled.
module Amazonka.CloudControl.CancelResourceRequest
  ( -- * Creating a Request
    CancelResourceRequest (..),
    newCancelResourceRequest,

    -- * Request Lenses
    cancelResourceRequest_requestToken,

    -- * Destructuring the Response
    CancelResourceRequestResponse (..),
    newCancelResourceRequestResponse,

    -- * Response Lenses
    cancelResourceRequestResponse_progressEvent,
    cancelResourceRequestResponse_httpStatus,
  )
where

import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelResourceRequest' smart constructor.
data CancelResourceRequest = CancelResourceRequest'
  { -- | The @RequestToken@ of the @ProgressEvent@ object returned by the
    -- resource operation request.
    requestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelResourceRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestToken', 'cancelResourceRequest_requestToken' - The @RequestToken@ of the @ProgressEvent@ object returned by the
-- resource operation request.
newCancelResourceRequest ::
  -- | 'requestToken'
  Prelude.Text ->
  CancelResourceRequest
newCancelResourceRequest pRequestToken_ =
  CancelResourceRequest'
    { requestToken =
        pRequestToken_
    }

-- | The @RequestToken@ of the @ProgressEvent@ object returned by the
-- resource operation request.
cancelResourceRequest_requestToken :: Lens.Lens' CancelResourceRequest Prelude.Text
cancelResourceRequest_requestToken = Lens.lens (\CancelResourceRequest' {requestToken} -> requestToken) (\s@CancelResourceRequest' {} a -> s {requestToken = a} :: CancelResourceRequest)

instance Core.AWSRequest CancelResourceRequest where
  type
    AWSResponse CancelResourceRequest =
      CancelResourceRequestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CancelResourceRequestResponse'
            Prelude.<$> (x Data..?> "ProgressEvent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelResourceRequest where
  hashWithSalt _salt CancelResourceRequest' {..} =
    _salt `Prelude.hashWithSalt` requestToken

instance Prelude.NFData CancelResourceRequest where
  rnf CancelResourceRequest' {..} =
    Prelude.rnf requestToken

instance Data.ToHeaders CancelResourceRequest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudApiService.CancelResourceRequest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CancelResourceRequest where
  toJSON CancelResourceRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RequestToken" Data..= requestToken)]
      )

instance Data.ToPath CancelResourceRequest where
  toPath = Prelude.const "/"

instance Data.ToQuery CancelResourceRequest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCancelResourceRequestResponse' smart constructor.
data CancelResourceRequestResponse = CancelResourceRequestResponse'
  { progressEvent :: Prelude.Maybe ProgressEvent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelResourceRequestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressEvent', 'cancelResourceRequestResponse_progressEvent' - Undocumented member.
--
-- 'httpStatus', 'cancelResourceRequestResponse_httpStatus' - The response's http status code.
newCancelResourceRequestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelResourceRequestResponse
newCancelResourceRequestResponse pHttpStatus_ =
  CancelResourceRequestResponse'
    { progressEvent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
cancelResourceRequestResponse_progressEvent :: Lens.Lens' CancelResourceRequestResponse (Prelude.Maybe ProgressEvent)
cancelResourceRequestResponse_progressEvent = Lens.lens (\CancelResourceRequestResponse' {progressEvent} -> progressEvent) (\s@CancelResourceRequestResponse' {} a -> s {progressEvent = a} :: CancelResourceRequestResponse)

-- | The response's http status code.
cancelResourceRequestResponse_httpStatus :: Lens.Lens' CancelResourceRequestResponse Prelude.Int
cancelResourceRequestResponse_httpStatus = Lens.lens (\CancelResourceRequestResponse' {httpStatus} -> httpStatus) (\s@CancelResourceRequestResponse' {} a -> s {httpStatus = a} :: CancelResourceRequestResponse)

instance Prelude.NFData CancelResourceRequestResponse where
  rnf CancelResourceRequestResponse' {..} =
    Prelude.rnf progressEvent
      `Prelude.seq` Prelude.rnf httpStatus
