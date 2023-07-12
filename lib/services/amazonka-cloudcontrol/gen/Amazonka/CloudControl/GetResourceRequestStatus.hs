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
-- Module      : Amazonka.CloudControl.GetResourceRequestStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of a resource operation request. For more
-- information, see
-- <https://docs.aws.amazon.com/cloudcontrolapi/latest/userguide/resource-operations-manage-requests.html#resource-operations-manage-requests-track Tracking the progress of resource operation requests>
-- in the /Amazon Web Services Cloud Control API User Guide/.
module Amazonka.CloudControl.GetResourceRequestStatus
  ( -- * Creating a Request
    GetResourceRequestStatus (..),
    newGetResourceRequestStatus,

    -- * Request Lenses
    getResourceRequestStatus_requestToken,

    -- * Destructuring the Response
    GetResourceRequestStatusResponse (..),
    newGetResourceRequestStatusResponse,

    -- * Response Lenses
    getResourceRequestStatusResponse_progressEvent,
    getResourceRequestStatusResponse_httpStatus,
  )
where

import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourceRequestStatus' smart constructor.
data GetResourceRequestStatus = GetResourceRequestStatus'
  { -- | A unique token used to track the progress of the resource operation
    -- request.
    --
    -- Request tokens are included in the @ProgressEvent@ type returned by a
    -- resource operation request.
    requestToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceRequestStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestToken', 'getResourceRequestStatus_requestToken' - A unique token used to track the progress of the resource operation
-- request.
--
-- Request tokens are included in the @ProgressEvent@ type returned by a
-- resource operation request.
newGetResourceRequestStatus ::
  -- | 'requestToken'
  Prelude.Text ->
  GetResourceRequestStatus
newGetResourceRequestStatus pRequestToken_ =
  GetResourceRequestStatus'
    { requestToken =
        pRequestToken_
    }

-- | A unique token used to track the progress of the resource operation
-- request.
--
-- Request tokens are included in the @ProgressEvent@ type returned by a
-- resource operation request.
getResourceRequestStatus_requestToken :: Lens.Lens' GetResourceRequestStatus Prelude.Text
getResourceRequestStatus_requestToken = Lens.lens (\GetResourceRequestStatus' {requestToken} -> requestToken) (\s@GetResourceRequestStatus' {} a -> s {requestToken = a} :: GetResourceRequestStatus)

instance Core.AWSRequest GetResourceRequestStatus where
  type
    AWSResponse GetResourceRequestStatus =
      GetResourceRequestStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourceRequestStatusResponse'
            Prelude.<$> (x Data..?> "ProgressEvent")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourceRequestStatus where
  hashWithSalt _salt GetResourceRequestStatus' {..} =
    _salt `Prelude.hashWithSalt` requestToken

instance Prelude.NFData GetResourceRequestStatus where
  rnf GetResourceRequestStatus' {..} =
    Prelude.rnf requestToken

instance Data.ToHeaders GetResourceRequestStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CloudApiService.GetResourceRequestStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourceRequestStatus where
  toJSON GetResourceRequestStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("RequestToken" Data..= requestToken)]
      )

instance Data.ToPath GetResourceRequestStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourceRequestStatus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourceRequestStatusResponse' smart constructor.
data GetResourceRequestStatusResponse = GetResourceRequestStatusResponse'
  { -- | Represents the current status of the resource operation request.
    progressEvent :: Prelude.Maybe ProgressEvent,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourceRequestStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressEvent', 'getResourceRequestStatusResponse_progressEvent' - Represents the current status of the resource operation request.
--
-- 'httpStatus', 'getResourceRequestStatusResponse_httpStatus' - The response's http status code.
newGetResourceRequestStatusResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourceRequestStatusResponse
newGetResourceRequestStatusResponse pHttpStatus_ =
  GetResourceRequestStatusResponse'
    { progressEvent =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Represents the current status of the resource operation request.
getResourceRequestStatusResponse_progressEvent :: Lens.Lens' GetResourceRequestStatusResponse (Prelude.Maybe ProgressEvent)
getResourceRequestStatusResponse_progressEvent = Lens.lens (\GetResourceRequestStatusResponse' {progressEvent} -> progressEvent) (\s@GetResourceRequestStatusResponse' {} a -> s {progressEvent = a} :: GetResourceRequestStatusResponse)

-- | The response's http status code.
getResourceRequestStatusResponse_httpStatus :: Lens.Lens' GetResourceRequestStatusResponse Prelude.Int
getResourceRequestStatusResponse_httpStatus = Lens.lens (\GetResourceRequestStatusResponse' {httpStatus} -> httpStatus) (\s@GetResourceRequestStatusResponse' {} a -> s {httpStatus = a} :: GetResourceRequestStatusResponse)

instance
  Prelude.NFData
    GetResourceRequestStatusResponse
  where
  rnf GetResourceRequestStatusResponse' {..} =
    Prelude.rnf progressEvent
      `Prelude.seq` Prelude.rnf httpStatus
