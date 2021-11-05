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
-- Module      : Network.AWS.Nimble.DeleteStudio
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete a studio resource.
module Network.AWS.Nimble.DeleteStudio
  ( -- * Creating a Request
    DeleteStudio (..),
    newDeleteStudio,

    -- * Request Lenses
    deleteStudio_clientToken,
    deleteStudio_studioId,

    -- * Destructuring the Response
    DeleteStudioResponse (..),
    newDeleteStudioResponse,

    -- * Response Lenses
    deleteStudioResponse_studio,
    deleteStudioResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Nimble.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteStudio' smart constructor.
data DeleteStudio = DeleteStudio'
  { -- | To make an idempotent API request using one of these actions, specify a
    -- client token in the request. You should not reuse the same client token
    -- for other API requests. If you retry a request that completed
    -- successfully using the same client token and the same parameters, the
    -- retry succeeds without performing any further actions. If you retry a
    -- successful request using the same client token, but one or more of the
    -- parameters are different, the retry fails with a ValidationException
    -- error.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudio' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteStudio_clientToken' - To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
--
-- 'studioId', 'deleteStudio_studioId' - The studio ID.
newDeleteStudio ::
  -- | 'studioId'
  Prelude.Text ->
  DeleteStudio
newDeleteStudio pStudioId_ =
  DeleteStudio'
    { clientToken = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | To make an idempotent API request using one of these actions, specify a
-- client token in the request. You should not reuse the same client token
-- for other API requests. If you retry a request that completed
-- successfully using the same client token and the same parameters, the
-- retry succeeds without performing any further actions. If you retry a
-- successful request using the same client token, but one or more of the
-- parameters are different, the retry fails with a ValidationException
-- error.
deleteStudio_clientToken :: Lens.Lens' DeleteStudio (Prelude.Maybe Prelude.Text)
deleteStudio_clientToken = Lens.lens (\DeleteStudio' {clientToken} -> clientToken) (\s@DeleteStudio' {} a -> s {clientToken = a} :: DeleteStudio)

-- | The studio ID.
deleteStudio_studioId :: Lens.Lens' DeleteStudio Prelude.Text
deleteStudio_studioId = Lens.lens (\DeleteStudio' {studioId} -> studioId) (\s@DeleteStudio' {} a -> s {studioId = a} :: DeleteStudio)

instance Core.AWSRequest DeleteStudio where
  type AWSResponse DeleteStudio = DeleteStudioResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteStudioResponse'
            Prelude.<$> (x Core..?> "studio")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteStudio

instance Prelude.NFData DeleteStudio

instance Core.ToHeaders DeleteStudio where
  toHeaders DeleteStudio' {..} =
    Prelude.mconcat
      [ "X-Amz-Client-Token" Core.=# clientToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToPath DeleteStudio where
  toPath DeleteStudio' {..} =
    Prelude.mconcat
      ["/2020-08-01/studios/", Core.toBS studioId]

instance Core.ToQuery DeleteStudio where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteStudioResponse' smart constructor.
data DeleteStudioResponse = DeleteStudioResponse'
  { -- | Information about a studio.
    studio :: Prelude.Maybe Studio,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteStudioResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'studio', 'deleteStudioResponse_studio' - Information about a studio.
--
-- 'httpStatus', 'deleteStudioResponse_httpStatus' - The response's http status code.
newDeleteStudioResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteStudioResponse
newDeleteStudioResponse pHttpStatus_ =
  DeleteStudioResponse'
    { studio = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a studio.
deleteStudioResponse_studio :: Lens.Lens' DeleteStudioResponse (Prelude.Maybe Studio)
deleteStudioResponse_studio = Lens.lens (\DeleteStudioResponse' {studio} -> studio) (\s@DeleteStudioResponse' {} a -> s {studio = a} :: DeleteStudioResponse)

-- | The response's http status code.
deleteStudioResponse_httpStatus :: Lens.Lens' DeleteStudioResponse Prelude.Int
deleteStudioResponse_httpStatus = Lens.lens (\DeleteStudioResponse' {httpStatus} -> httpStatus) (\s@DeleteStudioResponse' {} a -> s {httpStatus = a} :: DeleteStudioResponse)

instance Prelude.NFData DeleteStudioResponse
