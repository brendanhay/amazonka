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
-- Module      : Network.AWS.DeviceFarm.DeleteRemoteAccessSession
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a completed remote access session and its results.
module Network.AWS.DeviceFarm.DeleteRemoteAccessSession
  ( -- * Creating a Request
    DeleteRemoteAccessSession (..),
    newDeleteRemoteAccessSession,

    -- * Request Lenses
    deleteRemoteAccessSession_arn,

    -- * Destructuring the Response
    DeleteRemoteAccessSessionResponse (..),
    newDeleteRemoteAccessSessionResponse,

    -- * Response Lenses
    deleteRemoteAccessSessionResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete the specified remote access session.
--
-- /See:/ 'newDeleteRemoteAccessSession' smart constructor.
data DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the session for which you want to
    -- delete remote access.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRemoteAccessSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteRemoteAccessSession_arn' - The Amazon Resource Name (ARN) of the session for which you want to
-- delete remote access.
newDeleteRemoteAccessSession ::
  -- | 'arn'
  Core.Text ->
  DeleteRemoteAccessSession
newDeleteRemoteAccessSession pArn_ =
  DeleteRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the session for which you want to
-- delete remote access.
deleteRemoteAccessSession_arn :: Lens.Lens' DeleteRemoteAccessSession Core.Text
deleteRemoteAccessSession_arn = Lens.lens (\DeleteRemoteAccessSession' {arn} -> arn) (\s@DeleteRemoteAccessSession' {} a -> s {arn = a} :: DeleteRemoteAccessSession)

instance Core.AWSRequest DeleteRemoteAccessSession where
  type
    AWSResponse DeleteRemoteAccessSession =
      DeleteRemoteAccessSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemoteAccessSessionResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteRemoteAccessSession

instance Core.NFData DeleteRemoteAccessSession

instance Core.ToHeaders DeleteRemoteAccessSession where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.DeleteRemoteAccessSession" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteRemoteAccessSession where
  toJSON DeleteRemoteAccessSession' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath DeleteRemoteAccessSession where
  toPath = Core.const "/"

instance Core.ToQuery DeleteRemoteAccessSession where
  toQuery = Core.const Core.mempty

-- | The response from the server when a request is made to delete the remote
-- access session.
--
-- /See:/ 'newDeleteRemoteAccessSessionResponse' smart constructor.
data DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteRemoteAccessSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteRemoteAccessSessionResponse_httpStatus' - The response's http status code.
newDeleteRemoteAccessSessionResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteRemoteAccessSessionResponse
newDeleteRemoteAccessSessionResponse pHttpStatus_ =
  DeleteRemoteAccessSessionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRemoteAccessSessionResponse_httpStatus :: Lens.Lens' DeleteRemoteAccessSessionResponse Core.Int
deleteRemoteAccessSessionResponse_httpStatus = Lens.lens (\DeleteRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: DeleteRemoteAccessSessionResponse)

instance
  Core.NFData
    DeleteRemoteAccessSessionResponse
