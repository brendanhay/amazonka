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

import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to delete the specified remote access session.
--
-- /See:/ 'newDeleteRemoteAccessSession' smart constructor.
data DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the session for which you want to
    -- delete remote access.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  DeleteRemoteAccessSession
newDeleteRemoteAccessSession pArn_ =
  DeleteRemoteAccessSession' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the session for which you want to
-- delete remote access.
deleteRemoteAccessSession_arn :: Lens.Lens' DeleteRemoteAccessSession Prelude.Text
deleteRemoteAccessSession_arn = Lens.lens (\DeleteRemoteAccessSession' {arn} -> arn) (\s@DeleteRemoteAccessSession' {} a -> s {arn = a} :: DeleteRemoteAccessSession)

instance Prelude.AWSRequest DeleteRemoteAccessSession where
  type
    Rs DeleteRemoteAccessSession =
      DeleteRemoteAccessSessionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemoteAccessSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRemoteAccessSession

instance Prelude.NFData DeleteRemoteAccessSession

instance Prelude.ToHeaders DeleteRemoteAccessSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "DeviceFarm_20150623.DeleteRemoteAccessSession" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteRemoteAccessSession where
  toJSON DeleteRemoteAccessSession' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Prelude..= arn)]
      )

instance Prelude.ToPath DeleteRemoteAccessSession where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteRemoteAccessSession where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server when a request is made to delete the remote
-- access session.
--
-- /See:/ 'newDeleteRemoteAccessSessionResponse' smart constructor.
data DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteRemoteAccessSessionResponse
newDeleteRemoteAccessSessionResponse pHttpStatus_ =
  DeleteRemoteAccessSessionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteRemoteAccessSessionResponse_httpStatus :: Lens.Lens' DeleteRemoteAccessSessionResponse Prelude.Int
deleteRemoteAccessSessionResponse_httpStatus = Lens.lens (\DeleteRemoteAccessSessionResponse' {httpStatus} -> httpStatus) (\s@DeleteRemoteAccessSessionResponse' {} a -> s {httpStatus = a} :: DeleteRemoteAccessSessionResponse)

instance
  Prelude.NFData
    DeleteRemoteAccessSessionResponse
