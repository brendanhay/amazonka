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
-- Module      : Amazonka.DeviceFarm.DeleteRemoteAccessSession
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a completed remote access session and its results.
module Amazonka.DeviceFarm.DeleteRemoteAccessSession
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to delete the specified remote access session.
--
-- /See:/ 'newDeleteRemoteAccessSession' smart constructor.
data DeleteRemoteAccessSession = DeleteRemoteAccessSession'
  { -- | The Amazon Resource Name (ARN) of the session for which you want to
    -- delete remote access.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteRemoteAccessSession where
  type
    AWSResponse DeleteRemoteAccessSession =
      DeleteRemoteAccessSessionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteRemoteAccessSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteRemoteAccessSession where
  hashWithSalt _salt DeleteRemoteAccessSession' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteRemoteAccessSession where
  rnf DeleteRemoteAccessSession' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteRemoteAccessSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.DeleteRemoteAccessSession" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteRemoteAccessSession where
  toJSON DeleteRemoteAccessSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteRemoteAccessSession where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteRemoteAccessSession where
  toQuery = Prelude.const Prelude.mempty

-- | The response from the server when a request is made to delete the remote
-- access session.
--
-- /See:/ 'newDeleteRemoteAccessSessionResponse' smart constructor.
data DeleteRemoteAccessSessionResponse = DeleteRemoteAccessSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteRemoteAccessSessionResponse' {..} =
    Prelude.rnf httpStatus
