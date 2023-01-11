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
-- Module      : Amazonka.MacieV2.UpdateMemberSession
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables an Amazon Macie administrator to suspend or re-enable Macie for
-- a member account.
module Amazonka.MacieV2.UpdateMemberSession
  ( -- * Creating a Request
    UpdateMemberSession (..),
    newUpdateMemberSession,

    -- * Request Lenses
    updateMemberSession_id,
    updateMemberSession_status,

    -- * Destructuring the Response
    UpdateMemberSessionResponse (..),
    newUpdateMemberSessionResponse,

    -- * Response Lenses
    updateMemberSessionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMemberSession' smart constructor.
data UpdateMemberSession = UpdateMemberSession'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text,
    -- | Specifies the new status for the account. Valid values are: ENABLED,
    -- resume all Amazon Macie activities for the account; and, PAUSED, suspend
    -- all Macie activities for the account.
    status :: MacieStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMemberSession' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'updateMemberSession_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
--
-- 'status', 'updateMemberSession_status' - Specifies the new status for the account. Valid values are: ENABLED,
-- resume all Amazon Macie activities for the account; and, PAUSED, suspend
-- all Macie activities for the account.
newUpdateMemberSession ::
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  MacieStatus ->
  UpdateMemberSession
newUpdateMemberSession pId_ pStatus_ =
  UpdateMemberSession' {id = pId_, status = pStatus_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
updateMemberSession_id :: Lens.Lens' UpdateMemberSession Prelude.Text
updateMemberSession_id = Lens.lens (\UpdateMemberSession' {id} -> id) (\s@UpdateMemberSession' {} a -> s {id = a} :: UpdateMemberSession)

-- | Specifies the new status for the account. Valid values are: ENABLED,
-- resume all Amazon Macie activities for the account; and, PAUSED, suspend
-- all Macie activities for the account.
updateMemberSession_status :: Lens.Lens' UpdateMemberSession MacieStatus
updateMemberSession_status = Lens.lens (\UpdateMemberSession' {status} -> status) (\s@UpdateMemberSession' {} a -> s {status = a} :: UpdateMemberSession)

instance Core.AWSRequest UpdateMemberSession where
  type
    AWSResponse UpdateMemberSession =
      UpdateMemberSessionResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateMemberSessionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateMemberSession where
  hashWithSalt _salt UpdateMemberSession' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status

instance Prelude.NFData UpdateMemberSession where
  rnf UpdateMemberSession' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf status

instance Data.ToHeaders UpdateMemberSession where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMemberSession where
  toJSON UpdateMemberSession' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("status" Data..= status)]
      )

instance Data.ToPath UpdateMemberSession where
  toPath UpdateMemberSession' {..} =
    Prelude.mconcat ["/macie/members/", Data.toBS id]

instance Data.ToQuery UpdateMemberSession where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMemberSessionResponse' smart constructor.
data UpdateMemberSessionResponse = UpdateMemberSessionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMemberSessionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMemberSessionResponse_httpStatus' - The response's http status code.
newUpdateMemberSessionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateMemberSessionResponse
newUpdateMemberSessionResponse pHttpStatus_ =
  UpdateMemberSessionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateMemberSessionResponse_httpStatus :: Lens.Lens' UpdateMemberSessionResponse Prelude.Int
updateMemberSessionResponse_httpStatus = Lens.lens (\UpdateMemberSessionResponse' {httpStatus} -> httpStatus) (\s@UpdateMemberSessionResponse' {} a -> s {httpStatus = a} :: UpdateMemberSessionResponse)

instance Prelude.NFData UpdateMemberSessionResponse where
  rnf UpdateMemberSessionResponse' {..} =
    Prelude.rnf httpStatus
