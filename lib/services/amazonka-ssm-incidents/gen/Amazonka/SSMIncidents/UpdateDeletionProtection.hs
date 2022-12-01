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
-- Module      : Amazonka.SSMIncidents.UpdateDeletionProtection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update deletion protection to either allow or deny deletion of the final
-- Region in a replication set.
module Amazonka.SSMIncidents.UpdateDeletionProtection
  ( -- * Creating a Request
    UpdateDeletionProtection (..),
    newUpdateDeletionProtection,

    -- * Request Lenses
    updateDeletionProtection_clientToken,
    updateDeletionProtection_arn,
    updateDeletionProtection_deletionProtected,

    -- * Destructuring the Response
    UpdateDeletionProtectionResponse (..),
    newUpdateDeletionProtectionResponse,

    -- * Response Lenses
    updateDeletionProtectionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateDeletionProtection' smart constructor.
data UpdateDeletionProtection = UpdateDeletionProtection'
  { -- | A token that ensures that the operation is called only once with the
    -- specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication set to update.
    arn :: Prelude.Text,
    -- | Specifies if deletion protection is turned on or off in your account.
    deletionProtected :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeletionProtection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateDeletionProtection_clientToken' - A token that ensures that the operation is called only once with the
-- specified details.
--
-- 'arn', 'updateDeletionProtection_arn' - The Amazon Resource Name (ARN) of the replication set to update.
--
-- 'deletionProtected', 'updateDeletionProtection_deletionProtected' - Specifies if deletion protection is turned on or off in your account.
newUpdateDeletionProtection ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'deletionProtected'
  Prelude.Bool ->
  UpdateDeletionProtection
newUpdateDeletionProtection pArn_ pDeletionProtected_ =
  UpdateDeletionProtection'
    { clientToken =
        Prelude.Nothing,
      arn = pArn_,
      deletionProtected = pDeletionProtected_
    }

-- | A token that ensures that the operation is called only once with the
-- specified details.
updateDeletionProtection_clientToken :: Lens.Lens' UpdateDeletionProtection (Prelude.Maybe Prelude.Text)
updateDeletionProtection_clientToken = Lens.lens (\UpdateDeletionProtection' {clientToken} -> clientToken) (\s@UpdateDeletionProtection' {} a -> s {clientToken = a} :: UpdateDeletionProtection)

-- | The Amazon Resource Name (ARN) of the replication set to update.
updateDeletionProtection_arn :: Lens.Lens' UpdateDeletionProtection Prelude.Text
updateDeletionProtection_arn = Lens.lens (\UpdateDeletionProtection' {arn} -> arn) (\s@UpdateDeletionProtection' {} a -> s {arn = a} :: UpdateDeletionProtection)

-- | Specifies if deletion protection is turned on or off in your account.
updateDeletionProtection_deletionProtected :: Lens.Lens' UpdateDeletionProtection Prelude.Bool
updateDeletionProtection_deletionProtected = Lens.lens (\UpdateDeletionProtection' {deletionProtected} -> deletionProtected) (\s@UpdateDeletionProtection' {} a -> s {deletionProtected = a} :: UpdateDeletionProtection)

instance Core.AWSRequest UpdateDeletionProtection where
  type
    AWSResponse UpdateDeletionProtection =
      UpdateDeletionProtectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDeletionProtectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDeletionProtection where
  hashWithSalt _salt UpdateDeletionProtection' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` deletionProtected

instance Prelude.NFData UpdateDeletionProtection where
  rnf UpdateDeletionProtection' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf deletionProtected

instance Core.ToHeaders UpdateDeletionProtection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDeletionProtection where
  toJSON UpdateDeletionProtection' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            Prelude.Just ("arn" Core..= arn),
            Prelude.Just
              ("deletionProtected" Core..= deletionProtected)
          ]
      )

instance Core.ToPath UpdateDeletionProtection where
  toPath = Prelude.const "/updateDeletionProtection"

instance Core.ToQuery UpdateDeletionProtection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDeletionProtectionResponse' smart constructor.
data UpdateDeletionProtectionResponse = UpdateDeletionProtectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDeletionProtectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateDeletionProtectionResponse_httpStatus' - The response's http status code.
newUpdateDeletionProtectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDeletionProtectionResponse
newUpdateDeletionProtectionResponse pHttpStatus_ =
  UpdateDeletionProtectionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateDeletionProtectionResponse_httpStatus :: Lens.Lens' UpdateDeletionProtectionResponse Prelude.Int
updateDeletionProtectionResponse_httpStatus = Lens.lens (\UpdateDeletionProtectionResponse' {httpStatus} -> httpStatus) (\s@UpdateDeletionProtectionResponse' {} a -> s {httpStatus = a} :: UpdateDeletionProtectionResponse)

instance
  Prelude.NFData
    UpdateDeletionProtectionResponse
  where
  rnf UpdateDeletionProtectionResponse' {..} =
    Prelude.rnf httpStatus
