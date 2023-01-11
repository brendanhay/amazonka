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
-- Module      : Amazonka.Backup.CancelLegalHold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action removes the specified legal hold on a recovery point. This
-- action can only be performed by a user with sufficient permissions.
module Amazonka.Backup.CancelLegalHold
  ( -- * Creating a Request
    CancelLegalHold (..),
    newCancelLegalHold,

    -- * Request Lenses
    cancelLegalHold_retainRecordInDays,
    cancelLegalHold_legalHoldId,
    cancelLegalHold_cancelDescription,

    -- * Destructuring the Response
    CancelLegalHoldResponse (..),
    newCancelLegalHoldResponse,

    -- * Response Lenses
    cancelLegalHoldResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCancelLegalHold' smart constructor.
data CancelLegalHold = CancelLegalHold'
  { -- | The integer amount in days specifying amount of days after this API
    -- operation to remove legal hold.
    retainRecordInDays :: Prelude.Maybe Prelude.Integer,
    -- | Legal hold ID required to remove the specified legal hold on a recovery
    -- point.
    legalHoldId :: Prelude.Text,
    -- | String describing the reason for removing the legal hold.
    cancelDescription :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retainRecordInDays', 'cancelLegalHold_retainRecordInDays' - The integer amount in days specifying amount of days after this API
-- operation to remove legal hold.
--
-- 'legalHoldId', 'cancelLegalHold_legalHoldId' - Legal hold ID required to remove the specified legal hold on a recovery
-- point.
--
-- 'cancelDescription', 'cancelLegalHold_cancelDescription' - String describing the reason for removing the legal hold.
newCancelLegalHold ::
  -- | 'legalHoldId'
  Prelude.Text ->
  -- | 'cancelDescription'
  Prelude.Text ->
  CancelLegalHold
newCancelLegalHold pLegalHoldId_ pCancelDescription_ =
  CancelLegalHold'
    { retainRecordInDays =
        Prelude.Nothing,
      legalHoldId = pLegalHoldId_,
      cancelDescription = pCancelDescription_
    }

-- | The integer amount in days specifying amount of days after this API
-- operation to remove legal hold.
cancelLegalHold_retainRecordInDays :: Lens.Lens' CancelLegalHold (Prelude.Maybe Prelude.Integer)
cancelLegalHold_retainRecordInDays = Lens.lens (\CancelLegalHold' {retainRecordInDays} -> retainRecordInDays) (\s@CancelLegalHold' {} a -> s {retainRecordInDays = a} :: CancelLegalHold)

-- | Legal hold ID required to remove the specified legal hold on a recovery
-- point.
cancelLegalHold_legalHoldId :: Lens.Lens' CancelLegalHold Prelude.Text
cancelLegalHold_legalHoldId = Lens.lens (\CancelLegalHold' {legalHoldId} -> legalHoldId) (\s@CancelLegalHold' {} a -> s {legalHoldId = a} :: CancelLegalHold)

-- | String describing the reason for removing the legal hold.
cancelLegalHold_cancelDescription :: Lens.Lens' CancelLegalHold Prelude.Text
cancelLegalHold_cancelDescription = Lens.lens (\CancelLegalHold' {cancelDescription} -> cancelDescription) (\s@CancelLegalHold' {} a -> s {cancelDescription = a} :: CancelLegalHold)

instance Core.AWSRequest CancelLegalHold where
  type
    AWSResponse CancelLegalHold =
      CancelLegalHoldResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          CancelLegalHoldResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CancelLegalHold where
  hashWithSalt _salt CancelLegalHold' {..} =
    _salt `Prelude.hashWithSalt` retainRecordInDays
      `Prelude.hashWithSalt` legalHoldId
      `Prelude.hashWithSalt` cancelDescription

instance Prelude.NFData CancelLegalHold where
  rnf CancelLegalHold' {..} =
    Prelude.rnf retainRecordInDays
      `Prelude.seq` Prelude.rnf legalHoldId
      `Prelude.seq` Prelude.rnf cancelDescription

instance Data.ToHeaders CancelLegalHold where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath CancelLegalHold where
  toPath CancelLegalHold' {..} =
    Prelude.mconcat
      ["/legal-holds/", Data.toBS legalHoldId]

instance Data.ToQuery CancelLegalHold where
  toQuery CancelLegalHold' {..} =
    Prelude.mconcat
      [ "retainRecordInDays" Data.=: retainRecordInDays,
        "cancelDescription" Data.=: cancelDescription
      ]

-- | /See:/ 'newCancelLegalHoldResponse' smart constructor.
data CancelLegalHoldResponse = CancelLegalHoldResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CancelLegalHoldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'cancelLegalHoldResponse_httpStatus' - The response's http status code.
newCancelLegalHoldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CancelLegalHoldResponse
newCancelLegalHoldResponse pHttpStatus_ =
  CancelLegalHoldResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
cancelLegalHoldResponse_httpStatus :: Lens.Lens' CancelLegalHoldResponse Prelude.Int
cancelLegalHoldResponse_httpStatus = Lens.lens (\CancelLegalHoldResponse' {httpStatus} -> httpStatus) (\s@CancelLegalHoldResponse' {} a -> s {httpStatus = a} :: CancelLegalHoldResponse)

instance Prelude.NFData CancelLegalHoldResponse where
  rnf CancelLegalHoldResponse' {..} =
    Prelude.rnf httpStatus
