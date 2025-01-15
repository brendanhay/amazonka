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
-- Module      : Amazonka.Backup.GetLegalHold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns details for a specified legal hold. The details are
-- the body of a legal hold in JSON format, in addition to metadata.
module Amazonka.Backup.GetLegalHold
  ( -- * Creating a Request
    GetLegalHold (..),
    newGetLegalHold,

    -- * Request Lenses
    getLegalHold_legalHoldId,

    -- * Destructuring the Response
    GetLegalHoldResponse (..),
    newGetLegalHoldResponse,

    -- * Response Lenses
    getLegalHoldResponse_cancelDescription,
    getLegalHoldResponse_cancellationDate,
    getLegalHoldResponse_creationDate,
    getLegalHoldResponse_description,
    getLegalHoldResponse_legalHoldArn,
    getLegalHoldResponse_legalHoldId,
    getLegalHoldResponse_recoveryPointSelection,
    getLegalHoldResponse_retainRecordUntil,
    getLegalHoldResponse_status,
    getLegalHoldResponse_title,
    getLegalHoldResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLegalHold' smart constructor.
data GetLegalHold = GetLegalHold'
  { -- | This is the ID required to use @GetLegalHold@. This unique ID is
    -- associated with a specific legal hold.
    legalHoldId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'legalHoldId', 'getLegalHold_legalHoldId' - This is the ID required to use @GetLegalHold@. This unique ID is
-- associated with a specific legal hold.
newGetLegalHold ::
  -- | 'legalHoldId'
  Prelude.Text ->
  GetLegalHold
newGetLegalHold pLegalHoldId_ =
  GetLegalHold' {legalHoldId = pLegalHoldId_}

-- | This is the ID required to use @GetLegalHold@. This unique ID is
-- associated with a specific legal hold.
getLegalHold_legalHoldId :: Lens.Lens' GetLegalHold Prelude.Text
getLegalHold_legalHoldId = Lens.lens (\GetLegalHold' {legalHoldId} -> legalHoldId) (\s@GetLegalHold' {} a -> s {legalHoldId = a} :: GetLegalHold)

instance Core.AWSRequest GetLegalHold where
  type AWSResponse GetLegalHold = GetLegalHoldResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLegalHoldResponse'
            Prelude.<$> (x Data..?> "CancelDescription")
            Prelude.<*> (x Data..?> "CancellationDate")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LegalHoldArn")
            Prelude.<*> (x Data..?> "LegalHoldId")
            Prelude.<*> (x Data..?> "RecoveryPointSelection")
            Prelude.<*> (x Data..?> "RetainRecordUntil")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Title")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLegalHold where
  hashWithSalt _salt GetLegalHold' {..} =
    _salt `Prelude.hashWithSalt` legalHoldId

instance Prelude.NFData GetLegalHold where
  rnf GetLegalHold' {..} = Prelude.rnf legalHoldId

instance Data.ToHeaders GetLegalHold where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLegalHold where
  toPath GetLegalHold' {..} =
    Prelude.mconcat
      ["/legal-holds/", Data.toBS legalHoldId, "/"]

instance Data.ToQuery GetLegalHold where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLegalHoldResponse' smart constructor.
data GetLegalHoldResponse = GetLegalHoldResponse'
  { -- | String describing the reason for removing the legal hold.
    cancelDescription :: Prelude.Maybe Prelude.Text,
    -- | Time in number when legal hold was cancelled.
    cancellationDate :: Prelude.Maybe Data.POSIX,
    -- | Time in number format when legal hold was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | This is the returned string description of the legal hold.
    description :: Prelude.Maybe Prelude.Text,
    -- | This is the returned framework ARN for the specified legal hold. An
    -- Amazon Resource Name (ARN) uniquely identifies a resource. The format of
    -- the ARN depends on the resource type.
    legalHoldArn :: Prelude.Maybe Prelude.Text,
    -- | This is the returned ID associated with a specified legal hold.
    legalHoldId :: Prelude.Maybe Prelude.Text,
    -- | This specifies criteria to assign a set of resources, such as resource
    -- types or backup vaults.
    recoveryPointSelection :: Prelude.Maybe RecoveryPointSelection,
    -- | This is the date and time until which the legal hold record will be
    -- retained.
    retainRecordUntil :: Prelude.Maybe Data.POSIX,
    -- | This is the status of the legal hold. Statuses can be @ACTIVE@,
    -- @CREATING@, @CANCELED@, and @CANCELING@.
    status :: Prelude.Maybe LegalHoldStatus,
    -- | This is the string title of the legal hold.
    title :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLegalHoldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cancelDescription', 'getLegalHoldResponse_cancelDescription' - String describing the reason for removing the legal hold.
--
-- 'cancellationDate', 'getLegalHoldResponse_cancellationDate' - Time in number when legal hold was cancelled.
--
-- 'creationDate', 'getLegalHoldResponse_creationDate' - Time in number format when legal hold was created.
--
-- 'description', 'getLegalHoldResponse_description' - This is the returned string description of the legal hold.
--
-- 'legalHoldArn', 'getLegalHoldResponse_legalHoldArn' - This is the returned framework ARN for the specified legal hold. An
-- Amazon Resource Name (ARN) uniquely identifies a resource. The format of
-- the ARN depends on the resource type.
--
-- 'legalHoldId', 'getLegalHoldResponse_legalHoldId' - This is the returned ID associated with a specified legal hold.
--
-- 'recoveryPointSelection', 'getLegalHoldResponse_recoveryPointSelection' - This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
--
-- 'retainRecordUntil', 'getLegalHoldResponse_retainRecordUntil' - This is the date and time until which the legal hold record will be
-- retained.
--
-- 'status', 'getLegalHoldResponse_status' - This is the status of the legal hold. Statuses can be @ACTIVE@,
-- @CREATING@, @CANCELED@, and @CANCELING@.
--
-- 'title', 'getLegalHoldResponse_title' - This is the string title of the legal hold.
--
-- 'httpStatus', 'getLegalHoldResponse_httpStatus' - The response's http status code.
newGetLegalHoldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLegalHoldResponse
newGetLegalHoldResponse pHttpStatus_ =
  GetLegalHoldResponse'
    { cancelDescription =
        Prelude.Nothing,
      cancellationDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      legalHoldArn = Prelude.Nothing,
      legalHoldId = Prelude.Nothing,
      recoveryPointSelection = Prelude.Nothing,
      retainRecordUntil = Prelude.Nothing,
      status = Prelude.Nothing,
      title = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | String describing the reason for removing the legal hold.
getLegalHoldResponse_cancelDescription :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.Text)
getLegalHoldResponse_cancelDescription = Lens.lens (\GetLegalHoldResponse' {cancelDescription} -> cancelDescription) (\s@GetLegalHoldResponse' {} a -> s {cancelDescription = a} :: GetLegalHoldResponse)

-- | Time in number when legal hold was cancelled.
getLegalHoldResponse_cancellationDate :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.UTCTime)
getLegalHoldResponse_cancellationDate = Lens.lens (\GetLegalHoldResponse' {cancellationDate} -> cancellationDate) (\s@GetLegalHoldResponse' {} a -> s {cancellationDate = a} :: GetLegalHoldResponse) Prelude.. Lens.mapping Data._Time

-- | Time in number format when legal hold was created.
getLegalHoldResponse_creationDate :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.UTCTime)
getLegalHoldResponse_creationDate = Lens.lens (\GetLegalHoldResponse' {creationDate} -> creationDate) (\s@GetLegalHoldResponse' {} a -> s {creationDate = a} :: GetLegalHoldResponse) Prelude.. Lens.mapping Data._Time

-- | This is the returned string description of the legal hold.
getLegalHoldResponse_description :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.Text)
getLegalHoldResponse_description = Lens.lens (\GetLegalHoldResponse' {description} -> description) (\s@GetLegalHoldResponse' {} a -> s {description = a} :: GetLegalHoldResponse)

-- | This is the returned framework ARN for the specified legal hold. An
-- Amazon Resource Name (ARN) uniquely identifies a resource. The format of
-- the ARN depends on the resource type.
getLegalHoldResponse_legalHoldArn :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.Text)
getLegalHoldResponse_legalHoldArn = Lens.lens (\GetLegalHoldResponse' {legalHoldArn} -> legalHoldArn) (\s@GetLegalHoldResponse' {} a -> s {legalHoldArn = a} :: GetLegalHoldResponse)

-- | This is the returned ID associated with a specified legal hold.
getLegalHoldResponse_legalHoldId :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.Text)
getLegalHoldResponse_legalHoldId = Lens.lens (\GetLegalHoldResponse' {legalHoldId} -> legalHoldId) (\s@GetLegalHoldResponse' {} a -> s {legalHoldId = a} :: GetLegalHoldResponse)

-- | This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
getLegalHoldResponse_recoveryPointSelection :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe RecoveryPointSelection)
getLegalHoldResponse_recoveryPointSelection = Lens.lens (\GetLegalHoldResponse' {recoveryPointSelection} -> recoveryPointSelection) (\s@GetLegalHoldResponse' {} a -> s {recoveryPointSelection = a} :: GetLegalHoldResponse)

-- | This is the date and time until which the legal hold record will be
-- retained.
getLegalHoldResponse_retainRecordUntil :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.UTCTime)
getLegalHoldResponse_retainRecordUntil = Lens.lens (\GetLegalHoldResponse' {retainRecordUntil} -> retainRecordUntil) (\s@GetLegalHoldResponse' {} a -> s {retainRecordUntil = a} :: GetLegalHoldResponse) Prelude.. Lens.mapping Data._Time

-- | This is the status of the legal hold. Statuses can be @ACTIVE@,
-- @CREATING@, @CANCELED@, and @CANCELING@.
getLegalHoldResponse_status :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe LegalHoldStatus)
getLegalHoldResponse_status = Lens.lens (\GetLegalHoldResponse' {status} -> status) (\s@GetLegalHoldResponse' {} a -> s {status = a} :: GetLegalHoldResponse)

-- | This is the string title of the legal hold.
getLegalHoldResponse_title :: Lens.Lens' GetLegalHoldResponse (Prelude.Maybe Prelude.Text)
getLegalHoldResponse_title = Lens.lens (\GetLegalHoldResponse' {title} -> title) (\s@GetLegalHoldResponse' {} a -> s {title = a} :: GetLegalHoldResponse)

-- | The response's http status code.
getLegalHoldResponse_httpStatus :: Lens.Lens' GetLegalHoldResponse Prelude.Int
getLegalHoldResponse_httpStatus = Lens.lens (\GetLegalHoldResponse' {httpStatus} -> httpStatus) (\s@GetLegalHoldResponse' {} a -> s {httpStatus = a} :: GetLegalHoldResponse)

instance Prelude.NFData GetLegalHoldResponse where
  rnf GetLegalHoldResponse' {..} =
    Prelude.rnf cancelDescription `Prelude.seq`
      Prelude.rnf cancellationDate `Prelude.seq`
        Prelude.rnf creationDate `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf legalHoldArn `Prelude.seq`
              Prelude.rnf legalHoldId `Prelude.seq`
                Prelude.rnf recoveryPointSelection `Prelude.seq`
                  Prelude.rnf retainRecordUntil `Prelude.seq`
                    Prelude.rnf status `Prelude.seq`
                      Prelude.rnf title `Prelude.seq`
                        Prelude.rnf httpStatus
