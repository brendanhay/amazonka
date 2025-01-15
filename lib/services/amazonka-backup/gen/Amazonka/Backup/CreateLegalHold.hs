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
-- Module      : Amazonka.Backup.CreateLegalHold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action creates a legal hold on a recovery point (backup). A legal
-- hold is a restraint on altering or deleting a backup until an authorized
-- user cancels the legal hold. Any actions to delete or disassociate a
-- recovery point will fail with an error if one or more active legal holds
-- are on the recovery point.
module Amazonka.Backup.CreateLegalHold
  ( -- * Creating a Request
    CreateLegalHold (..),
    newCreateLegalHold,

    -- * Request Lenses
    createLegalHold_idempotencyToken,
    createLegalHold_recoveryPointSelection,
    createLegalHold_tags,
    createLegalHold_title,
    createLegalHold_description,

    -- * Destructuring the Response
    CreateLegalHoldResponse (..),
    newCreateLegalHoldResponse,

    -- * Response Lenses
    createLegalHoldResponse_creationDate,
    createLegalHoldResponse_description,
    createLegalHoldResponse_legalHoldArn,
    createLegalHoldResponse_legalHoldId,
    createLegalHoldResponse_recoveryPointSelection,
    createLegalHoldResponse_status,
    createLegalHoldResponse_title,
    createLegalHoldResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLegalHold' smart constructor.
data CreateLegalHold = CreateLegalHold'
  { -- | This is a user-chosen string used to distinguish between otherwise
    -- identical calls. Retrying a successful request with the same idempotency
    -- token results in a success message with no action taken.
    idempotencyToken :: Prelude.Maybe Prelude.Text,
    -- | This specifies criteria to assign a set of resources, such as resource
    -- types or backup vaults.
    recoveryPointSelection :: Prelude.Maybe RecoveryPointSelection,
    -- | Optional tags to include. A tag is a key-value pair you can use to
    -- manage, filter, and search for your resources. Allowed characters
    -- include UTF-8 letters, numbers, spaces, and the following characters: +
    -- - = . _ : \/.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | This is the string title of the legal hold.
    title :: Prelude.Text,
    -- | This is the string description of the legal hold.
    description :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idempotencyToken', 'createLegalHold_idempotencyToken' - This is a user-chosen string used to distinguish between otherwise
-- identical calls. Retrying a successful request with the same idempotency
-- token results in a success message with no action taken.
--
-- 'recoveryPointSelection', 'createLegalHold_recoveryPointSelection' - This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
--
-- 'tags', 'createLegalHold_tags' - Optional tags to include. A tag is a key-value pair you can use to
-- manage, filter, and search for your resources. Allowed characters
-- include UTF-8 letters, numbers, spaces, and the following characters: +
-- - = . _ : \/.
--
-- 'title', 'createLegalHold_title' - This is the string title of the legal hold.
--
-- 'description', 'createLegalHold_description' - This is the string description of the legal hold.
newCreateLegalHold ::
  -- | 'title'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  CreateLegalHold
newCreateLegalHold pTitle_ pDescription_ =
  CreateLegalHold'
    { idempotencyToken =
        Prelude.Nothing,
      recoveryPointSelection = Prelude.Nothing,
      tags = Prelude.Nothing,
      title = pTitle_,
      description = pDescription_
    }

-- | This is a user-chosen string used to distinguish between otherwise
-- identical calls. Retrying a successful request with the same idempotency
-- token results in a success message with no action taken.
createLegalHold_idempotencyToken :: Lens.Lens' CreateLegalHold (Prelude.Maybe Prelude.Text)
createLegalHold_idempotencyToken = Lens.lens (\CreateLegalHold' {idempotencyToken} -> idempotencyToken) (\s@CreateLegalHold' {} a -> s {idempotencyToken = a} :: CreateLegalHold)

-- | This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
createLegalHold_recoveryPointSelection :: Lens.Lens' CreateLegalHold (Prelude.Maybe RecoveryPointSelection)
createLegalHold_recoveryPointSelection = Lens.lens (\CreateLegalHold' {recoveryPointSelection} -> recoveryPointSelection) (\s@CreateLegalHold' {} a -> s {recoveryPointSelection = a} :: CreateLegalHold)

-- | Optional tags to include. A tag is a key-value pair you can use to
-- manage, filter, and search for your resources. Allowed characters
-- include UTF-8 letters, numbers, spaces, and the following characters: +
-- - = . _ : \/.
createLegalHold_tags :: Lens.Lens' CreateLegalHold (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createLegalHold_tags = Lens.lens (\CreateLegalHold' {tags} -> tags) (\s@CreateLegalHold' {} a -> s {tags = a} :: CreateLegalHold) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | This is the string title of the legal hold.
createLegalHold_title :: Lens.Lens' CreateLegalHold Prelude.Text
createLegalHold_title = Lens.lens (\CreateLegalHold' {title} -> title) (\s@CreateLegalHold' {} a -> s {title = a} :: CreateLegalHold)

-- | This is the string description of the legal hold.
createLegalHold_description :: Lens.Lens' CreateLegalHold Prelude.Text
createLegalHold_description = Lens.lens (\CreateLegalHold' {description} -> description) (\s@CreateLegalHold' {} a -> s {description = a} :: CreateLegalHold)

instance Core.AWSRequest CreateLegalHold where
  type
    AWSResponse CreateLegalHold =
      CreateLegalHoldResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLegalHoldResponse'
            Prelude.<$> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LegalHoldArn")
            Prelude.<*> (x Data..?> "LegalHoldId")
            Prelude.<*> (x Data..?> "RecoveryPointSelection")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Title")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLegalHold where
  hashWithSalt _salt CreateLegalHold' {..} =
    _salt
      `Prelude.hashWithSalt` idempotencyToken
      `Prelude.hashWithSalt` recoveryPointSelection
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` description

instance Prelude.NFData CreateLegalHold where
  rnf CreateLegalHold' {..} =
    Prelude.rnf idempotencyToken `Prelude.seq`
      Prelude.rnf recoveryPointSelection `Prelude.seq`
        Prelude.rnf tags `Prelude.seq`
          Prelude.rnf title `Prelude.seq`
            Prelude.rnf description

instance Data.ToHeaders CreateLegalHold where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLegalHold where
  toJSON CreateLegalHold' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("IdempotencyToken" Data..=)
              Prelude.<$> idempotencyToken,
            ("RecoveryPointSelection" Data..=)
              Prelude.<$> recoveryPointSelection,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Title" Data..= title),
            Prelude.Just ("Description" Data..= description)
          ]
      )

instance Data.ToPath CreateLegalHold where
  toPath = Prelude.const "/legal-holds/"

instance Data.ToQuery CreateLegalHold where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLegalHoldResponse' smart constructor.
data CreateLegalHoldResponse = CreateLegalHoldResponse'
  { -- | Time in number format when legal hold was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | This is the returned string description of the legal hold.
    description :: Prelude.Maybe Prelude.Text,
    -- | This is the ARN (Amazon Resource Number) of the created legal hold.
    legalHoldArn :: Prelude.Maybe Prelude.Text,
    -- | Legal hold ID returned for the specified legal hold on a recovery point.
    legalHoldId :: Prelude.Maybe Prelude.Text,
    -- | This specifies criteria to assign a set of resources, such as resource
    -- types or backup vaults.
    recoveryPointSelection :: Prelude.Maybe RecoveryPointSelection,
    -- | This displays the status of the legal hold returned after creating the
    -- legal hold. Statuses can be @ACTIVE@, @PENDING@, @CANCELED@,
    -- @CANCELING@, or @FAILED@.
    status :: Prelude.Maybe LegalHoldStatus,
    -- | This is the string title of the legal hold returned after creating the
    -- legal hold.
    title :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLegalHoldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'createLegalHoldResponse_creationDate' - Time in number format when legal hold was created.
--
-- 'description', 'createLegalHoldResponse_description' - This is the returned string description of the legal hold.
--
-- 'legalHoldArn', 'createLegalHoldResponse_legalHoldArn' - This is the ARN (Amazon Resource Number) of the created legal hold.
--
-- 'legalHoldId', 'createLegalHoldResponse_legalHoldId' - Legal hold ID returned for the specified legal hold on a recovery point.
--
-- 'recoveryPointSelection', 'createLegalHoldResponse_recoveryPointSelection' - This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
--
-- 'status', 'createLegalHoldResponse_status' - This displays the status of the legal hold returned after creating the
-- legal hold. Statuses can be @ACTIVE@, @PENDING@, @CANCELED@,
-- @CANCELING@, or @FAILED@.
--
-- 'title', 'createLegalHoldResponse_title' - This is the string title of the legal hold returned after creating the
-- legal hold.
--
-- 'httpStatus', 'createLegalHoldResponse_httpStatus' - The response's http status code.
newCreateLegalHoldResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLegalHoldResponse
newCreateLegalHoldResponse pHttpStatus_ =
  CreateLegalHoldResponse'
    { creationDate =
        Prelude.Nothing,
      description = Prelude.Nothing,
      legalHoldArn = Prelude.Nothing,
      legalHoldId = Prelude.Nothing,
      recoveryPointSelection = Prelude.Nothing,
      status = Prelude.Nothing,
      title = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Time in number format when legal hold was created.
createLegalHoldResponse_creationDate :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe Prelude.UTCTime)
createLegalHoldResponse_creationDate = Lens.lens (\CreateLegalHoldResponse' {creationDate} -> creationDate) (\s@CreateLegalHoldResponse' {} a -> s {creationDate = a} :: CreateLegalHoldResponse) Prelude.. Lens.mapping Data._Time

-- | This is the returned string description of the legal hold.
createLegalHoldResponse_description :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe Prelude.Text)
createLegalHoldResponse_description = Lens.lens (\CreateLegalHoldResponse' {description} -> description) (\s@CreateLegalHoldResponse' {} a -> s {description = a} :: CreateLegalHoldResponse)

-- | This is the ARN (Amazon Resource Number) of the created legal hold.
createLegalHoldResponse_legalHoldArn :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe Prelude.Text)
createLegalHoldResponse_legalHoldArn = Lens.lens (\CreateLegalHoldResponse' {legalHoldArn} -> legalHoldArn) (\s@CreateLegalHoldResponse' {} a -> s {legalHoldArn = a} :: CreateLegalHoldResponse)

-- | Legal hold ID returned for the specified legal hold on a recovery point.
createLegalHoldResponse_legalHoldId :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe Prelude.Text)
createLegalHoldResponse_legalHoldId = Lens.lens (\CreateLegalHoldResponse' {legalHoldId} -> legalHoldId) (\s@CreateLegalHoldResponse' {} a -> s {legalHoldId = a} :: CreateLegalHoldResponse)

-- | This specifies criteria to assign a set of resources, such as resource
-- types or backup vaults.
createLegalHoldResponse_recoveryPointSelection :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe RecoveryPointSelection)
createLegalHoldResponse_recoveryPointSelection = Lens.lens (\CreateLegalHoldResponse' {recoveryPointSelection} -> recoveryPointSelection) (\s@CreateLegalHoldResponse' {} a -> s {recoveryPointSelection = a} :: CreateLegalHoldResponse)

-- | This displays the status of the legal hold returned after creating the
-- legal hold. Statuses can be @ACTIVE@, @PENDING@, @CANCELED@,
-- @CANCELING@, or @FAILED@.
createLegalHoldResponse_status :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe LegalHoldStatus)
createLegalHoldResponse_status = Lens.lens (\CreateLegalHoldResponse' {status} -> status) (\s@CreateLegalHoldResponse' {} a -> s {status = a} :: CreateLegalHoldResponse)

-- | This is the string title of the legal hold returned after creating the
-- legal hold.
createLegalHoldResponse_title :: Lens.Lens' CreateLegalHoldResponse (Prelude.Maybe Prelude.Text)
createLegalHoldResponse_title = Lens.lens (\CreateLegalHoldResponse' {title} -> title) (\s@CreateLegalHoldResponse' {} a -> s {title = a} :: CreateLegalHoldResponse)

-- | The response's http status code.
createLegalHoldResponse_httpStatus :: Lens.Lens' CreateLegalHoldResponse Prelude.Int
createLegalHoldResponse_httpStatus = Lens.lens (\CreateLegalHoldResponse' {httpStatus} -> httpStatus) (\s@CreateLegalHoldResponse' {} a -> s {httpStatus = a} :: CreateLegalHoldResponse)

instance Prelude.NFData CreateLegalHoldResponse where
  rnf CreateLegalHoldResponse' {..} =
    Prelude.rnf creationDate `Prelude.seq`
      Prelude.rnf description `Prelude.seq`
        Prelude.rnf legalHoldArn `Prelude.seq`
          Prelude.rnf legalHoldId `Prelude.seq`
            Prelude.rnf recoveryPointSelection `Prelude.seq`
              Prelude.rnf status `Prelude.seq`
                Prelude.rnf title `Prelude.seq`
                  Prelude.rnf httpStatus
