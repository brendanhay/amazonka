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
-- Module      : Amazonka.Backup.CreateBackupSelection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a JSON document that specifies a set of resources to assign to a
-- backup plan. For examples, see
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/assigning-resources.html#assigning-resources-json Assigning resources programmatically>.
module Amazonka.Backup.CreateBackupSelection
  ( -- * Creating a Request
    CreateBackupSelection (..),
    newCreateBackupSelection,

    -- * Request Lenses
    createBackupSelection_creatorRequestId,
    createBackupSelection_backupPlanId,
    createBackupSelection_backupSelection,

    -- * Destructuring the Response
    CreateBackupSelectionResponse (..),
    newCreateBackupSelectionResponse,

    -- * Response Lenses
    createBackupSelectionResponse_backupPlanId,
    createBackupSelectionResponse_creationDate,
    createBackupSelectionResponse_selectionId,
    createBackupSelectionResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateBackupSelection' smart constructor.
data CreateBackupSelection = CreateBackupSelection'
  { -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice. This
    -- parameter is optional.
    --
    -- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
    -- characters.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies the backup plan to be associated with the selection
    -- of resources.
    backupPlanId :: Prelude.Text,
    -- | Specifies the body of a request to assign a set of resources to a backup
    -- plan.
    backupSelection :: BackupSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creatorRequestId', 'createBackupSelection_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
--
-- 'backupPlanId', 'createBackupSelection_backupPlanId' - Uniquely identifies the backup plan to be associated with the selection
-- of resources.
--
-- 'backupSelection', 'createBackupSelection_backupSelection' - Specifies the body of a request to assign a set of resources to a backup
-- plan.
newCreateBackupSelection ::
  -- | 'backupPlanId'
  Prelude.Text ->
  -- | 'backupSelection'
  BackupSelection ->
  CreateBackupSelection
newCreateBackupSelection
  pBackupPlanId_
  pBackupSelection_ =
    CreateBackupSelection'
      { creatorRequestId =
          Prelude.Nothing,
        backupPlanId = pBackupPlanId_,
        backupSelection = pBackupSelection_
      }

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice. This
-- parameter is optional.
--
-- If used, this parameter must contain 1 to 50 alphanumeric or \'-_.\'
-- characters.
createBackupSelection_creatorRequestId :: Lens.Lens' CreateBackupSelection (Prelude.Maybe Prelude.Text)
createBackupSelection_creatorRequestId = Lens.lens (\CreateBackupSelection' {creatorRequestId} -> creatorRequestId) (\s@CreateBackupSelection' {} a -> s {creatorRequestId = a} :: CreateBackupSelection)

-- | Uniquely identifies the backup plan to be associated with the selection
-- of resources.
createBackupSelection_backupPlanId :: Lens.Lens' CreateBackupSelection Prelude.Text
createBackupSelection_backupPlanId = Lens.lens (\CreateBackupSelection' {backupPlanId} -> backupPlanId) (\s@CreateBackupSelection' {} a -> s {backupPlanId = a} :: CreateBackupSelection)

-- | Specifies the body of a request to assign a set of resources to a backup
-- plan.
createBackupSelection_backupSelection :: Lens.Lens' CreateBackupSelection BackupSelection
createBackupSelection_backupSelection = Lens.lens (\CreateBackupSelection' {backupSelection} -> backupSelection) (\s@CreateBackupSelection' {} a -> s {backupSelection = a} :: CreateBackupSelection)

instance Core.AWSRequest CreateBackupSelection where
  type
    AWSResponse CreateBackupSelection =
      CreateBackupSelectionResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBackupSelectionResponse'
            Prelude.<$> (x Data..?> "BackupPlanId")
            Prelude.<*> (x Data..?> "CreationDate")
            Prelude.<*> (x Data..?> "SelectionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBackupSelection where
  hashWithSalt _salt CreateBackupSelection' {..} =
    _salt `Prelude.hashWithSalt` creatorRequestId
      `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` backupSelection

instance Prelude.NFData CreateBackupSelection where
  rnf CreateBackupSelection' {..} =
    Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf backupSelection

instance Data.ToHeaders CreateBackupSelection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateBackupSelection where
  toJSON CreateBackupSelection' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatorRequestId" Data..=)
              Prelude.<$> creatorRequestId,
            Prelude.Just
              ("BackupSelection" Data..= backupSelection)
          ]
      )

instance Data.ToPath CreateBackupSelection where
  toPath CreateBackupSelection' {..} =
    Prelude.mconcat
      [ "/backup/plans/",
        Data.toBS backupPlanId,
        "/selections/"
      ]

instance Data.ToQuery CreateBackupSelection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBackupSelectionResponse' smart constructor.
data CreateBackupSelectionResponse = CreateBackupSelectionResponse'
  { -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | The date and time a backup selection is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | Uniquely identifies the body of a request to assign a set of resources
    -- to a backup plan.
    selectionId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBackupSelectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanId', 'createBackupSelectionResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'creationDate', 'createBackupSelectionResponse_creationDate' - The date and time a backup selection is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'selectionId', 'createBackupSelectionResponse_selectionId' - Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
--
-- 'httpStatus', 'createBackupSelectionResponse_httpStatus' - The response's http status code.
newCreateBackupSelectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBackupSelectionResponse
newCreateBackupSelectionResponse pHttpStatus_ =
  CreateBackupSelectionResponse'
    { backupPlanId =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      selectionId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Uniquely identifies a backup plan.
createBackupSelectionResponse_backupPlanId :: Lens.Lens' CreateBackupSelectionResponse (Prelude.Maybe Prelude.Text)
createBackupSelectionResponse_backupPlanId = Lens.lens (\CreateBackupSelectionResponse' {backupPlanId} -> backupPlanId) (\s@CreateBackupSelectionResponse' {} a -> s {backupPlanId = a} :: CreateBackupSelectionResponse)

-- | The date and time a backup selection is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
createBackupSelectionResponse_creationDate :: Lens.Lens' CreateBackupSelectionResponse (Prelude.Maybe Prelude.UTCTime)
createBackupSelectionResponse_creationDate = Lens.lens (\CreateBackupSelectionResponse' {creationDate} -> creationDate) (\s@CreateBackupSelectionResponse' {} a -> s {creationDate = a} :: CreateBackupSelectionResponse) Prelude.. Lens.mapping Data._Time

-- | Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
createBackupSelectionResponse_selectionId :: Lens.Lens' CreateBackupSelectionResponse (Prelude.Maybe Prelude.Text)
createBackupSelectionResponse_selectionId = Lens.lens (\CreateBackupSelectionResponse' {selectionId} -> selectionId) (\s@CreateBackupSelectionResponse' {} a -> s {selectionId = a} :: CreateBackupSelectionResponse)

-- | The response's http status code.
createBackupSelectionResponse_httpStatus :: Lens.Lens' CreateBackupSelectionResponse Prelude.Int
createBackupSelectionResponse_httpStatus = Lens.lens (\CreateBackupSelectionResponse' {httpStatus} -> httpStatus) (\s@CreateBackupSelectionResponse' {} a -> s {httpStatus = a} :: CreateBackupSelectionResponse)

instance Prelude.NFData CreateBackupSelectionResponse where
  rnf CreateBackupSelectionResponse' {..} =
    Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf selectionId
      `Prelude.seq` Prelude.rnf httpStatus
