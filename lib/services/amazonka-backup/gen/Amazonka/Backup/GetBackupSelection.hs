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
-- Module      : Amazonka.Backup.GetBackupSelection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns selection metadata and a document in JSON format that specifies
-- a list of resources that are associated with a backup plan.
module Amazonka.Backup.GetBackupSelection
  ( -- * Creating a Request
    GetBackupSelection (..),
    newGetBackupSelection,

    -- * Request Lenses
    getBackupSelection_backupPlanId,
    getBackupSelection_selectionId,

    -- * Destructuring the Response
    GetBackupSelectionResponse (..),
    newGetBackupSelectionResponse,

    -- * Response Lenses
    getBackupSelectionResponse_creationDate,
    getBackupSelectionResponse_creatorRequestId,
    getBackupSelectionResponse_backupSelection,
    getBackupSelectionResponse_selectionId,
    getBackupSelectionResponse_backupPlanId,
    getBackupSelectionResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBackupSelection' smart constructor.
data GetBackupSelection = GetBackupSelection'
  { -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text,
    -- | Uniquely identifies the body of a request to assign a set of resources
    -- to a backup plan.
    selectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanId', 'getBackupSelection_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'selectionId', 'getBackupSelection_selectionId' - Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
newGetBackupSelection ::
  -- | 'backupPlanId'
  Prelude.Text ->
  -- | 'selectionId'
  Prelude.Text ->
  GetBackupSelection
newGetBackupSelection pBackupPlanId_ pSelectionId_ =
  GetBackupSelection'
    { backupPlanId = pBackupPlanId_,
      selectionId = pSelectionId_
    }

-- | Uniquely identifies a backup plan.
getBackupSelection_backupPlanId :: Lens.Lens' GetBackupSelection Prelude.Text
getBackupSelection_backupPlanId = Lens.lens (\GetBackupSelection' {backupPlanId} -> backupPlanId) (\s@GetBackupSelection' {} a -> s {backupPlanId = a} :: GetBackupSelection)

-- | Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
getBackupSelection_selectionId :: Lens.Lens' GetBackupSelection Prelude.Text
getBackupSelection_selectionId = Lens.lens (\GetBackupSelection' {selectionId} -> selectionId) (\s@GetBackupSelection' {} a -> s {selectionId = a} :: GetBackupSelection)

instance Core.AWSRequest GetBackupSelection where
  type
    AWSResponse GetBackupSelection =
      GetBackupSelectionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackupSelectionResponse'
            Prelude.<$> (x Core..?> "CreationDate")
            Prelude.<*> (x Core..?> "CreatorRequestId")
            Prelude.<*> (x Core..?> "BackupSelection")
            Prelude.<*> (x Core..?> "SelectionId")
            Prelude.<*> (x Core..?> "BackupPlanId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackupSelection where
  hashWithSalt _salt GetBackupSelection' {..} =
    _salt `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` selectionId

instance Prelude.NFData GetBackupSelection where
  rnf GetBackupSelection' {..} =
    Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf selectionId

instance Core.ToHeaders GetBackupSelection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBackupSelection where
  toPath GetBackupSelection' {..} =
    Prelude.mconcat
      [ "/backup/plans/",
        Core.toBS backupPlanId,
        "/selections/",
        Core.toBS selectionId
      ]

instance Core.ToQuery GetBackupSelection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackupSelectionResponse' smart constructor.
data GetBackupSelectionResponse = GetBackupSelectionResponse'
  { -- | The date and time a backup selection is created, in Unix format and
    -- Coordinated Universal Time (UTC). The value of @CreationDate@ is
    -- accurate to milliseconds. For example, the value 1516925490.087
    -- represents Friday, January 26, 2018 12:11:30.087 AM.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | A unique string that identifies the request and allows failed requests
    -- to be retried without the risk of running the operation twice.
    creatorRequestId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the body of a request to assign a set of resources to a backup
    -- plan.
    backupSelection :: Prelude.Maybe BackupSelection,
    -- | Uniquely identifies the body of a request to assign a set of resources
    -- to a backup plan.
    selectionId :: Prelude.Maybe Prelude.Text,
    -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupSelectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'getBackupSelectionResponse_creationDate' - The date and time a backup selection is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
--
-- 'creatorRequestId', 'getBackupSelectionResponse_creatorRequestId' - A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
--
-- 'backupSelection', 'getBackupSelectionResponse_backupSelection' - Specifies the body of a request to assign a set of resources to a backup
-- plan.
--
-- 'selectionId', 'getBackupSelectionResponse_selectionId' - Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
--
-- 'backupPlanId', 'getBackupSelectionResponse_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'httpStatus', 'getBackupSelectionResponse_httpStatus' - The response's http status code.
newGetBackupSelectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackupSelectionResponse
newGetBackupSelectionResponse pHttpStatus_ =
  GetBackupSelectionResponse'
    { creationDate =
        Prelude.Nothing,
      creatorRequestId = Prelude.Nothing,
      backupSelection = Prelude.Nothing,
      selectionId = Prelude.Nothing,
      backupPlanId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time a backup selection is created, in Unix format and
-- Coordinated Universal Time (UTC). The value of @CreationDate@ is
-- accurate to milliseconds. For example, the value 1516925490.087
-- represents Friday, January 26, 2018 12:11:30.087 AM.
getBackupSelectionResponse_creationDate :: Lens.Lens' GetBackupSelectionResponse (Prelude.Maybe Prelude.UTCTime)
getBackupSelectionResponse_creationDate = Lens.lens (\GetBackupSelectionResponse' {creationDate} -> creationDate) (\s@GetBackupSelectionResponse' {} a -> s {creationDate = a} :: GetBackupSelectionResponse) Prelude.. Lens.mapping Core._Time

-- | A unique string that identifies the request and allows failed requests
-- to be retried without the risk of running the operation twice.
getBackupSelectionResponse_creatorRequestId :: Lens.Lens' GetBackupSelectionResponse (Prelude.Maybe Prelude.Text)
getBackupSelectionResponse_creatorRequestId = Lens.lens (\GetBackupSelectionResponse' {creatorRequestId} -> creatorRequestId) (\s@GetBackupSelectionResponse' {} a -> s {creatorRequestId = a} :: GetBackupSelectionResponse)

-- | Specifies the body of a request to assign a set of resources to a backup
-- plan.
getBackupSelectionResponse_backupSelection :: Lens.Lens' GetBackupSelectionResponse (Prelude.Maybe BackupSelection)
getBackupSelectionResponse_backupSelection = Lens.lens (\GetBackupSelectionResponse' {backupSelection} -> backupSelection) (\s@GetBackupSelectionResponse' {} a -> s {backupSelection = a} :: GetBackupSelectionResponse)

-- | Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
getBackupSelectionResponse_selectionId :: Lens.Lens' GetBackupSelectionResponse (Prelude.Maybe Prelude.Text)
getBackupSelectionResponse_selectionId = Lens.lens (\GetBackupSelectionResponse' {selectionId} -> selectionId) (\s@GetBackupSelectionResponse' {} a -> s {selectionId = a} :: GetBackupSelectionResponse)

-- | Uniquely identifies a backup plan.
getBackupSelectionResponse_backupPlanId :: Lens.Lens' GetBackupSelectionResponse (Prelude.Maybe Prelude.Text)
getBackupSelectionResponse_backupPlanId = Lens.lens (\GetBackupSelectionResponse' {backupPlanId} -> backupPlanId) (\s@GetBackupSelectionResponse' {} a -> s {backupPlanId = a} :: GetBackupSelectionResponse)

-- | The response's http status code.
getBackupSelectionResponse_httpStatus :: Lens.Lens' GetBackupSelectionResponse Prelude.Int
getBackupSelectionResponse_httpStatus = Lens.lens (\GetBackupSelectionResponse' {httpStatus} -> httpStatus) (\s@GetBackupSelectionResponse' {} a -> s {httpStatus = a} :: GetBackupSelectionResponse)

instance Prelude.NFData GetBackupSelectionResponse where
  rnf GetBackupSelectionResponse' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf creatorRequestId
      `Prelude.seq` Prelude.rnf backupSelection
      `Prelude.seq` Prelude.rnf selectionId
      `Prelude.seq` Prelude.rnf backupPlanId
      `Prelude.seq` Prelude.rnf httpStatus
