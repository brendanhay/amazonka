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
-- Module      : Amazonka.Backup.DeleteBackupSelection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the resource selection associated with a backup plan that is
-- specified by the @SelectionId@.
module Amazonka.Backup.DeleteBackupSelection
  ( -- * Creating a Request
    DeleteBackupSelection (..),
    newDeleteBackupSelection,

    -- * Request Lenses
    deleteBackupSelection_backupPlanId,
    deleteBackupSelection_selectionId,

    -- * Destructuring the Response
    DeleteBackupSelectionResponse (..),
    newDeleteBackupSelectionResponse,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBackupSelection' smart constructor.
data DeleteBackupSelection = DeleteBackupSelection'
  { -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text,
    -- | Uniquely identifies the body of a request to assign a set of resources
    -- to a backup plan.
    selectionId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupSelection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanId', 'deleteBackupSelection_backupPlanId' - Uniquely identifies a backup plan.
--
-- 'selectionId', 'deleteBackupSelection_selectionId' - Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
newDeleteBackupSelection ::
  -- | 'backupPlanId'
  Prelude.Text ->
  -- | 'selectionId'
  Prelude.Text ->
  DeleteBackupSelection
newDeleteBackupSelection pBackupPlanId_ pSelectionId_ =
  DeleteBackupSelection'
    { backupPlanId =
        pBackupPlanId_,
      selectionId = pSelectionId_
    }

-- | Uniquely identifies a backup plan.
deleteBackupSelection_backupPlanId :: Lens.Lens' DeleteBackupSelection Prelude.Text
deleteBackupSelection_backupPlanId = Lens.lens (\DeleteBackupSelection' {backupPlanId} -> backupPlanId) (\s@DeleteBackupSelection' {} a -> s {backupPlanId = a} :: DeleteBackupSelection)

-- | Uniquely identifies the body of a request to assign a set of resources
-- to a backup plan.
deleteBackupSelection_selectionId :: Lens.Lens' DeleteBackupSelection Prelude.Text
deleteBackupSelection_selectionId = Lens.lens (\DeleteBackupSelection' {selectionId} -> selectionId) (\s@DeleteBackupSelection' {} a -> s {selectionId = a} :: DeleteBackupSelection)

instance Core.AWSRequest DeleteBackupSelection where
  type
    AWSResponse DeleteBackupSelection =
      DeleteBackupSelectionResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteBackupSelectionResponse'

instance Prelude.Hashable DeleteBackupSelection where
  hashWithSalt _salt DeleteBackupSelection' {..} =
    _salt
      `Prelude.hashWithSalt` backupPlanId
      `Prelude.hashWithSalt` selectionId

instance Prelude.NFData DeleteBackupSelection where
  rnf DeleteBackupSelection' {..} =
    Prelude.rnf backupPlanId `Prelude.seq`
      Prelude.rnf selectionId

instance Data.ToHeaders DeleteBackupSelection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBackupSelection where
  toPath DeleteBackupSelection' {..} =
    Prelude.mconcat
      [ "/backup/plans/",
        Data.toBS backupPlanId,
        "/selections/",
        Data.toBS selectionId
      ]

instance Data.ToQuery DeleteBackupSelection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBackupSelectionResponse' smart constructor.
data DeleteBackupSelectionResponse = DeleteBackupSelectionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteBackupSelectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteBackupSelectionResponse ::
  DeleteBackupSelectionResponse
newDeleteBackupSelectionResponse =
  DeleteBackupSelectionResponse'

instance Prelude.NFData DeleteBackupSelectionResponse where
  rnf _ = ()
