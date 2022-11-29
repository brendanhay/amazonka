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
-- Module      : Amazonka.Backup.GetBackupPlanFromTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the template specified by its @templateId@ as a backup plan.
module Amazonka.Backup.GetBackupPlanFromTemplate
  ( -- * Creating a Request
    GetBackupPlanFromTemplate (..),
    newGetBackupPlanFromTemplate,

    -- * Request Lenses
    getBackupPlanFromTemplate_backupPlanTemplateId,

    -- * Destructuring the Response
    GetBackupPlanFromTemplateResponse (..),
    newGetBackupPlanFromTemplateResponse,

    -- * Response Lenses
    getBackupPlanFromTemplateResponse_backupPlanDocument,
    getBackupPlanFromTemplateResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBackupPlanFromTemplate' smart constructor.
data GetBackupPlanFromTemplate = GetBackupPlanFromTemplate'
  { -- | Uniquely identifies a stored backup plan template.
    backupPlanTemplateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupPlanFromTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanTemplateId', 'getBackupPlanFromTemplate_backupPlanTemplateId' - Uniquely identifies a stored backup plan template.
newGetBackupPlanFromTemplate ::
  -- | 'backupPlanTemplateId'
  Prelude.Text ->
  GetBackupPlanFromTemplate
newGetBackupPlanFromTemplate pBackupPlanTemplateId_ =
  GetBackupPlanFromTemplate'
    { backupPlanTemplateId =
        pBackupPlanTemplateId_
    }

-- | Uniquely identifies a stored backup plan template.
getBackupPlanFromTemplate_backupPlanTemplateId :: Lens.Lens' GetBackupPlanFromTemplate Prelude.Text
getBackupPlanFromTemplate_backupPlanTemplateId = Lens.lens (\GetBackupPlanFromTemplate' {backupPlanTemplateId} -> backupPlanTemplateId) (\s@GetBackupPlanFromTemplate' {} a -> s {backupPlanTemplateId = a} :: GetBackupPlanFromTemplate)

instance Core.AWSRequest GetBackupPlanFromTemplate where
  type
    AWSResponse GetBackupPlanFromTemplate =
      GetBackupPlanFromTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBackupPlanFromTemplateResponse'
            Prelude.<$> (x Core..?> "BackupPlanDocument")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBackupPlanFromTemplate where
  hashWithSalt _salt GetBackupPlanFromTemplate' {..} =
    _salt `Prelude.hashWithSalt` backupPlanTemplateId

instance Prelude.NFData GetBackupPlanFromTemplate where
  rnf GetBackupPlanFromTemplate' {..} =
    Prelude.rnf backupPlanTemplateId

instance Core.ToHeaders GetBackupPlanFromTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetBackupPlanFromTemplate where
  toPath GetBackupPlanFromTemplate' {..} =
    Prelude.mconcat
      [ "/backup/template/plans/",
        Core.toBS backupPlanTemplateId,
        "/toPlan"
      ]

instance Core.ToQuery GetBackupPlanFromTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBackupPlanFromTemplateResponse' smart constructor.
data GetBackupPlanFromTemplateResponse = GetBackupPlanFromTemplateResponse'
  { -- | Returns the body of a backup plan based on the target template,
    -- including the name, rules, and backup vault of the plan.
    backupPlanDocument :: Prelude.Maybe BackupPlan,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBackupPlanFromTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanDocument', 'getBackupPlanFromTemplateResponse_backupPlanDocument' - Returns the body of a backup plan based on the target template,
-- including the name, rules, and backup vault of the plan.
--
-- 'httpStatus', 'getBackupPlanFromTemplateResponse_httpStatus' - The response's http status code.
newGetBackupPlanFromTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBackupPlanFromTemplateResponse
newGetBackupPlanFromTemplateResponse pHttpStatus_ =
  GetBackupPlanFromTemplateResponse'
    { backupPlanDocument =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the body of a backup plan based on the target template,
-- including the name, rules, and backup vault of the plan.
getBackupPlanFromTemplateResponse_backupPlanDocument :: Lens.Lens' GetBackupPlanFromTemplateResponse (Prelude.Maybe BackupPlan)
getBackupPlanFromTemplateResponse_backupPlanDocument = Lens.lens (\GetBackupPlanFromTemplateResponse' {backupPlanDocument} -> backupPlanDocument) (\s@GetBackupPlanFromTemplateResponse' {} a -> s {backupPlanDocument = a} :: GetBackupPlanFromTemplateResponse)

-- | The response's http status code.
getBackupPlanFromTemplateResponse_httpStatus :: Lens.Lens' GetBackupPlanFromTemplateResponse Prelude.Int
getBackupPlanFromTemplateResponse_httpStatus = Lens.lens (\GetBackupPlanFromTemplateResponse' {httpStatus} -> httpStatus) (\s@GetBackupPlanFromTemplateResponse' {} a -> s {httpStatus = a} :: GetBackupPlanFromTemplateResponse)

instance
  Prelude.NFData
    GetBackupPlanFromTemplateResponse
  where
  rnf GetBackupPlanFromTemplateResponse' {..} =
    Prelude.rnf backupPlanDocument
      `Prelude.seq` Prelude.rnf httpStatus
