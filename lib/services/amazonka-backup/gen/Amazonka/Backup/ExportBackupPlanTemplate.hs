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
-- Module      : Amazonka.Backup.ExportBackupPlanTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the backup plan that is specified by the plan ID as a backup
-- template.
module Amazonka.Backup.ExportBackupPlanTemplate
  ( -- * Creating a Request
    ExportBackupPlanTemplate (..),
    newExportBackupPlanTemplate,

    -- * Request Lenses
    exportBackupPlanTemplate_backupPlanId,

    -- * Destructuring the Response
    ExportBackupPlanTemplateResponse (..),
    newExportBackupPlanTemplateResponse,

    -- * Response Lenses
    exportBackupPlanTemplateResponse_backupPlanTemplateJson,
    exportBackupPlanTemplateResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newExportBackupPlanTemplate' smart constructor.
data ExportBackupPlanTemplate = ExportBackupPlanTemplate'
  { -- | Uniquely identifies a backup plan.
    backupPlanId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportBackupPlanTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanId', 'exportBackupPlanTemplate_backupPlanId' - Uniquely identifies a backup plan.
newExportBackupPlanTemplate ::
  -- | 'backupPlanId'
  Prelude.Text ->
  ExportBackupPlanTemplate
newExportBackupPlanTemplate pBackupPlanId_ =
  ExportBackupPlanTemplate'
    { backupPlanId =
        pBackupPlanId_
    }

-- | Uniquely identifies a backup plan.
exportBackupPlanTemplate_backupPlanId :: Lens.Lens' ExportBackupPlanTemplate Prelude.Text
exportBackupPlanTemplate_backupPlanId = Lens.lens (\ExportBackupPlanTemplate' {backupPlanId} -> backupPlanId) (\s@ExportBackupPlanTemplate' {} a -> s {backupPlanId = a} :: ExportBackupPlanTemplate)

instance Core.AWSRequest ExportBackupPlanTemplate where
  type
    AWSResponse ExportBackupPlanTemplate =
      ExportBackupPlanTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ExportBackupPlanTemplateResponse'
            Prelude.<$> (x Data..?> "BackupPlanTemplateJson")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ExportBackupPlanTemplate where
  hashWithSalt _salt ExportBackupPlanTemplate' {..} =
    _salt `Prelude.hashWithSalt` backupPlanId

instance Prelude.NFData ExportBackupPlanTemplate where
  rnf ExportBackupPlanTemplate' {..} =
    Prelude.rnf backupPlanId

instance Data.ToHeaders ExportBackupPlanTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ExportBackupPlanTemplate where
  toPath ExportBackupPlanTemplate' {..} =
    Prelude.mconcat
      [ "/backup/plans/",
        Data.toBS backupPlanId,
        "/toTemplate/"
      ]

instance Data.ToQuery ExportBackupPlanTemplate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newExportBackupPlanTemplateResponse' smart constructor.
data ExportBackupPlanTemplateResponse = ExportBackupPlanTemplateResponse'
  { -- | The body of a backup plan template in JSON format.
    --
    -- This is a signed JSON document that cannot be modified before being
    -- passed to @GetBackupPlanFromJSON.@
    backupPlanTemplateJson :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ExportBackupPlanTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'backupPlanTemplateJson', 'exportBackupPlanTemplateResponse_backupPlanTemplateJson' - The body of a backup plan template in JSON format.
--
-- This is a signed JSON document that cannot be modified before being
-- passed to @GetBackupPlanFromJSON.@
--
-- 'httpStatus', 'exportBackupPlanTemplateResponse_httpStatus' - The response's http status code.
newExportBackupPlanTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ExportBackupPlanTemplateResponse
newExportBackupPlanTemplateResponse pHttpStatus_ =
  ExportBackupPlanTemplateResponse'
    { backupPlanTemplateJson =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The body of a backup plan template in JSON format.
--
-- This is a signed JSON document that cannot be modified before being
-- passed to @GetBackupPlanFromJSON.@
exportBackupPlanTemplateResponse_backupPlanTemplateJson :: Lens.Lens' ExportBackupPlanTemplateResponse (Prelude.Maybe Prelude.Text)
exportBackupPlanTemplateResponse_backupPlanTemplateJson = Lens.lens (\ExportBackupPlanTemplateResponse' {backupPlanTemplateJson} -> backupPlanTemplateJson) (\s@ExportBackupPlanTemplateResponse' {} a -> s {backupPlanTemplateJson = a} :: ExportBackupPlanTemplateResponse)

-- | The response's http status code.
exportBackupPlanTemplateResponse_httpStatus :: Lens.Lens' ExportBackupPlanTemplateResponse Prelude.Int
exportBackupPlanTemplateResponse_httpStatus = Lens.lens (\ExportBackupPlanTemplateResponse' {httpStatus} -> httpStatus) (\s@ExportBackupPlanTemplateResponse' {} a -> s {httpStatus = a} :: ExportBackupPlanTemplateResponse)

instance
  Prelude.NFData
    ExportBackupPlanTemplateResponse
  where
  rnf ExportBackupPlanTemplateResponse' {..} =
    Prelude.rnf backupPlanTemplateJson
      `Prelude.seq` Prelude.rnf httpStatus
