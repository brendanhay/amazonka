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
-- Module      : Amazonka.QuickSight.DeleteTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a template.
module Amazonka.QuickSight.DeleteTemplate
  ( -- * Creating a Request
    DeleteTemplate (..),
    newDeleteTemplate,

    -- * Request Lenses
    deleteTemplate_versionNumber,
    deleteTemplate_awsAccountId,
    deleteTemplate_templateId,

    -- * Destructuring the Response
    DeleteTemplateResponse (..),
    newDeleteTemplateResponse,

    -- * Response Lenses
    deleteTemplateResponse_arn,
    deleteTemplateResponse_requestId,
    deleteTemplateResponse_templateId,
    deleteTemplateResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteTemplate' smart constructor.
data DeleteTemplate = DeleteTemplate'
  { -- | Specifies the version of the template that you want to delete. If you
    -- don\'t provide a version number, @DeleteTemplate@ deletes all versions
    -- of the template.
    versionNumber :: Prelude.Maybe Prelude.Natural,
    -- | The ID of the Amazon Web Services account that contains the template
    -- that you\'re deleting.
    awsAccountId :: Prelude.Text,
    -- | An ID for the template you want to delete.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionNumber', 'deleteTemplate_versionNumber' - Specifies the version of the template that you want to delete. If you
-- don\'t provide a version number, @DeleteTemplate@ deletes all versions
-- of the template.
--
-- 'awsAccountId', 'deleteTemplate_awsAccountId' - The ID of the Amazon Web Services account that contains the template
-- that you\'re deleting.
--
-- 'templateId', 'deleteTemplate_templateId' - An ID for the template you want to delete.
newDeleteTemplate ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  DeleteTemplate
newDeleteTemplate pAwsAccountId_ pTemplateId_ =
  DeleteTemplate'
    { versionNumber = Prelude.Nothing,
      awsAccountId = pAwsAccountId_,
      templateId = pTemplateId_
    }

-- | Specifies the version of the template that you want to delete. If you
-- don\'t provide a version number, @DeleteTemplate@ deletes all versions
-- of the template.
deleteTemplate_versionNumber :: Lens.Lens' DeleteTemplate (Prelude.Maybe Prelude.Natural)
deleteTemplate_versionNumber = Lens.lens (\DeleteTemplate' {versionNumber} -> versionNumber) (\s@DeleteTemplate' {} a -> s {versionNumber = a} :: DeleteTemplate)

-- | The ID of the Amazon Web Services account that contains the template
-- that you\'re deleting.
deleteTemplate_awsAccountId :: Lens.Lens' DeleteTemplate Prelude.Text
deleteTemplate_awsAccountId = Lens.lens (\DeleteTemplate' {awsAccountId} -> awsAccountId) (\s@DeleteTemplate' {} a -> s {awsAccountId = a} :: DeleteTemplate)

-- | An ID for the template you want to delete.
deleteTemplate_templateId :: Lens.Lens' DeleteTemplate Prelude.Text
deleteTemplate_templateId = Lens.lens (\DeleteTemplate' {templateId} -> templateId) (\s@DeleteTemplate' {} a -> s {templateId = a} :: DeleteTemplate)

instance Core.AWSRequest DeleteTemplate where
  type
    AWSResponse DeleteTemplate =
      DeleteTemplateResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteTemplateResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> (x Data..?> "TemplateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteTemplate where
  hashWithSalt _salt DeleteTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` versionNumber
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData DeleteTemplate where
  rnf DeleteTemplate' {..} =
    Prelude.rnf versionNumber `Prelude.seq`
      Prelude.rnf awsAccountId `Prelude.seq`
        Prelude.rnf templateId

instance Data.ToHeaders DeleteTemplate where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteTemplate where
  toPath DeleteTemplate' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/templates/",
        Data.toBS templateId
      ]

instance Data.ToQuery DeleteTemplate where
  toQuery DeleteTemplate' {..} =
    Prelude.mconcat
      ["version-number" Data.=: versionNumber]

-- | /See:/ 'newDeleteTemplateResponse' smart constructor.
data DeleteTemplateResponse = DeleteTemplateResponse'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | An ID for the template.
    templateId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteTemplateResponse_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'requestId', 'deleteTemplateResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'templateId', 'deleteTemplateResponse_templateId' - An ID for the template.
--
-- 'status', 'deleteTemplateResponse_status' - The HTTP status of the request.
newDeleteTemplateResponse ::
  -- | 'status'
  Prelude.Int ->
  DeleteTemplateResponse
newDeleteTemplateResponse pStatus_ =
  DeleteTemplateResponse'
    { arn = Prelude.Nothing,
      requestId = Prelude.Nothing,
      templateId = Prelude.Nothing,
      status = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the resource.
deleteTemplateResponse_arn :: Lens.Lens' DeleteTemplateResponse (Prelude.Maybe Prelude.Text)
deleteTemplateResponse_arn = Lens.lens (\DeleteTemplateResponse' {arn} -> arn) (\s@DeleteTemplateResponse' {} a -> s {arn = a} :: DeleteTemplateResponse)

-- | The Amazon Web Services request ID for this operation.
deleteTemplateResponse_requestId :: Lens.Lens' DeleteTemplateResponse (Prelude.Maybe Prelude.Text)
deleteTemplateResponse_requestId = Lens.lens (\DeleteTemplateResponse' {requestId} -> requestId) (\s@DeleteTemplateResponse' {} a -> s {requestId = a} :: DeleteTemplateResponse)

-- | An ID for the template.
deleteTemplateResponse_templateId :: Lens.Lens' DeleteTemplateResponse (Prelude.Maybe Prelude.Text)
deleteTemplateResponse_templateId = Lens.lens (\DeleteTemplateResponse' {templateId} -> templateId) (\s@DeleteTemplateResponse' {} a -> s {templateId = a} :: DeleteTemplateResponse)

-- | The HTTP status of the request.
deleteTemplateResponse_status :: Lens.Lens' DeleteTemplateResponse Prelude.Int
deleteTemplateResponse_status = Lens.lens (\DeleteTemplateResponse' {status} -> status) (\s@DeleteTemplateResponse' {} a -> s {status = a} :: DeleteTemplateResponse)

instance Prelude.NFData DeleteTemplateResponse where
  rnf DeleteTemplateResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf requestId `Prelude.seq`
        Prelude.rnf templateId `Prelude.seq`
          Prelude.rnf status
