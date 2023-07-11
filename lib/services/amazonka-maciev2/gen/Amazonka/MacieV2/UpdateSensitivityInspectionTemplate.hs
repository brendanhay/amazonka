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
-- Module      : Amazonka.MacieV2.UpdateSensitivityInspectionTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the settings for the sensitivity inspection template for an
-- account.
module Amazonka.MacieV2.UpdateSensitivityInspectionTemplate
  ( -- * Creating a Request
    UpdateSensitivityInspectionTemplate (..),
    newUpdateSensitivityInspectionTemplate,

    -- * Request Lenses
    updateSensitivityInspectionTemplate_description,
    updateSensitivityInspectionTemplate_excludes,
    updateSensitivityInspectionTemplate_includes,
    updateSensitivityInspectionTemplate_id,

    -- * Destructuring the Response
    UpdateSensitivityInspectionTemplateResponse (..),
    newUpdateSensitivityInspectionTemplateResponse,

    -- * Response Lenses
    updateSensitivityInspectionTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSensitivityInspectionTemplate' smart constructor.
data UpdateSensitivityInspectionTemplate = UpdateSensitivityInspectionTemplate'
  { -- | A custom description of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The managed data identifiers to explicitly exclude (not use) when
    -- analyzing data.
    --
    -- To exclude an allow list or custom data identifier that\'s currently
    -- included by the template, update the values for the
    -- SensitivityInspectionTemplateIncludes.allowListIds and
    -- SensitivityInspectionTemplateIncludes.customDataIdentifierIds
    -- properties, respectively.
    excludes :: Prelude.Maybe SensitivityInspectionTemplateExcludes,
    -- | The allow lists, custom data identifiers, and managed data identifiers
    -- to include (use) when analyzing data.
    includes :: Prelude.Maybe SensitivityInspectionTemplateIncludes,
    -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSensitivityInspectionTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateSensitivityInspectionTemplate_description' - A custom description of the template.
--
-- 'excludes', 'updateSensitivityInspectionTemplate_excludes' - The managed data identifiers to explicitly exclude (not use) when
-- analyzing data.
--
-- To exclude an allow list or custom data identifier that\'s currently
-- included by the template, update the values for the
-- SensitivityInspectionTemplateIncludes.allowListIds and
-- SensitivityInspectionTemplateIncludes.customDataIdentifierIds
-- properties, respectively.
--
-- 'includes', 'updateSensitivityInspectionTemplate_includes' - The allow lists, custom data identifiers, and managed data identifiers
-- to include (use) when analyzing data.
--
-- 'id', 'updateSensitivityInspectionTemplate_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newUpdateSensitivityInspectionTemplate ::
  -- | 'id'
  Prelude.Text ->
  UpdateSensitivityInspectionTemplate
newUpdateSensitivityInspectionTemplate pId_ =
  UpdateSensitivityInspectionTemplate'
    { description =
        Prelude.Nothing,
      excludes = Prelude.Nothing,
      includes = Prelude.Nothing,
      id = pId_
    }

-- | A custom description of the template.
updateSensitivityInspectionTemplate_description :: Lens.Lens' UpdateSensitivityInspectionTemplate (Prelude.Maybe Prelude.Text)
updateSensitivityInspectionTemplate_description = Lens.lens (\UpdateSensitivityInspectionTemplate' {description} -> description) (\s@UpdateSensitivityInspectionTemplate' {} a -> s {description = a} :: UpdateSensitivityInspectionTemplate)

-- | The managed data identifiers to explicitly exclude (not use) when
-- analyzing data.
--
-- To exclude an allow list or custom data identifier that\'s currently
-- included by the template, update the values for the
-- SensitivityInspectionTemplateIncludes.allowListIds and
-- SensitivityInspectionTemplateIncludes.customDataIdentifierIds
-- properties, respectively.
updateSensitivityInspectionTemplate_excludes :: Lens.Lens' UpdateSensitivityInspectionTemplate (Prelude.Maybe SensitivityInspectionTemplateExcludes)
updateSensitivityInspectionTemplate_excludes = Lens.lens (\UpdateSensitivityInspectionTemplate' {excludes} -> excludes) (\s@UpdateSensitivityInspectionTemplate' {} a -> s {excludes = a} :: UpdateSensitivityInspectionTemplate)

-- | The allow lists, custom data identifiers, and managed data identifiers
-- to include (use) when analyzing data.
updateSensitivityInspectionTemplate_includes :: Lens.Lens' UpdateSensitivityInspectionTemplate (Prelude.Maybe SensitivityInspectionTemplateIncludes)
updateSensitivityInspectionTemplate_includes = Lens.lens (\UpdateSensitivityInspectionTemplate' {includes} -> includes) (\s@UpdateSensitivityInspectionTemplate' {} a -> s {includes = a} :: UpdateSensitivityInspectionTemplate)

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
updateSensitivityInspectionTemplate_id :: Lens.Lens' UpdateSensitivityInspectionTemplate Prelude.Text
updateSensitivityInspectionTemplate_id = Lens.lens (\UpdateSensitivityInspectionTemplate' {id} -> id) (\s@UpdateSensitivityInspectionTemplate' {} a -> s {id = a} :: UpdateSensitivityInspectionTemplate)

instance
  Core.AWSRequest
    UpdateSensitivityInspectionTemplate
  where
  type
    AWSResponse UpdateSensitivityInspectionTemplate =
      UpdateSensitivityInspectionTemplateResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateSensitivityInspectionTemplateResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateSensitivityInspectionTemplate
  where
  hashWithSalt
    _salt
    UpdateSensitivityInspectionTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` excludes
        `Prelude.hashWithSalt` includes
        `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    UpdateSensitivityInspectionTemplate
  where
  rnf UpdateSensitivityInspectionTemplate' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf excludes
      `Prelude.seq` Prelude.rnf includes
      `Prelude.seq` Prelude.rnf id

instance
  Data.ToHeaders
    UpdateSensitivityInspectionTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    UpdateSensitivityInspectionTemplate
  where
  toJSON UpdateSensitivityInspectionTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("excludes" Data..=) Prelude.<$> excludes,
            ("includes" Data..=) Prelude.<$> includes
          ]
      )

instance
  Data.ToPath
    UpdateSensitivityInspectionTemplate
  where
  toPath UpdateSensitivityInspectionTemplate' {..} =
    Prelude.mconcat
      ["/templates/sensitivity-inspections/", Data.toBS id]

instance
  Data.ToQuery
    UpdateSensitivityInspectionTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSensitivityInspectionTemplateResponse' smart constructor.
data UpdateSensitivityInspectionTemplateResponse = UpdateSensitivityInspectionTemplateResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSensitivityInspectionTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateSensitivityInspectionTemplateResponse_httpStatus' - The response's http status code.
newUpdateSensitivityInspectionTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSensitivityInspectionTemplateResponse
newUpdateSensitivityInspectionTemplateResponse
  pHttpStatus_ =
    UpdateSensitivityInspectionTemplateResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
updateSensitivityInspectionTemplateResponse_httpStatus :: Lens.Lens' UpdateSensitivityInspectionTemplateResponse Prelude.Int
updateSensitivityInspectionTemplateResponse_httpStatus = Lens.lens (\UpdateSensitivityInspectionTemplateResponse' {httpStatus} -> httpStatus) (\s@UpdateSensitivityInspectionTemplateResponse' {} a -> s {httpStatus = a} :: UpdateSensitivityInspectionTemplateResponse)

instance
  Prelude.NFData
    UpdateSensitivityInspectionTemplateResponse
  where
  rnf UpdateSensitivityInspectionTemplateResponse' {..} =
    Prelude.rnf httpStatus
