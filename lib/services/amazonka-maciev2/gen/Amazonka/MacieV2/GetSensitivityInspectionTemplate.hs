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
-- Module      : Amazonka.MacieV2.GetSensitivityInspectionTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the settings for the sensitivity inspection template for an
-- account.
module Amazonka.MacieV2.GetSensitivityInspectionTemplate
  ( -- * Creating a Request
    GetSensitivityInspectionTemplate (..),
    newGetSensitivityInspectionTemplate,

    -- * Request Lenses
    getSensitivityInspectionTemplate_id,

    -- * Destructuring the Response
    GetSensitivityInspectionTemplateResponse (..),
    newGetSensitivityInspectionTemplateResponse,

    -- * Response Lenses
    getSensitivityInspectionTemplateResponse_description,
    getSensitivityInspectionTemplateResponse_excludes,
    getSensitivityInspectionTemplateResponse_includes,
    getSensitivityInspectionTemplateResponse_name,
    getSensitivityInspectionTemplateResponse_sensitivityInspectionTemplateId,
    getSensitivityInspectionTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSensitivityInspectionTemplate' smart constructor.
data GetSensitivityInspectionTemplate = GetSensitivityInspectionTemplate'
  { -- | The unique identifier for the Amazon Macie resource that the request
    -- applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSensitivityInspectionTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getSensitivityInspectionTemplate_id' - The unique identifier for the Amazon Macie resource that the request
-- applies to.
newGetSensitivityInspectionTemplate ::
  -- | 'id'
  Prelude.Text ->
  GetSensitivityInspectionTemplate
newGetSensitivityInspectionTemplate pId_ =
  GetSensitivityInspectionTemplate' {id = pId_}

-- | The unique identifier for the Amazon Macie resource that the request
-- applies to.
getSensitivityInspectionTemplate_id :: Lens.Lens' GetSensitivityInspectionTemplate Prelude.Text
getSensitivityInspectionTemplate_id = Lens.lens (\GetSensitivityInspectionTemplate' {id} -> id) (\s@GetSensitivityInspectionTemplate' {} a -> s {id = a} :: GetSensitivityInspectionTemplate)

instance
  Core.AWSRequest
    GetSensitivityInspectionTemplate
  where
  type
    AWSResponse GetSensitivityInspectionTemplate =
      GetSensitivityInspectionTemplateResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSensitivityInspectionTemplateResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "excludes")
            Prelude.<*> (x Data..?> "includes")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "sensitivityInspectionTemplateId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetSensitivityInspectionTemplate
  where
  hashWithSalt
    _salt
    GetSensitivityInspectionTemplate' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetSensitivityInspectionTemplate
  where
  rnf GetSensitivityInspectionTemplate' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetSensitivityInspectionTemplate
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

instance Data.ToPath GetSensitivityInspectionTemplate where
  toPath GetSensitivityInspectionTemplate' {..} =
    Prelude.mconcat
      ["/templates/sensitivity-inspections/", Data.toBS id]

instance
  Data.ToQuery
    GetSensitivityInspectionTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSensitivityInspectionTemplateResponse' smart constructor.
data GetSensitivityInspectionTemplateResponse = GetSensitivityInspectionTemplateResponse'
  { -- | The custom description of the template.
    description :: Prelude.Maybe Prelude.Text,
    -- | The managed data identifiers that are explicitly excluded (not used)
    -- when analyzing data.
    excludes :: Prelude.Maybe SensitivityInspectionTemplateExcludes,
    -- | The allow lists, custom data identifiers, and managed data identifiers
    -- that are included (used) when analyzing data.
    includes :: Prelude.Maybe SensitivityInspectionTemplateIncludes,
    -- | The name of the template.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the template.
    sensitivityInspectionTemplateId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSensitivityInspectionTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getSensitivityInspectionTemplateResponse_description' - The custom description of the template.
--
-- 'excludes', 'getSensitivityInspectionTemplateResponse_excludes' - The managed data identifiers that are explicitly excluded (not used)
-- when analyzing data.
--
-- 'includes', 'getSensitivityInspectionTemplateResponse_includes' - The allow lists, custom data identifiers, and managed data identifiers
-- that are included (used) when analyzing data.
--
-- 'name', 'getSensitivityInspectionTemplateResponse_name' - The name of the template.
--
-- 'sensitivityInspectionTemplateId', 'getSensitivityInspectionTemplateResponse_sensitivityInspectionTemplateId' - The unique identifier for the template.
--
-- 'httpStatus', 'getSensitivityInspectionTemplateResponse_httpStatus' - The response's http status code.
newGetSensitivityInspectionTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSensitivityInspectionTemplateResponse
newGetSensitivityInspectionTemplateResponse
  pHttpStatus_ =
    GetSensitivityInspectionTemplateResponse'
      { description =
          Prelude.Nothing,
        excludes = Prelude.Nothing,
        includes = Prelude.Nothing,
        name = Prelude.Nothing,
        sensitivityInspectionTemplateId =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The custom description of the template.
getSensitivityInspectionTemplateResponse_description :: Lens.Lens' GetSensitivityInspectionTemplateResponse (Prelude.Maybe Prelude.Text)
getSensitivityInspectionTemplateResponse_description = Lens.lens (\GetSensitivityInspectionTemplateResponse' {description} -> description) (\s@GetSensitivityInspectionTemplateResponse' {} a -> s {description = a} :: GetSensitivityInspectionTemplateResponse)

-- | The managed data identifiers that are explicitly excluded (not used)
-- when analyzing data.
getSensitivityInspectionTemplateResponse_excludes :: Lens.Lens' GetSensitivityInspectionTemplateResponse (Prelude.Maybe SensitivityInspectionTemplateExcludes)
getSensitivityInspectionTemplateResponse_excludes = Lens.lens (\GetSensitivityInspectionTemplateResponse' {excludes} -> excludes) (\s@GetSensitivityInspectionTemplateResponse' {} a -> s {excludes = a} :: GetSensitivityInspectionTemplateResponse)

-- | The allow lists, custom data identifiers, and managed data identifiers
-- that are included (used) when analyzing data.
getSensitivityInspectionTemplateResponse_includes :: Lens.Lens' GetSensitivityInspectionTemplateResponse (Prelude.Maybe SensitivityInspectionTemplateIncludes)
getSensitivityInspectionTemplateResponse_includes = Lens.lens (\GetSensitivityInspectionTemplateResponse' {includes} -> includes) (\s@GetSensitivityInspectionTemplateResponse' {} a -> s {includes = a} :: GetSensitivityInspectionTemplateResponse)

-- | The name of the template.
getSensitivityInspectionTemplateResponse_name :: Lens.Lens' GetSensitivityInspectionTemplateResponse (Prelude.Maybe Prelude.Text)
getSensitivityInspectionTemplateResponse_name = Lens.lens (\GetSensitivityInspectionTemplateResponse' {name} -> name) (\s@GetSensitivityInspectionTemplateResponse' {} a -> s {name = a} :: GetSensitivityInspectionTemplateResponse)

-- | The unique identifier for the template.
getSensitivityInspectionTemplateResponse_sensitivityInspectionTemplateId :: Lens.Lens' GetSensitivityInspectionTemplateResponse (Prelude.Maybe Prelude.Text)
getSensitivityInspectionTemplateResponse_sensitivityInspectionTemplateId = Lens.lens (\GetSensitivityInspectionTemplateResponse' {sensitivityInspectionTemplateId} -> sensitivityInspectionTemplateId) (\s@GetSensitivityInspectionTemplateResponse' {} a -> s {sensitivityInspectionTemplateId = a} :: GetSensitivityInspectionTemplateResponse)

-- | The response's http status code.
getSensitivityInspectionTemplateResponse_httpStatus :: Lens.Lens' GetSensitivityInspectionTemplateResponse Prelude.Int
getSensitivityInspectionTemplateResponse_httpStatus = Lens.lens (\GetSensitivityInspectionTemplateResponse' {httpStatus} -> httpStatus) (\s@GetSensitivityInspectionTemplateResponse' {} a -> s {httpStatus = a} :: GetSensitivityInspectionTemplateResponse)

instance
  Prelude.NFData
    GetSensitivityInspectionTemplateResponse
  where
  rnf GetSensitivityInspectionTemplateResponse' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf excludes `Prelude.seq`
        Prelude.rnf includes `Prelude.seq`
          Prelude.rnf name `Prelude.seq`
            Prelude.rnf sensitivityInspectionTemplateId `Prelude.seq`
              Prelude.rnf httpStatus
