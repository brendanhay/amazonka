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
-- Module      : Amazonka.AuditManager.CreateAssessmentFramework
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom framework in Audit Manager.
module Amazonka.AuditManager.CreateAssessmentFramework
  ( -- * Creating a Request
    CreateAssessmentFramework (..),
    newCreateAssessmentFramework,

    -- * Request Lenses
    createAssessmentFramework_tags,
    createAssessmentFramework_description,
    createAssessmentFramework_complianceType,
    createAssessmentFramework_name,
    createAssessmentFramework_controlSets,

    -- * Destructuring the Response
    CreateAssessmentFrameworkResponse (..),
    newCreateAssessmentFrameworkResponse,

    -- * Response Lenses
    createAssessmentFrameworkResponse_framework,
    createAssessmentFrameworkResponse_httpStatus,
  )
where

import Amazonka.AuditManager.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateAssessmentFramework' smart constructor.
data CreateAssessmentFramework = CreateAssessmentFramework'
  { -- | The tags that are associated with the framework.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | An optional description for the new custom framework.
    description :: Prelude.Maybe Prelude.Text,
    -- | The compliance type that the new custom framework supports, such as CIS
    -- or HIPAA.
    complianceType :: Prelude.Maybe Prelude.Text,
    -- | The name of the new custom framework.
    name :: Prelude.Text,
    -- | The control sets that are associated with the framework.
    controlSets :: Prelude.NonEmpty CreateAssessmentFrameworkControlSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentFramework' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createAssessmentFramework_tags' - The tags that are associated with the framework.
--
-- 'description', 'createAssessmentFramework_description' - An optional description for the new custom framework.
--
-- 'complianceType', 'createAssessmentFramework_complianceType' - The compliance type that the new custom framework supports, such as CIS
-- or HIPAA.
--
-- 'name', 'createAssessmentFramework_name' - The name of the new custom framework.
--
-- 'controlSets', 'createAssessmentFramework_controlSets' - The control sets that are associated with the framework.
newCreateAssessmentFramework ::
  -- | 'name'
  Prelude.Text ->
  -- | 'controlSets'
  Prelude.NonEmpty CreateAssessmentFrameworkControlSet ->
  CreateAssessmentFramework
newCreateAssessmentFramework pName_ pControlSets_ =
  CreateAssessmentFramework'
    { tags = Prelude.Nothing,
      description = Prelude.Nothing,
      complianceType = Prelude.Nothing,
      name = pName_,
      controlSets = Lens.coerced Lens.# pControlSets_
    }

-- | The tags that are associated with the framework.
createAssessmentFramework_tags :: Lens.Lens' CreateAssessmentFramework (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createAssessmentFramework_tags = Lens.lens (\CreateAssessmentFramework' {tags} -> tags) (\s@CreateAssessmentFramework' {} a -> s {tags = a} :: CreateAssessmentFramework) Prelude.. Lens.mapping Lens.coerced

-- | An optional description for the new custom framework.
createAssessmentFramework_description :: Lens.Lens' CreateAssessmentFramework (Prelude.Maybe Prelude.Text)
createAssessmentFramework_description = Lens.lens (\CreateAssessmentFramework' {description} -> description) (\s@CreateAssessmentFramework' {} a -> s {description = a} :: CreateAssessmentFramework)

-- | The compliance type that the new custom framework supports, such as CIS
-- or HIPAA.
createAssessmentFramework_complianceType :: Lens.Lens' CreateAssessmentFramework (Prelude.Maybe Prelude.Text)
createAssessmentFramework_complianceType = Lens.lens (\CreateAssessmentFramework' {complianceType} -> complianceType) (\s@CreateAssessmentFramework' {} a -> s {complianceType = a} :: CreateAssessmentFramework)

-- | The name of the new custom framework.
createAssessmentFramework_name :: Lens.Lens' CreateAssessmentFramework Prelude.Text
createAssessmentFramework_name = Lens.lens (\CreateAssessmentFramework' {name} -> name) (\s@CreateAssessmentFramework' {} a -> s {name = a} :: CreateAssessmentFramework)

-- | The control sets that are associated with the framework.
createAssessmentFramework_controlSets :: Lens.Lens' CreateAssessmentFramework (Prelude.NonEmpty CreateAssessmentFrameworkControlSet)
createAssessmentFramework_controlSets = Lens.lens (\CreateAssessmentFramework' {controlSets} -> controlSets) (\s@CreateAssessmentFramework' {} a -> s {controlSets = a} :: CreateAssessmentFramework) Prelude.. Lens.coerced

instance Core.AWSRequest CreateAssessmentFramework where
  type
    AWSResponse CreateAssessmentFramework =
      CreateAssessmentFrameworkResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAssessmentFrameworkResponse'
            Prelude.<$> (x Data..?> "framework")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateAssessmentFramework where
  hashWithSalt _salt CreateAssessmentFramework' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` complianceType
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` controlSets

instance Prelude.NFData CreateAssessmentFramework where
  rnf CreateAssessmentFramework' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf complianceType
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf controlSets

instance Data.ToHeaders CreateAssessmentFramework where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAssessmentFramework where
  toJSON CreateAssessmentFramework' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tags" Data..=) Prelude.<$> tags,
            ("description" Data..=) Prelude.<$> description,
            ("complianceType" Data..=)
              Prelude.<$> complianceType,
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("controlSets" Data..= controlSets)
          ]
      )

instance Data.ToPath CreateAssessmentFramework where
  toPath = Prelude.const "/assessmentFrameworks"

instance Data.ToQuery CreateAssessmentFramework where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAssessmentFrameworkResponse' smart constructor.
data CreateAssessmentFrameworkResponse = CreateAssessmentFrameworkResponse'
  { -- | The name of the new framework that the @CreateAssessmentFramework@ API
    -- returned.
    framework :: Prelude.Maybe Framework,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAssessmentFrameworkResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'framework', 'createAssessmentFrameworkResponse_framework' - The name of the new framework that the @CreateAssessmentFramework@ API
-- returned.
--
-- 'httpStatus', 'createAssessmentFrameworkResponse_httpStatus' - The response's http status code.
newCreateAssessmentFrameworkResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateAssessmentFrameworkResponse
newCreateAssessmentFrameworkResponse pHttpStatus_ =
  CreateAssessmentFrameworkResponse'
    { framework =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the new framework that the @CreateAssessmentFramework@ API
-- returned.
createAssessmentFrameworkResponse_framework :: Lens.Lens' CreateAssessmentFrameworkResponse (Prelude.Maybe Framework)
createAssessmentFrameworkResponse_framework = Lens.lens (\CreateAssessmentFrameworkResponse' {framework} -> framework) (\s@CreateAssessmentFrameworkResponse' {} a -> s {framework = a} :: CreateAssessmentFrameworkResponse)

-- | The response's http status code.
createAssessmentFrameworkResponse_httpStatus :: Lens.Lens' CreateAssessmentFrameworkResponse Prelude.Int
createAssessmentFrameworkResponse_httpStatus = Lens.lens (\CreateAssessmentFrameworkResponse' {httpStatus} -> httpStatus) (\s@CreateAssessmentFrameworkResponse' {} a -> s {httpStatus = a} :: CreateAssessmentFrameworkResponse)

instance
  Prelude.NFData
    CreateAssessmentFrameworkResponse
  where
  rnf CreateAssessmentFrameworkResponse' {..} =
    Prelude.rnf framework
      `Prelude.seq` Prelude.rnf httpStatus
