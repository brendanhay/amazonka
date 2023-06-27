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
-- Module      : Amazonka.CleanRooms.CreateConfiguredTable
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new configured table resource.
module Amazonka.CleanRooms.CreateConfiguredTable
  ( -- * Creating a Request
    CreateConfiguredTable (..),
    newCreateConfiguredTable,

    -- * Request Lenses
    createConfiguredTable_description,
    createConfiguredTable_tags,
    createConfiguredTable_name,
    createConfiguredTable_tableReference,
    createConfiguredTable_allowedColumns,
    createConfiguredTable_analysisMethod,

    -- * Destructuring the Response
    CreateConfiguredTableResponse (..),
    newCreateConfiguredTableResponse,

    -- * Response Lenses
    createConfiguredTableResponse_httpStatus,
    createConfiguredTableResponse_configuredTable,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfiguredTable' smart constructor.
data CreateConfiguredTable = CreateConfiguredTable'
  { -- | A description for the configured table.
    description :: Prelude.Maybe Prelude.Text,
    -- | An optional label that you can assign to a resource when you create it.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. When you use tagging, you can also use tag-based access control
    -- in IAM policies to control access to this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the configured table.
    name :: Prelude.Text,
    -- | A reference to the AWS Glue table being configured.
    tableReference :: TableReference,
    -- | The columns of the underlying table that can be used by collaborations
    -- or analysis rules.
    allowedColumns :: Prelude.NonEmpty Prelude.Text,
    -- | The analysis method for the configured tables. The only valid value is
    -- currently \`DIRECT_QUERY\`.
    analysisMethod :: AnalysisMethod
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguredTable' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createConfiguredTable_description' - A description for the configured table.
--
-- 'tags', 'createConfiguredTable_tags' - An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
--
-- 'name', 'createConfiguredTable_name' - The name of the configured table.
--
-- 'tableReference', 'createConfiguredTable_tableReference' - A reference to the AWS Glue table being configured.
--
-- 'allowedColumns', 'createConfiguredTable_allowedColumns' - The columns of the underlying table that can be used by collaborations
-- or analysis rules.
--
-- 'analysisMethod', 'createConfiguredTable_analysisMethod' - The analysis method for the configured tables. The only valid value is
-- currently \`DIRECT_QUERY\`.
newCreateConfiguredTable ::
  -- | 'name'
  Prelude.Text ->
  -- | 'tableReference'
  TableReference ->
  -- | 'allowedColumns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'analysisMethod'
  AnalysisMethod ->
  CreateConfiguredTable
newCreateConfiguredTable
  pName_
  pTableReference_
  pAllowedColumns_
  pAnalysisMethod_ =
    CreateConfiguredTable'
      { description =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        tableReference = pTableReference_,
        allowedColumns =
          Lens.coerced Lens.# pAllowedColumns_,
        analysisMethod = pAnalysisMethod_
      }

-- | A description for the configured table.
createConfiguredTable_description :: Lens.Lens' CreateConfiguredTable (Prelude.Maybe Prelude.Text)
createConfiguredTable_description = Lens.lens (\CreateConfiguredTable' {description} -> description) (\s@CreateConfiguredTable' {} a -> s {description = a} :: CreateConfiguredTable)

-- | An optional label that you can assign to a resource when you create it.
-- Each tag consists of a key and an optional value, both of which you
-- define. When you use tagging, you can also use tag-based access control
-- in IAM policies to control access to this resource.
createConfiguredTable_tags :: Lens.Lens' CreateConfiguredTable (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createConfiguredTable_tags = Lens.lens (\CreateConfiguredTable' {tags} -> tags) (\s@CreateConfiguredTable' {} a -> s {tags = a} :: CreateConfiguredTable) Prelude.. Lens.mapping Lens.coerced

-- | The name of the configured table.
createConfiguredTable_name :: Lens.Lens' CreateConfiguredTable Prelude.Text
createConfiguredTable_name = Lens.lens (\CreateConfiguredTable' {name} -> name) (\s@CreateConfiguredTable' {} a -> s {name = a} :: CreateConfiguredTable)

-- | A reference to the AWS Glue table being configured.
createConfiguredTable_tableReference :: Lens.Lens' CreateConfiguredTable TableReference
createConfiguredTable_tableReference = Lens.lens (\CreateConfiguredTable' {tableReference} -> tableReference) (\s@CreateConfiguredTable' {} a -> s {tableReference = a} :: CreateConfiguredTable)

-- | The columns of the underlying table that can be used by collaborations
-- or analysis rules.
createConfiguredTable_allowedColumns :: Lens.Lens' CreateConfiguredTable (Prelude.NonEmpty Prelude.Text)
createConfiguredTable_allowedColumns = Lens.lens (\CreateConfiguredTable' {allowedColumns} -> allowedColumns) (\s@CreateConfiguredTable' {} a -> s {allowedColumns = a} :: CreateConfiguredTable) Prelude.. Lens.coerced

-- | The analysis method for the configured tables. The only valid value is
-- currently \`DIRECT_QUERY\`.
createConfiguredTable_analysisMethod :: Lens.Lens' CreateConfiguredTable AnalysisMethod
createConfiguredTable_analysisMethod = Lens.lens (\CreateConfiguredTable' {analysisMethod} -> analysisMethod) (\s@CreateConfiguredTable' {} a -> s {analysisMethod = a} :: CreateConfiguredTable)

instance Core.AWSRequest CreateConfiguredTable where
  type
    AWSResponse CreateConfiguredTable =
      CreateConfiguredTableResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfiguredTableResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "configuredTable")
      )

instance Prelude.Hashable CreateConfiguredTable where
  hashWithSalt _salt CreateConfiguredTable' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tableReference
      `Prelude.hashWithSalt` allowedColumns
      `Prelude.hashWithSalt` analysisMethod

instance Prelude.NFData CreateConfiguredTable where
  rnf CreateConfiguredTable' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tableReference
      `Prelude.seq` Prelude.rnf allowedColumns
      `Prelude.seq` Prelude.rnf analysisMethod

instance Data.ToHeaders CreateConfiguredTable where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateConfiguredTable where
  toJSON CreateConfiguredTable' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("name" Data..= name),
            Prelude.Just
              ("tableReference" Data..= tableReference),
            Prelude.Just
              ("allowedColumns" Data..= allowedColumns),
            Prelude.Just
              ("analysisMethod" Data..= analysisMethod)
          ]
      )

instance Data.ToPath CreateConfiguredTable where
  toPath = Prelude.const "/configuredTables"

instance Data.ToQuery CreateConfiguredTable where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfiguredTableResponse' smart constructor.
data CreateConfiguredTableResponse = CreateConfiguredTableResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The created configured table.
    configuredTable :: ConfiguredTable
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguredTableResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfiguredTableResponse_httpStatus' - The response's http status code.
--
-- 'configuredTable', 'createConfiguredTableResponse_configuredTable' - The created configured table.
newCreateConfiguredTableResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'configuredTable'
  ConfiguredTable ->
  CreateConfiguredTableResponse
newCreateConfiguredTableResponse
  pHttpStatus_
  pConfiguredTable_ =
    CreateConfiguredTableResponse'
      { httpStatus =
          pHttpStatus_,
        configuredTable = pConfiguredTable_
      }

-- | The response's http status code.
createConfiguredTableResponse_httpStatus :: Lens.Lens' CreateConfiguredTableResponse Prelude.Int
createConfiguredTableResponse_httpStatus = Lens.lens (\CreateConfiguredTableResponse' {httpStatus} -> httpStatus) (\s@CreateConfiguredTableResponse' {} a -> s {httpStatus = a} :: CreateConfiguredTableResponse)

-- | The created configured table.
createConfiguredTableResponse_configuredTable :: Lens.Lens' CreateConfiguredTableResponse ConfiguredTable
createConfiguredTableResponse_configuredTable = Lens.lens (\CreateConfiguredTableResponse' {configuredTable} -> configuredTable) (\s@CreateConfiguredTableResponse' {} a -> s {configuredTable = a} :: CreateConfiguredTableResponse)

instance Prelude.NFData CreateConfiguredTableResponse where
  rnf CreateConfiguredTableResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf configuredTable
