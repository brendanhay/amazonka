{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenJobGenericDataSchema
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenJobGenericDataSchema where

import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataEnum
import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataModel
import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataNonModel
import Amazonka.AmplifyUiBuilder.Types.CodegenJobGenericDataSourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the data schema for a code generation job.
--
-- /See:/ 'newCodegenJobGenericDataSchema' smart constructor.
data CodegenJobGenericDataSchema = CodegenJobGenericDataSchema'
  { -- | The type of the data source for the schema. Currently, the only valid
    -- value is an Amplify @DataStore@.
    dataSourceType :: CodegenJobGenericDataSourceType,
    -- | The name of a @CodegenGenericDataModel@.
    models :: Prelude.HashMap Prelude.Text CodegenGenericDataModel,
    -- | The name of a @CodegenGenericDataEnum@.
    enums :: Prelude.HashMap Prelude.Text CodegenGenericDataEnum,
    -- | The name of a @CodegenGenericDataNonModel@.
    nonModels :: Prelude.HashMap Prelude.Text CodegenGenericDataNonModel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenJobGenericDataSchema' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSourceType', 'codegenJobGenericDataSchema_dataSourceType' - The type of the data source for the schema. Currently, the only valid
-- value is an Amplify @DataStore@.
--
-- 'models', 'codegenJobGenericDataSchema_models' - The name of a @CodegenGenericDataModel@.
--
-- 'enums', 'codegenJobGenericDataSchema_enums' - The name of a @CodegenGenericDataEnum@.
--
-- 'nonModels', 'codegenJobGenericDataSchema_nonModels' - The name of a @CodegenGenericDataNonModel@.
newCodegenJobGenericDataSchema ::
  -- | 'dataSourceType'
  CodegenJobGenericDataSourceType ->
  CodegenJobGenericDataSchema
newCodegenJobGenericDataSchema pDataSourceType_ =
  CodegenJobGenericDataSchema'
    { dataSourceType =
        pDataSourceType_,
      models = Prelude.mempty,
      enums = Prelude.mempty,
      nonModels = Prelude.mempty
    }

-- | The type of the data source for the schema. Currently, the only valid
-- value is an Amplify @DataStore@.
codegenJobGenericDataSchema_dataSourceType :: Lens.Lens' CodegenJobGenericDataSchema CodegenJobGenericDataSourceType
codegenJobGenericDataSchema_dataSourceType = Lens.lens (\CodegenJobGenericDataSchema' {dataSourceType} -> dataSourceType) (\s@CodegenJobGenericDataSchema' {} a -> s {dataSourceType = a} :: CodegenJobGenericDataSchema)

-- | The name of a @CodegenGenericDataModel@.
codegenJobGenericDataSchema_models :: Lens.Lens' CodegenJobGenericDataSchema (Prelude.HashMap Prelude.Text CodegenGenericDataModel)
codegenJobGenericDataSchema_models = Lens.lens (\CodegenJobGenericDataSchema' {models} -> models) (\s@CodegenJobGenericDataSchema' {} a -> s {models = a} :: CodegenJobGenericDataSchema) Prelude.. Lens.coerced

-- | The name of a @CodegenGenericDataEnum@.
codegenJobGenericDataSchema_enums :: Lens.Lens' CodegenJobGenericDataSchema (Prelude.HashMap Prelude.Text CodegenGenericDataEnum)
codegenJobGenericDataSchema_enums = Lens.lens (\CodegenJobGenericDataSchema' {enums} -> enums) (\s@CodegenJobGenericDataSchema' {} a -> s {enums = a} :: CodegenJobGenericDataSchema) Prelude.. Lens.coerced

-- | The name of a @CodegenGenericDataNonModel@.
codegenJobGenericDataSchema_nonModels :: Lens.Lens' CodegenJobGenericDataSchema (Prelude.HashMap Prelude.Text CodegenGenericDataNonModel)
codegenJobGenericDataSchema_nonModels = Lens.lens (\CodegenJobGenericDataSchema' {nonModels} -> nonModels) (\s@CodegenJobGenericDataSchema' {} a -> s {nonModels = a} :: CodegenJobGenericDataSchema) Prelude.. Lens.coerced

instance Data.FromJSON CodegenJobGenericDataSchema where
  parseJSON =
    Data.withObject
      "CodegenJobGenericDataSchema"
      ( \x ->
          CodegenJobGenericDataSchema'
            Prelude.<$> (x Data..: "dataSourceType")
            Prelude.<*> (x Data..:? "models" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "enums" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "nonModels" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CodegenJobGenericDataSchema where
  hashWithSalt _salt CodegenJobGenericDataSchema' {..} =
    _salt
      `Prelude.hashWithSalt` dataSourceType
      `Prelude.hashWithSalt` models
      `Prelude.hashWithSalt` enums
      `Prelude.hashWithSalt` nonModels

instance Prelude.NFData CodegenJobGenericDataSchema where
  rnf CodegenJobGenericDataSchema' {..} =
    Prelude.rnf dataSourceType
      `Prelude.seq` Prelude.rnf models
      `Prelude.seq` Prelude.rnf enums
      `Prelude.seq` Prelude.rnf nonModels

instance Data.ToJSON CodegenJobGenericDataSchema where
  toJSON CodegenJobGenericDataSchema' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("dataSourceType" Data..= dataSourceType),
            Prelude.Just ("models" Data..= models),
            Prelude.Just ("enums" Data..= enums),
            Prelude.Just ("nonModels" Data..= nonModels)
          ]
      )
