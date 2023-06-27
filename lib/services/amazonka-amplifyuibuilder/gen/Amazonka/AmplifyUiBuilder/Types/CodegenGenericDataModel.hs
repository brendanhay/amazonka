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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataModel
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataModel where

import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataField
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a model in a generic data schema.
--
-- /See:/ 'newCodegenGenericDataModel' smart constructor.
data CodegenGenericDataModel = CodegenGenericDataModel'
  { -- | Specifies whether the generic data model is a join table.
    isJoinTable :: Prelude.Maybe Prelude.Bool,
    -- | The fields in the generic data model.
    fields :: Prelude.HashMap Prelude.Text CodegenGenericDataField,
    -- | The primary keys of the generic data model.
    primaryKeys :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenGenericDataModel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isJoinTable', 'codegenGenericDataModel_isJoinTable' - Specifies whether the generic data model is a join table.
--
-- 'fields', 'codegenGenericDataModel_fields' - The fields in the generic data model.
--
-- 'primaryKeys', 'codegenGenericDataModel_primaryKeys' - The primary keys of the generic data model.
newCodegenGenericDataModel ::
  CodegenGenericDataModel
newCodegenGenericDataModel =
  CodegenGenericDataModel'
    { isJoinTable =
        Prelude.Nothing,
      fields = Prelude.mempty,
      primaryKeys = Prelude.mempty
    }

-- | Specifies whether the generic data model is a join table.
codegenGenericDataModel_isJoinTable :: Lens.Lens' CodegenGenericDataModel (Prelude.Maybe Prelude.Bool)
codegenGenericDataModel_isJoinTable = Lens.lens (\CodegenGenericDataModel' {isJoinTable} -> isJoinTable) (\s@CodegenGenericDataModel' {} a -> s {isJoinTable = a} :: CodegenGenericDataModel)

-- | The fields in the generic data model.
codegenGenericDataModel_fields :: Lens.Lens' CodegenGenericDataModel (Prelude.HashMap Prelude.Text CodegenGenericDataField)
codegenGenericDataModel_fields = Lens.lens (\CodegenGenericDataModel' {fields} -> fields) (\s@CodegenGenericDataModel' {} a -> s {fields = a} :: CodegenGenericDataModel) Prelude.. Lens.coerced

-- | The primary keys of the generic data model.
codegenGenericDataModel_primaryKeys :: Lens.Lens' CodegenGenericDataModel [Prelude.Text]
codegenGenericDataModel_primaryKeys = Lens.lens (\CodegenGenericDataModel' {primaryKeys} -> primaryKeys) (\s@CodegenGenericDataModel' {} a -> s {primaryKeys = a} :: CodegenGenericDataModel) Prelude.. Lens.coerced

instance Data.FromJSON CodegenGenericDataModel where
  parseJSON =
    Data.withObject
      "CodegenGenericDataModel"
      ( \x ->
          CodegenGenericDataModel'
            Prelude.<$> (x Data..:? "isJoinTable")
            Prelude.<*> (x Data..:? "fields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "primaryKeys" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable CodegenGenericDataModel where
  hashWithSalt _salt CodegenGenericDataModel' {..} =
    _salt
      `Prelude.hashWithSalt` isJoinTable
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` primaryKeys

instance Prelude.NFData CodegenGenericDataModel where
  rnf CodegenGenericDataModel' {..} =
    Prelude.rnf isJoinTable
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf primaryKeys

instance Data.ToJSON CodegenGenericDataModel where
  toJSON CodegenGenericDataModel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("isJoinTable" Data..=) Prelude.<$> isJoinTable,
            Prelude.Just ("fields" Data..= fields),
            Prelude.Just ("primaryKeys" Data..= primaryKeys)
          ]
      )
