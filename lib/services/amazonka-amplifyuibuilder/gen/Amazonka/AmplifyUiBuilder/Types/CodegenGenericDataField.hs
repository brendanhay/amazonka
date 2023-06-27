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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataField
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataField where

import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataFieldDataType
import Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataRelationshipType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a field in a generic data schema.
--
-- /See:/ 'newCodegenGenericDataField' smart constructor.
data CodegenGenericDataField = CodegenGenericDataField'
  { -- | The relationship of the generic data schema.
    relationship :: Prelude.Maybe CodegenGenericDataRelationshipType,
    -- | The data type for the generic data field.
    dataType :: CodegenGenericDataFieldDataType,
    -- | The value of the data type for the generic data field.
    dataTypeValue :: Prelude.Text,
    -- | Specifies whether the generic data field is required.
    required :: Prelude.Bool,
    -- | Specifies whether the generic data field is read-only.
    readOnly :: Prelude.Bool,
    -- | Specifies whether the generic data field is an array.
    isArray :: Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenGenericDataField' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationship', 'codegenGenericDataField_relationship' - The relationship of the generic data schema.
--
-- 'dataType', 'codegenGenericDataField_dataType' - The data type for the generic data field.
--
-- 'dataTypeValue', 'codegenGenericDataField_dataTypeValue' - The value of the data type for the generic data field.
--
-- 'required', 'codegenGenericDataField_required' - Specifies whether the generic data field is required.
--
-- 'readOnly', 'codegenGenericDataField_readOnly' - Specifies whether the generic data field is read-only.
--
-- 'isArray', 'codegenGenericDataField_isArray' - Specifies whether the generic data field is an array.
newCodegenGenericDataField ::
  -- | 'dataType'
  CodegenGenericDataFieldDataType ->
  -- | 'dataTypeValue'
  Prelude.Text ->
  -- | 'required'
  Prelude.Bool ->
  -- | 'readOnly'
  Prelude.Bool ->
  -- | 'isArray'
  Prelude.Bool ->
  CodegenGenericDataField
newCodegenGenericDataField
  pDataType_
  pDataTypeValue_
  pRequired_
  pReadOnly_
  pIsArray_ =
    CodegenGenericDataField'
      { relationship =
          Prelude.Nothing,
        dataType = pDataType_,
        dataTypeValue = pDataTypeValue_,
        required = pRequired_,
        readOnly = pReadOnly_,
        isArray = pIsArray_
      }

-- | The relationship of the generic data schema.
codegenGenericDataField_relationship :: Lens.Lens' CodegenGenericDataField (Prelude.Maybe CodegenGenericDataRelationshipType)
codegenGenericDataField_relationship = Lens.lens (\CodegenGenericDataField' {relationship} -> relationship) (\s@CodegenGenericDataField' {} a -> s {relationship = a} :: CodegenGenericDataField)

-- | The data type for the generic data field.
codegenGenericDataField_dataType :: Lens.Lens' CodegenGenericDataField CodegenGenericDataFieldDataType
codegenGenericDataField_dataType = Lens.lens (\CodegenGenericDataField' {dataType} -> dataType) (\s@CodegenGenericDataField' {} a -> s {dataType = a} :: CodegenGenericDataField)

-- | The value of the data type for the generic data field.
codegenGenericDataField_dataTypeValue :: Lens.Lens' CodegenGenericDataField Prelude.Text
codegenGenericDataField_dataTypeValue = Lens.lens (\CodegenGenericDataField' {dataTypeValue} -> dataTypeValue) (\s@CodegenGenericDataField' {} a -> s {dataTypeValue = a} :: CodegenGenericDataField)

-- | Specifies whether the generic data field is required.
codegenGenericDataField_required :: Lens.Lens' CodegenGenericDataField Prelude.Bool
codegenGenericDataField_required = Lens.lens (\CodegenGenericDataField' {required} -> required) (\s@CodegenGenericDataField' {} a -> s {required = a} :: CodegenGenericDataField)

-- | Specifies whether the generic data field is read-only.
codegenGenericDataField_readOnly :: Lens.Lens' CodegenGenericDataField Prelude.Bool
codegenGenericDataField_readOnly = Lens.lens (\CodegenGenericDataField' {readOnly} -> readOnly) (\s@CodegenGenericDataField' {} a -> s {readOnly = a} :: CodegenGenericDataField)

-- | Specifies whether the generic data field is an array.
codegenGenericDataField_isArray :: Lens.Lens' CodegenGenericDataField Prelude.Bool
codegenGenericDataField_isArray = Lens.lens (\CodegenGenericDataField' {isArray} -> isArray) (\s@CodegenGenericDataField' {} a -> s {isArray = a} :: CodegenGenericDataField)

instance Data.FromJSON CodegenGenericDataField where
  parseJSON =
    Data.withObject
      "CodegenGenericDataField"
      ( \x ->
          CodegenGenericDataField'
            Prelude.<$> (x Data..:? "relationship")
            Prelude.<*> (x Data..: "dataType")
            Prelude.<*> (x Data..: "dataTypeValue")
            Prelude.<*> (x Data..: "required")
            Prelude.<*> (x Data..: "readOnly")
            Prelude.<*> (x Data..: "isArray")
      )

instance Prelude.Hashable CodegenGenericDataField where
  hashWithSalt _salt CodegenGenericDataField' {..} =
    _salt
      `Prelude.hashWithSalt` relationship
      `Prelude.hashWithSalt` dataType
      `Prelude.hashWithSalt` dataTypeValue
      `Prelude.hashWithSalt` required
      `Prelude.hashWithSalt` readOnly
      `Prelude.hashWithSalt` isArray

instance Prelude.NFData CodegenGenericDataField where
  rnf CodegenGenericDataField' {..} =
    Prelude.rnf relationship
      `Prelude.seq` Prelude.rnf dataType
      `Prelude.seq` Prelude.rnf dataTypeValue
      `Prelude.seq` Prelude.rnf required
      `Prelude.seq` Prelude.rnf readOnly
      `Prelude.seq` Prelude.rnf isArray

instance Data.ToJSON CodegenGenericDataField where
  toJSON CodegenGenericDataField' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("relationship" Data..=) Prelude.<$> relationship,
            Prelude.Just ("dataType" Data..= dataType),
            Prelude.Just ("dataTypeValue" Data..= dataTypeValue),
            Prelude.Just ("required" Data..= required),
            Prelude.Just ("readOnly" Data..= readOnly),
            Prelude.Just ("isArray" Data..= isArray)
          ]
      )
