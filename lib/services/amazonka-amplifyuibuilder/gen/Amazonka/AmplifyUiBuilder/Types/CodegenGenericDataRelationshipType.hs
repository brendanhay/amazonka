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
-- Module      : Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataRelationshipType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.CodegenGenericDataRelationshipType where

import Amazonka.AmplifyUiBuilder.Types.GenericDataRelationshipType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the relationship between generic data models.
--
-- /See:/ 'newCodegenGenericDataRelationshipType' smart constructor.
data CodegenGenericDataRelationshipType = CodegenGenericDataRelationshipType'
  { -- | The associated fields of the data relationship.
    associatedFields :: Prelude.Maybe [Prelude.Text],
    -- | The value of the @belongsTo@ field on the related data model.
    belongsToFieldOnRelatedModel :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the relationship can unlink the associated model.
    canUnlinkAssociatedModel :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether the @\@index@ directive is supported for a @hasMany@
    -- data relationship.
    isHasManyIndex :: Prelude.Maybe Prelude.Bool,
    -- | The name of the related join field in the data relationship.
    relatedJoinFieldName :: Prelude.Maybe Prelude.Text,
    -- | The name of the related join table in the data relationship.
    relatedJoinTableName :: Prelude.Maybe Prelude.Text,
    -- | The related model fields in the data relationship.
    relatedModelFields :: Prelude.Maybe [Prelude.Text],
    -- | The data relationship type.
    type' :: GenericDataRelationshipType,
    -- | The name of the related model in the data relationship.
    relatedModelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CodegenGenericDataRelationshipType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedFields', 'codegenGenericDataRelationshipType_associatedFields' - The associated fields of the data relationship.
--
-- 'belongsToFieldOnRelatedModel', 'codegenGenericDataRelationshipType_belongsToFieldOnRelatedModel' - The value of the @belongsTo@ field on the related data model.
--
-- 'canUnlinkAssociatedModel', 'codegenGenericDataRelationshipType_canUnlinkAssociatedModel' - Specifies whether the relationship can unlink the associated model.
--
-- 'isHasManyIndex', 'codegenGenericDataRelationshipType_isHasManyIndex' - Specifies whether the @\@index@ directive is supported for a @hasMany@
-- data relationship.
--
-- 'relatedJoinFieldName', 'codegenGenericDataRelationshipType_relatedJoinFieldName' - The name of the related join field in the data relationship.
--
-- 'relatedJoinTableName', 'codegenGenericDataRelationshipType_relatedJoinTableName' - The name of the related join table in the data relationship.
--
-- 'relatedModelFields', 'codegenGenericDataRelationshipType_relatedModelFields' - The related model fields in the data relationship.
--
-- 'type'', 'codegenGenericDataRelationshipType_type' - The data relationship type.
--
-- 'relatedModelName', 'codegenGenericDataRelationshipType_relatedModelName' - The name of the related model in the data relationship.
newCodegenGenericDataRelationshipType ::
  -- | 'type''
  GenericDataRelationshipType ->
  -- | 'relatedModelName'
  Prelude.Text ->
  CodegenGenericDataRelationshipType
newCodegenGenericDataRelationshipType
  pType_
  pRelatedModelName_ =
    CodegenGenericDataRelationshipType'
      { associatedFields =
          Prelude.Nothing,
        belongsToFieldOnRelatedModel =
          Prelude.Nothing,
        canUnlinkAssociatedModel =
          Prelude.Nothing,
        isHasManyIndex = Prelude.Nothing,
        relatedJoinFieldName = Prelude.Nothing,
        relatedJoinTableName = Prelude.Nothing,
        relatedModelFields = Prelude.Nothing,
        type' = pType_,
        relatedModelName = pRelatedModelName_
      }

-- | The associated fields of the data relationship.
codegenGenericDataRelationshipType_associatedFields :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe [Prelude.Text])
codegenGenericDataRelationshipType_associatedFields = Lens.lens (\CodegenGenericDataRelationshipType' {associatedFields} -> associatedFields) (\s@CodegenGenericDataRelationshipType' {} a -> s {associatedFields = a} :: CodegenGenericDataRelationshipType) Prelude.. Lens.mapping Lens.coerced

-- | The value of the @belongsTo@ field on the related data model.
codegenGenericDataRelationshipType_belongsToFieldOnRelatedModel :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe Prelude.Text)
codegenGenericDataRelationshipType_belongsToFieldOnRelatedModel = Lens.lens (\CodegenGenericDataRelationshipType' {belongsToFieldOnRelatedModel} -> belongsToFieldOnRelatedModel) (\s@CodegenGenericDataRelationshipType' {} a -> s {belongsToFieldOnRelatedModel = a} :: CodegenGenericDataRelationshipType)

-- | Specifies whether the relationship can unlink the associated model.
codegenGenericDataRelationshipType_canUnlinkAssociatedModel :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe Prelude.Bool)
codegenGenericDataRelationshipType_canUnlinkAssociatedModel = Lens.lens (\CodegenGenericDataRelationshipType' {canUnlinkAssociatedModel} -> canUnlinkAssociatedModel) (\s@CodegenGenericDataRelationshipType' {} a -> s {canUnlinkAssociatedModel = a} :: CodegenGenericDataRelationshipType)

-- | Specifies whether the @\@index@ directive is supported for a @hasMany@
-- data relationship.
codegenGenericDataRelationshipType_isHasManyIndex :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe Prelude.Bool)
codegenGenericDataRelationshipType_isHasManyIndex = Lens.lens (\CodegenGenericDataRelationshipType' {isHasManyIndex} -> isHasManyIndex) (\s@CodegenGenericDataRelationshipType' {} a -> s {isHasManyIndex = a} :: CodegenGenericDataRelationshipType)

-- | The name of the related join field in the data relationship.
codegenGenericDataRelationshipType_relatedJoinFieldName :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe Prelude.Text)
codegenGenericDataRelationshipType_relatedJoinFieldName = Lens.lens (\CodegenGenericDataRelationshipType' {relatedJoinFieldName} -> relatedJoinFieldName) (\s@CodegenGenericDataRelationshipType' {} a -> s {relatedJoinFieldName = a} :: CodegenGenericDataRelationshipType)

-- | The name of the related join table in the data relationship.
codegenGenericDataRelationshipType_relatedJoinTableName :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe Prelude.Text)
codegenGenericDataRelationshipType_relatedJoinTableName = Lens.lens (\CodegenGenericDataRelationshipType' {relatedJoinTableName} -> relatedJoinTableName) (\s@CodegenGenericDataRelationshipType' {} a -> s {relatedJoinTableName = a} :: CodegenGenericDataRelationshipType)

-- | The related model fields in the data relationship.
codegenGenericDataRelationshipType_relatedModelFields :: Lens.Lens' CodegenGenericDataRelationshipType (Prelude.Maybe [Prelude.Text])
codegenGenericDataRelationshipType_relatedModelFields = Lens.lens (\CodegenGenericDataRelationshipType' {relatedModelFields} -> relatedModelFields) (\s@CodegenGenericDataRelationshipType' {} a -> s {relatedModelFields = a} :: CodegenGenericDataRelationshipType) Prelude.. Lens.mapping Lens.coerced

-- | The data relationship type.
codegenGenericDataRelationshipType_type :: Lens.Lens' CodegenGenericDataRelationshipType GenericDataRelationshipType
codegenGenericDataRelationshipType_type = Lens.lens (\CodegenGenericDataRelationshipType' {type'} -> type') (\s@CodegenGenericDataRelationshipType' {} a -> s {type' = a} :: CodegenGenericDataRelationshipType)

-- | The name of the related model in the data relationship.
codegenGenericDataRelationshipType_relatedModelName :: Lens.Lens' CodegenGenericDataRelationshipType Prelude.Text
codegenGenericDataRelationshipType_relatedModelName = Lens.lens (\CodegenGenericDataRelationshipType' {relatedModelName} -> relatedModelName) (\s@CodegenGenericDataRelationshipType' {} a -> s {relatedModelName = a} :: CodegenGenericDataRelationshipType)

instance
  Data.FromJSON
    CodegenGenericDataRelationshipType
  where
  parseJSON =
    Data.withObject
      "CodegenGenericDataRelationshipType"
      ( \x ->
          CodegenGenericDataRelationshipType'
            Prelude.<$> ( x
                            Data..:? "associatedFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "belongsToFieldOnRelatedModel")
            Prelude.<*> (x Data..:? "canUnlinkAssociatedModel")
            Prelude.<*> (x Data..:? "isHasManyIndex")
            Prelude.<*> (x Data..:? "relatedJoinFieldName")
            Prelude.<*> (x Data..:? "relatedJoinTableName")
            Prelude.<*> ( x
                            Data..:? "relatedModelFields"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "type")
            Prelude.<*> (x Data..: "relatedModelName")
      )

instance
  Prelude.Hashable
    CodegenGenericDataRelationshipType
  where
  hashWithSalt
    _salt
    CodegenGenericDataRelationshipType' {..} =
      _salt
        `Prelude.hashWithSalt` associatedFields
        `Prelude.hashWithSalt` belongsToFieldOnRelatedModel
        `Prelude.hashWithSalt` canUnlinkAssociatedModel
        `Prelude.hashWithSalt` isHasManyIndex
        `Prelude.hashWithSalt` relatedJoinFieldName
        `Prelude.hashWithSalt` relatedJoinTableName
        `Prelude.hashWithSalt` relatedModelFields
        `Prelude.hashWithSalt` type'
        `Prelude.hashWithSalt` relatedModelName

instance
  Prelude.NFData
    CodegenGenericDataRelationshipType
  where
  rnf CodegenGenericDataRelationshipType' {..} =
    Prelude.rnf associatedFields
      `Prelude.seq` Prelude.rnf belongsToFieldOnRelatedModel
      `Prelude.seq` Prelude.rnf canUnlinkAssociatedModel
      `Prelude.seq` Prelude.rnf isHasManyIndex
      `Prelude.seq` Prelude.rnf relatedJoinFieldName
      `Prelude.seq` Prelude.rnf relatedJoinTableName
      `Prelude.seq` Prelude.rnf relatedModelFields
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf relatedModelName

instance
  Data.ToJSON
    CodegenGenericDataRelationshipType
  where
  toJSON CodegenGenericDataRelationshipType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("associatedFields" Data..=)
              Prelude.<$> associatedFields,
            ("belongsToFieldOnRelatedModel" Data..=)
              Prelude.<$> belongsToFieldOnRelatedModel,
            ("canUnlinkAssociatedModel" Data..=)
              Prelude.<$> canUnlinkAssociatedModel,
            ("isHasManyIndex" Data..=)
              Prelude.<$> isHasManyIndex,
            ("relatedJoinFieldName" Data..=)
              Prelude.<$> relatedJoinFieldName,
            ("relatedJoinTableName" Data..=)
              Prelude.<$> relatedJoinTableName,
            ("relatedModelFields" Data..=)
              Prelude.<$> relatedModelFields,
            Prelude.Just ("type" Data..= type'),
            Prelude.Just
              ("relatedModelName" Data..= relatedModelName)
          ]
      )
