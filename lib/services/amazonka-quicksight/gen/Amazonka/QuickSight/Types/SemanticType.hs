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
-- Module      : Amazonka.QuickSight.Types.SemanticType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SemanticType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents a semantic type.
--
-- /See:/ 'newSemanticType' smart constructor.
data SemanticType = SemanticType'
  { -- | The semantic type falsey cell value.
    falseyCellValue :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The other names or aliases for the false cell value.
    falseyCellValueSynonyms :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The semantic type sub type name.
    subTypeName :: Prelude.Maybe Prelude.Text,
    -- | The semantic type truthy cell value.
    truthyCellValue :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The other names or aliases for the true cell value.
    truthyCellValueSynonyms :: Prelude.Maybe [Data.Sensitive Prelude.Text],
    -- | The semantic type name.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The semantic type parameters.
    typeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SemanticType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'falseyCellValue', 'semanticType_falseyCellValue' - The semantic type falsey cell value.
--
-- 'falseyCellValueSynonyms', 'semanticType_falseyCellValueSynonyms' - The other names or aliases for the false cell value.
--
-- 'subTypeName', 'semanticType_subTypeName' - The semantic type sub type name.
--
-- 'truthyCellValue', 'semanticType_truthyCellValue' - The semantic type truthy cell value.
--
-- 'truthyCellValueSynonyms', 'semanticType_truthyCellValueSynonyms' - The other names or aliases for the true cell value.
--
-- 'typeName', 'semanticType_typeName' - The semantic type name.
--
-- 'typeParameters', 'semanticType_typeParameters' - The semantic type parameters.
newSemanticType ::
  SemanticType
newSemanticType =
  SemanticType'
    { falseyCellValue = Prelude.Nothing,
      falseyCellValueSynonyms = Prelude.Nothing,
      subTypeName = Prelude.Nothing,
      truthyCellValue = Prelude.Nothing,
      truthyCellValueSynonyms = Prelude.Nothing,
      typeName = Prelude.Nothing,
      typeParameters = Prelude.Nothing
    }

-- | The semantic type falsey cell value.
semanticType_falseyCellValue :: Lens.Lens' SemanticType (Prelude.Maybe Prelude.Text)
semanticType_falseyCellValue = Lens.lens (\SemanticType' {falseyCellValue} -> falseyCellValue) (\s@SemanticType' {} a -> s {falseyCellValue = a} :: SemanticType) Prelude.. Lens.mapping Data._Sensitive

-- | The other names or aliases for the false cell value.
semanticType_falseyCellValueSynonyms :: Lens.Lens' SemanticType (Prelude.Maybe [Prelude.Text])
semanticType_falseyCellValueSynonyms = Lens.lens (\SemanticType' {falseyCellValueSynonyms} -> falseyCellValueSynonyms) (\s@SemanticType' {} a -> s {falseyCellValueSynonyms = a} :: SemanticType) Prelude.. Lens.mapping Lens.coerced

-- | The semantic type sub type name.
semanticType_subTypeName :: Lens.Lens' SemanticType (Prelude.Maybe Prelude.Text)
semanticType_subTypeName = Lens.lens (\SemanticType' {subTypeName} -> subTypeName) (\s@SemanticType' {} a -> s {subTypeName = a} :: SemanticType)

-- | The semantic type truthy cell value.
semanticType_truthyCellValue :: Lens.Lens' SemanticType (Prelude.Maybe Prelude.Text)
semanticType_truthyCellValue = Lens.lens (\SemanticType' {truthyCellValue} -> truthyCellValue) (\s@SemanticType' {} a -> s {truthyCellValue = a} :: SemanticType) Prelude.. Lens.mapping Data._Sensitive

-- | The other names or aliases for the true cell value.
semanticType_truthyCellValueSynonyms :: Lens.Lens' SemanticType (Prelude.Maybe [Prelude.Text])
semanticType_truthyCellValueSynonyms = Lens.lens (\SemanticType' {truthyCellValueSynonyms} -> truthyCellValueSynonyms) (\s@SemanticType' {} a -> s {truthyCellValueSynonyms = a} :: SemanticType) Prelude.. Lens.mapping Lens.coerced

-- | The semantic type name.
semanticType_typeName :: Lens.Lens' SemanticType (Prelude.Maybe Prelude.Text)
semanticType_typeName = Lens.lens (\SemanticType' {typeName} -> typeName) (\s@SemanticType' {} a -> s {typeName = a} :: SemanticType)

-- | The semantic type parameters.
semanticType_typeParameters :: Lens.Lens' SemanticType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
semanticType_typeParameters = Lens.lens (\SemanticType' {typeParameters} -> typeParameters) (\s@SemanticType' {} a -> s {typeParameters = a} :: SemanticType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SemanticType where
  parseJSON =
    Data.withObject
      "SemanticType"
      ( \x ->
          SemanticType'
            Prelude.<$> (x Data..:? "FalseyCellValue")
            Prelude.<*> ( x
                            Data..:? "FalseyCellValueSynonyms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "SubTypeName")
            Prelude.<*> (x Data..:? "TruthyCellValue")
            Prelude.<*> ( x
                            Data..:? "TruthyCellValueSynonyms"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "TypeName")
            Prelude.<*> ( x
                            Data..:? "TypeParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SemanticType where
  hashWithSalt _salt SemanticType' {..} =
    _salt
      `Prelude.hashWithSalt` falseyCellValue
      `Prelude.hashWithSalt` falseyCellValueSynonyms
      `Prelude.hashWithSalt` subTypeName
      `Prelude.hashWithSalt` truthyCellValue
      `Prelude.hashWithSalt` truthyCellValueSynonyms
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` typeParameters

instance Prelude.NFData SemanticType where
  rnf SemanticType' {..} =
    Prelude.rnf falseyCellValue
      `Prelude.seq` Prelude.rnf falseyCellValueSynonyms
      `Prelude.seq` Prelude.rnf subTypeName
      `Prelude.seq` Prelude.rnf truthyCellValue
      `Prelude.seq` Prelude.rnf truthyCellValueSynonyms
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf typeParameters

instance Data.ToJSON SemanticType where
  toJSON SemanticType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FalseyCellValue" Data..=)
              Prelude.<$> falseyCellValue,
            ("FalseyCellValueSynonyms" Data..=)
              Prelude.<$> falseyCellValueSynonyms,
            ("SubTypeName" Data..=) Prelude.<$> subTypeName,
            ("TruthyCellValue" Data..=)
              Prelude.<$> truthyCellValue,
            ("TruthyCellValueSynonyms" Data..=)
              Prelude.<$> truthyCellValueSynonyms,
            ("TypeName" Data..=) Prelude.<$> typeName,
            ("TypeParameters" Data..=)
              Prelude.<$> typeParameters
          ]
      )
