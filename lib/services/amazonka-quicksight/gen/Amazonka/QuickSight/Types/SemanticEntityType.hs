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
-- Module      : Amazonka.QuickSight.Types.SemanticEntityType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SemanticEntityType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A structure that represents a semantic entity type.
--
-- /See:/ 'newSemanticEntityType' smart constructor.
data SemanticEntityType = SemanticEntityType'
  { -- | The semantic entity sub type name.
    subTypeName :: Prelude.Maybe Prelude.Text,
    -- | The semantic entity type name.
    typeName :: Prelude.Maybe Prelude.Text,
    -- | The semantic entity type parameters.
    typeParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SemanticEntityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subTypeName', 'semanticEntityType_subTypeName' - The semantic entity sub type name.
--
-- 'typeName', 'semanticEntityType_typeName' - The semantic entity type name.
--
-- 'typeParameters', 'semanticEntityType_typeParameters' - The semantic entity type parameters.
newSemanticEntityType ::
  SemanticEntityType
newSemanticEntityType =
  SemanticEntityType'
    { subTypeName = Prelude.Nothing,
      typeName = Prelude.Nothing,
      typeParameters = Prelude.Nothing
    }

-- | The semantic entity sub type name.
semanticEntityType_subTypeName :: Lens.Lens' SemanticEntityType (Prelude.Maybe Prelude.Text)
semanticEntityType_subTypeName = Lens.lens (\SemanticEntityType' {subTypeName} -> subTypeName) (\s@SemanticEntityType' {} a -> s {subTypeName = a} :: SemanticEntityType)

-- | The semantic entity type name.
semanticEntityType_typeName :: Lens.Lens' SemanticEntityType (Prelude.Maybe Prelude.Text)
semanticEntityType_typeName = Lens.lens (\SemanticEntityType' {typeName} -> typeName) (\s@SemanticEntityType' {} a -> s {typeName = a} :: SemanticEntityType)

-- | The semantic entity type parameters.
semanticEntityType_typeParameters :: Lens.Lens' SemanticEntityType (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
semanticEntityType_typeParameters = Lens.lens (\SemanticEntityType' {typeParameters} -> typeParameters) (\s@SemanticEntityType' {} a -> s {typeParameters = a} :: SemanticEntityType) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON SemanticEntityType where
  parseJSON =
    Data.withObject
      "SemanticEntityType"
      ( \x ->
          SemanticEntityType'
            Prelude.<$> (x Data..:? "SubTypeName")
            Prelude.<*> (x Data..:? "TypeName")
            Prelude.<*> ( x
                            Data..:? "TypeParameters"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable SemanticEntityType where
  hashWithSalt _salt SemanticEntityType' {..} =
    _salt
      `Prelude.hashWithSalt` subTypeName
      `Prelude.hashWithSalt` typeName
      `Prelude.hashWithSalt` typeParameters

instance Prelude.NFData SemanticEntityType where
  rnf SemanticEntityType' {..} =
    Prelude.rnf subTypeName
      `Prelude.seq` Prelude.rnf typeName
      `Prelude.seq` Prelude.rnf typeParameters

instance Data.ToJSON SemanticEntityType where
  toJSON SemanticEntityType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SubTypeName" Data..=) Prelude.<$> subTypeName,
            ("TypeName" Data..=) Prelude.<$> typeName,
            ("TypeParameters" Data..=)
              Prelude.<$> typeParameters
          ]
      )
