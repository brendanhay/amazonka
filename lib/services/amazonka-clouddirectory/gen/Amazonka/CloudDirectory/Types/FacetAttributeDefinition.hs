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
-- Module      : Amazonka.CloudDirectory.Types.FacetAttributeDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.FacetAttributeDefinition where

import Amazonka.CloudDirectory.Types.FacetAttributeType
import Amazonka.CloudDirectory.Types.Rule
import Amazonka.CloudDirectory.Types.TypedAttributeValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A facet attribute definition. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
--
-- /See:/ 'newFacetAttributeDefinition' smart constructor.
data FacetAttributeDefinition = FacetAttributeDefinition'
  { -- | The default value of the attribute (if configured).
    defaultValue :: Prelude.Maybe TypedAttributeValue,
    -- | Whether the attribute is mutable or not.
    isImmutable :: Prelude.Maybe Prelude.Bool,
    -- | Validation rules attached to the attribute definition.
    rules :: Prelude.Maybe (Prelude.HashMap Prelude.Text Rule),
    -- | The type of the attribute.
    type' :: FacetAttributeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FacetAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'facetAttributeDefinition_defaultValue' - The default value of the attribute (if configured).
--
-- 'isImmutable', 'facetAttributeDefinition_isImmutable' - Whether the attribute is mutable or not.
--
-- 'rules', 'facetAttributeDefinition_rules' - Validation rules attached to the attribute definition.
--
-- 'type'', 'facetAttributeDefinition_type' - The type of the attribute.
newFacetAttributeDefinition ::
  -- | 'type''
  FacetAttributeType ->
  FacetAttributeDefinition
newFacetAttributeDefinition pType_ =
  FacetAttributeDefinition'
    { defaultValue =
        Prelude.Nothing,
      isImmutable = Prelude.Nothing,
      rules = Prelude.Nothing,
      type' = pType_
    }

-- | The default value of the attribute (if configured).
facetAttributeDefinition_defaultValue :: Lens.Lens' FacetAttributeDefinition (Prelude.Maybe TypedAttributeValue)
facetAttributeDefinition_defaultValue = Lens.lens (\FacetAttributeDefinition' {defaultValue} -> defaultValue) (\s@FacetAttributeDefinition' {} a -> s {defaultValue = a} :: FacetAttributeDefinition)

-- | Whether the attribute is mutable or not.
facetAttributeDefinition_isImmutable :: Lens.Lens' FacetAttributeDefinition (Prelude.Maybe Prelude.Bool)
facetAttributeDefinition_isImmutable = Lens.lens (\FacetAttributeDefinition' {isImmutable} -> isImmutable) (\s@FacetAttributeDefinition' {} a -> s {isImmutable = a} :: FacetAttributeDefinition)

-- | Validation rules attached to the attribute definition.
facetAttributeDefinition_rules :: Lens.Lens' FacetAttributeDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Rule))
facetAttributeDefinition_rules = Lens.lens (\FacetAttributeDefinition' {rules} -> rules) (\s@FacetAttributeDefinition' {} a -> s {rules = a} :: FacetAttributeDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The type of the attribute.
facetAttributeDefinition_type :: Lens.Lens' FacetAttributeDefinition FacetAttributeType
facetAttributeDefinition_type = Lens.lens (\FacetAttributeDefinition' {type'} -> type') (\s@FacetAttributeDefinition' {} a -> s {type' = a} :: FacetAttributeDefinition)

instance Data.FromJSON FacetAttributeDefinition where
  parseJSON =
    Data.withObject
      "FacetAttributeDefinition"
      ( \x ->
          FacetAttributeDefinition'
            Prelude.<$> (x Data..:? "DefaultValue")
            Prelude.<*> (x Data..:? "IsImmutable")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Type")
      )

instance Prelude.Hashable FacetAttributeDefinition where
  hashWithSalt _salt FacetAttributeDefinition' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` isImmutable
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` type'

instance Prelude.NFData FacetAttributeDefinition where
  rnf FacetAttributeDefinition' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf isImmutable
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf type'

instance Data.ToJSON FacetAttributeDefinition where
  toJSON FacetAttributeDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValue" Data..=) Prelude.<$> defaultValue,
            ("IsImmutable" Data..=) Prelude.<$> isImmutable,
            ("Rules" Data..=) Prelude.<$> rules,
            Prelude.Just ("Type" Data..= type')
          ]
      )
