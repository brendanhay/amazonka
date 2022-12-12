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
-- Module      : Amazonka.CloudDirectory.Types.TypedLinkAttributeDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.TypedLinkAttributeDefinition where

import Amazonka.CloudDirectory.Types.FacetAttributeType
import Amazonka.CloudDirectory.Types.RequiredAttributeBehavior
import Amazonka.CloudDirectory.Types.Rule
import Amazonka.CloudDirectory.Types.TypedAttributeValue
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A typed link attribute definition.
--
-- /See:/ 'newTypedLinkAttributeDefinition' smart constructor.
data TypedLinkAttributeDefinition = TypedLinkAttributeDefinition'
  { -- | The default value of the attribute (if configured).
    defaultValue :: Prelude.Maybe TypedAttributeValue,
    -- | Whether the attribute is mutable or not.
    isImmutable :: Prelude.Maybe Prelude.Bool,
    -- | Validation rules that are attached to the attribute definition.
    rules :: Prelude.Maybe (Prelude.HashMap Prelude.Text Rule),
    -- | The unique name of the typed link attribute.
    name :: Prelude.Text,
    -- | The type of the attribute.
    type' :: FacetAttributeType,
    -- | The required behavior of the @TypedLinkAttributeDefinition@.
    requiredBehavior :: RequiredAttributeBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultValue', 'typedLinkAttributeDefinition_defaultValue' - The default value of the attribute (if configured).
--
-- 'isImmutable', 'typedLinkAttributeDefinition_isImmutable' - Whether the attribute is mutable or not.
--
-- 'rules', 'typedLinkAttributeDefinition_rules' - Validation rules that are attached to the attribute definition.
--
-- 'name', 'typedLinkAttributeDefinition_name' - The unique name of the typed link attribute.
--
-- 'type'', 'typedLinkAttributeDefinition_type' - The type of the attribute.
--
-- 'requiredBehavior', 'typedLinkAttributeDefinition_requiredBehavior' - The required behavior of the @TypedLinkAttributeDefinition@.
newTypedLinkAttributeDefinition ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  FacetAttributeType ->
  -- | 'requiredBehavior'
  RequiredAttributeBehavior ->
  TypedLinkAttributeDefinition
newTypedLinkAttributeDefinition
  pName_
  pType_
  pRequiredBehavior_ =
    TypedLinkAttributeDefinition'
      { defaultValue =
          Prelude.Nothing,
        isImmutable = Prelude.Nothing,
        rules = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        requiredBehavior = pRequiredBehavior_
      }

-- | The default value of the attribute (if configured).
typedLinkAttributeDefinition_defaultValue :: Lens.Lens' TypedLinkAttributeDefinition (Prelude.Maybe TypedAttributeValue)
typedLinkAttributeDefinition_defaultValue = Lens.lens (\TypedLinkAttributeDefinition' {defaultValue} -> defaultValue) (\s@TypedLinkAttributeDefinition' {} a -> s {defaultValue = a} :: TypedLinkAttributeDefinition)

-- | Whether the attribute is mutable or not.
typedLinkAttributeDefinition_isImmutable :: Lens.Lens' TypedLinkAttributeDefinition (Prelude.Maybe Prelude.Bool)
typedLinkAttributeDefinition_isImmutable = Lens.lens (\TypedLinkAttributeDefinition' {isImmutable} -> isImmutable) (\s@TypedLinkAttributeDefinition' {} a -> s {isImmutable = a} :: TypedLinkAttributeDefinition)

-- | Validation rules that are attached to the attribute definition.
typedLinkAttributeDefinition_rules :: Lens.Lens' TypedLinkAttributeDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Rule))
typedLinkAttributeDefinition_rules = Lens.lens (\TypedLinkAttributeDefinition' {rules} -> rules) (\s@TypedLinkAttributeDefinition' {} a -> s {rules = a} :: TypedLinkAttributeDefinition) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the typed link attribute.
typedLinkAttributeDefinition_name :: Lens.Lens' TypedLinkAttributeDefinition Prelude.Text
typedLinkAttributeDefinition_name = Lens.lens (\TypedLinkAttributeDefinition' {name} -> name) (\s@TypedLinkAttributeDefinition' {} a -> s {name = a} :: TypedLinkAttributeDefinition)

-- | The type of the attribute.
typedLinkAttributeDefinition_type :: Lens.Lens' TypedLinkAttributeDefinition FacetAttributeType
typedLinkAttributeDefinition_type = Lens.lens (\TypedLinkAttributeDefinition' {type'} -> type') (\s@TypedLinkAttributeDefinition' {} a -> s {type' = a} :: TypedLinkAttributeDefinition)

-- | The required behavior of the @TypedLinkAttributeDefinition@.
typedLinkAttributeDefinition_requiredBehavior :: Lens.Lens' TypedLinkAttributeDefinition RequiredAttributeBehavior
typedLinkAttributeDefinition_requiredBehavior = Lens.lens (\TypedLinkAttributeDefinition' {requiredBehavior} -> requiredBehavior) (\s@TypedLinkAttributeDefinition' {} a -> s {requiredBehavior = a} :: TypedLinkAttributeDefinition)

instance Data.FromJSON TypedLinkAttributeDefinition where
  parseJSON =
    Data.withObject
      "TypedLinkAttributeDefinition"
      ( \x ->
          TypedLinkAttributeDefinition'
            Prelude.<$> (x Data..:? "DefaultValue")
            Prelude.<*> (x Data..:? "IsImmutable")
            Prelude.<*> (x Data..:? "Rules" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Type")
            Prelude.<*> (x Data..: "RequiredBehavior")
      )

instance
  Prelude.Hashable
    TypedLinkAttributeDefinition
  where
  hashWithSalt _salt TypedLinkAttributeDefinition' {..} =
    _salt `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` isImmutable
      `Prelude.hashWithSalt` rules
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` requiredBehavior

instance Prelude.NFData TypedLinkAttributeDefinition where
  rnf TypedLinkAttributeDefinition' {..} =
    Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf isImmutable
      `Prelude.seq` Prelude.rnf rules
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf requiredBehavior

instance Data.ToJSON TypedLinkAttributeDefinition where
  toJSON TypedLinkAttributeDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DefaultValue" Data..=) Prelude.<$> defaultValue,
            ("IsImmutable" Data..=) Prelude.<$> isImmutable,
            ("Rules" Data..=) Prelude.<$> rules,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Type" Data..= type'),
            Prelude.Just
              ("RequiredBehavior" Data..= requiredBehavior)
          ]
      )
