{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition where

import Network.AWS.CloudDirectory.Types.FacetAttributeType
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A typed link attribute definition.
--
-- /See:/ 'newTypedLinkAttributeDefinition' smart constructor.
data TypedLinkAttributeDefinition = TypedLinkAttributeDefinition'
  { -- | Whether the attribute is mutable or not.
    isImmutable :: Prelude.Maybe Prelude.Bool,
    -- | Validation rules that are attached to the attribute definition.
    rules :: Prelude.Maybe (Prelude.HashMap Prelude.Text Rule),
    -- | The default value of the attribute (if configured).
    defaultValue :: Prelude.Maybe TypedAttributeValue,
    -- | The unique name of the typed link attribute.
    name :: Prelude.Text,
    -- | The type of the attribute.
    type' :: FacetAttributeType,
    -- | The required behavior of the @TypedLinkAttributeDefinition@.
    requiredBehavior :: RequiredAttributeBehavior
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TypedLinkAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isImmutable', 'typedLinkAttributeDefinition_isImmutable' - Whether the attribute is mutable or not.
--
-- 'rules', 'typedLinkAttributeDefinition_rules' - Validation rules that are attached to the attribute definition.
--
-- 'defaultValue', 'typedLinkAttributeDefinition_defaultValue' - The default value of the attribute (if configured).
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
      { isImmutable =
          Prelude.Nothing,
        rules = Prelude.Nothing,
        defaultValue = Prelude.Nothing,
        name = pName_,
        type' = pType_,
        requiredBehavior = pRequiredBehavior_
      }

-- | Whether the attribute is mutable or not.
typedLinkAttributeDefinition_isImmutable :: Lens.Lens' TypedLinkAttributeDefinition (Prelude.Maybe Prelude.Bool)
typedLinkAttributeDefinition_isImmutable = Lens.lens (\TypedLinkAttributeDefinition' {isImmutable} -> isImmutable) (\s@TypedLinkAttributeDefinition' {} a -> s {isImmutable = a} :: TypedLinkAttributeDefinition)

-- | Validation rules that are attached to the attribute definition.
typedLinkAttributeDefinition_rules :: Lens.Lens' TypedLinkAttributeDefinition (Prelude.Maybe (Prelude.HashMap Prelude.Text Rule))
typedLinkAttributeDefinition_rules = Lens.lens (\TypedLinkAttributeDefinition' {rules} -> rules) (\s@TypedLinkAttributeDefinition' {} a -> s {rules = a} :: TypedLinkAttributeDefinition) Prelude.. Lens.mapping Prelude._Coerce

-- | The default value of the attribute (if configured).
typedLinkAttributeDefinition_defaultValue :: Lens.Lens' TypedLinkAttributeDefinition (Prelude.Maybe TypedAttributeValue)
typedLinkAttributeDefinition_defaultValue = Lens.lens (\TypedLinkAttributeDefinition' {defaultValue} -> defaultValue) (\s@TypedLinkAttributeDefinition' {} a -> s {defaultValue = a} :: TypedLinkAttributeDefinition)

-- | The unique name of the typed link attribute.
typedLinkAttributeDefinition_name :: Lens.Lens' TypedLinkAttributeDefinition Prelude.Text
typedLinkAttributeDefinition_name = Lens.lens (\TypedLinkAttributeDefinition' {name} -> name) (\s@TypedLinkAttributeDefinition' {} a -> s {name = a} :: TypedLinkAttributeDefinition)

-- | The type of the attribute.
typedLinkAttributeDefinition_type :: Lens.Lens' TypedLinkAttributeDefinition FacetAttributeType
typedLinkAttributeDefinition_type = Lens.lens (\TypedLinkAttributeDefinition' {type'} -> type') (\s@TypedLinkAttributeDefinition' {} a -> s {type' = a} :: TypedLinkAttributeDefinition)

-- | The required behavior of the @TypedLinkAttributeDefinition@.
typedLinkAttributeDefinition_requiredBehavior :: Lens.Lens' TypedLinkAttributeDefinition RequiredAttributeBehavior
typedLinkAttributeDefinition_requiredBehavior = Lens.lens (\TypedLinkAttributeDefinition' {requiredBehavior} -> requiredBehavior) (\s@TypedLinkAttributeDefinition' {} a -> s {requiredBehavior = a} :: TypedLinkAttributeDefinition)

instance
  Prelude.FromJSON
    TypedLinkAttributeDefinition
  where
  parseJSON =
    Prelude.withObject
      "TypedLinkAttributeDefinition"
      ( \x ->
          TypedLinkAttributeDefinition'
            Prelude.<$> (x Prelude..:? "IsImmutable")
            Prelude.<*> (x Prelude..:? "Rules" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "DefaultValue")
            Prelude.<*> (x Prelude..: "Name")
            Prelude.<*> (x Prelude..: "Type")
            Prelude.<*> (x Prelude..: "RequiredBehavior")
      )

instance
  Prelude.Hashable
    TypedLinkAttributeDefinition

instance Prelude.NFData TypedLinkAttributeDefinition

instance Prelude.ToJSON TypedLinkAttributeDefinition where
  toJSON TypedLinkAttributeDefinition' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("IsImmutable" Prelude..=) Prelude.<$> isImmutable,
            ("Rules" Prelude..=) Prelude.<$> rules,
            ("DefaultValue" Prelude..=) Prelude.<$> defaultValue,
            Prelude.Just ("Name" Prelude..= name),
            Prelude.Just ("Type" Prelude..= type'),
            Prelude.Just
              ("RequiredBehavior" Prelude..= requiredBehavior)
          ]
      )
