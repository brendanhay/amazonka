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
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeDefinition where

import Network.AWS.CloudDirectory.Types.FacetAttributeType
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A facet attribute definition. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
--
-- /See:/ 'newFacetAttributeDefinition' smart constructor.
data FacetAttributeDefinition = FacetAttributeDefinition'
  { -- | Whether the attribute is mutable or not.
    isImmutable :: Core.Maybe Core.Bool,
    -- | Validation rules attached to the attribute definition.
    rules :: Core.Maybe (Core.HashMap Core.Text Rule),
    -- | The default value of the attribute (if configured).
    defaultValue :: Core.Maybe TypedAttributeValue,
    -- | The type of the attribute.
    type' :: FacetAttributeType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FacetAttributeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isImmutable', 'facetAttributeDefinition_isImmutable' - Whether the attribute is mutable or not.
--
-- 'rules', 'facetAttributeDefinition_rules' - Validation rules attached to the attribute definition.
--
-- 'defaultValue', 'facetAttributeDefinition_defaultValue' - The default value of the attribute (if configured).
--
-- 'type'', 'facetAttributeDefinition_type' - The type of the attribute.
newFacetAttributeDefinition ::
  -- | 'type''
  FacetAttributeType ->
  FacetAttributeDefinition
newFacetAttributeDefinition pType_ =
  FacetAttributeDefinition'
    { isImmutable =
        Core.Nothing,
      rules = Core.Nothing,
      defaultValue = Core.Nothing,
      type' = pType_
    }

-- | Whether the attribute is mutable or not.
facetAttributeDefinition_isImmutable :: Lens.Lens' FacetAttributeDefinition (Core.Maybe Core.Bool)
facetAttributeDefinition_isImmutable = Lens.lens (\FacetAttributeDefinition' {isImmutable} -> isImmutable) (\s@FacetAttributeDefinition' {} a -> s {isImmutable = a} :: FacetAttributeDefinition)

-- | Validation rules attached to the attribute definition.
facetAttributeDefinition_rules :: Lens.Lens' FacetAttributeDefinition (Core.Maybe (Core.HashMap Core.Text Rule))
facetAttributeDefinition_rules = Lens.lens (\FacetAttributeDefinition' {rules} -> rules) (\s@FacetAttributeDefinition' {} a -> s {rules = a} :: FacetAttributeDefinition) Core.. Lens.mapping Lens._Coerce

-- | The default value of the attribute (if configured).
facetAttributeDefinition_defaultValue :: Lens.Lens' FacetAttributeDefinition (Core.Maybe TypedAttributeValue)
facetAttributeDefinition_defaultValue = Lens.lens (\FacetAttributeDefinition' {defaultValue} -> defaultValue) (\s@FacetAttributeDefinition' {} a -> s {defaultValue = a} :: FacetAttributeDefinition)

-- | The type of the attribute.
facetAttributeDefinition_type :: Lens.Lens' FacetAttributeDefinition FacetAttributeType
facetAttributeDefinition_type = Lens.lens (\FacetAttributeDefinition' {type'} -> type') (\s@FacetAttributeDefinition' {} a -> s {type' = a} :: FacetAttributeDefinition)

instance Core.FromJSON FacetAttributeDefinition where
  parseJSON =
    Core.withObject
      "FacetAttributeDefinition"
      ( \x ->
          FacetAttributeDefinition'
            Core.<$> (x Core..:? "IsImmutable")
            Core.<*> (x Core..:? "Rules" Core..!= Core.mempty)
            Core.<*> (x Core..:? "DefaultValue")
            Core.<*> (x Core..: "Type")
      )

instance Core.Hashable FacetAttributeDefinition

instance Core.NFData FacetAttributeDefinition

instance Core.ToJSON FacetAttributeDefinition where
  toJSON FacetAttributeDefinition' {..} =
    Core.object
      ( Core.catMaybes
          [ ("IsImmutable" Core..=) Core.<$> isImmutable,
            ("Rules" Core..=) Core.<$> rules,
            ("DefaultValue" Core..=) Core.<$> defaultValue,
            Core.Just ("Type" Core..= type')
          ]
      )
