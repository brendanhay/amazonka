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
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttribute where

import Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
import Network.AWS.CloudDirectory.Types.FacetAttributeReference
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An attribute that is associated with the Facet.
--
-- /See:/ 'newFacetAttribute' smart constructor.
data FacetAttribute = FacetAttribute'
  { -- | An attribute reference that is associated with the attribute. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
    -- for more information.
    attributeReference :: Core.Maybe FacetAttributeReference,
    -- | The required behavior of the @FacetAttribute@.
    requiredBehavior :: Core.Maybe RequiredAttributeBehavior,
    -- | A facet attribute consists of either a definition or a reference. This
    -- structure contains the attribute definition. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
    -- for more information.
    attributeDefinition :: Core.Maybe FacetAttributeDefinition,
    -- | The name of the facet attribute.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FacetAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeReference', 'facetAttribute_attributeReference' - An attribute reference that is associated with the attribute. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
--
-- 'requiredBehavior', 'facetAttribute_requiredBehavior' - The required behavior of the @FacetAttribute@.
--
-- 'attributeDefinition', 'facetAttribute_attributeDefinition' - A facet attribute consists of either a definition or a reference. This
-- structure contains the attribute definition. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
--
-- 'name', 'facetAttribute_name' - The name of the facet attribute.
newFacetAttribute ::
  -- | 'name'
  Core.Text ->
  FacetAttribute
newFacetAttribute pName_ =
  FacetAttribute'
    { attributeReference = Core.Nothing,
      requiredBehavior = Core.Nothing,
      attributeDefinition = Core.Nothing,
      name = pName_
    }

-- | An attribute reference that is associated with the attribute. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
facetAttribute_attributeReference :: Lens.Lens' FacetAttribute (Core.Maybe FacetAttributeReference)
facetAttribute_attributeReference = Lens.lens (\FacetAttribute' {attributeReference} -> attributeReference) (\s@FacetAttribute' {} a -> s {attributeReference = a} :: FacetAttribute)

-- | The required behavior of the @FacetAttribute@.
facetAttribute_requiredBehavior :: Lens.Lens' FacetAttribute (Core.Maybe RequiredAttributeBehavior)
facetAttribute_requiredBehavior = Lens.lens (\FacetAttribute' {requiredBehavior} -> requiredBehavior) (\s@FacetAttribute' {} a -> s {requiredBehavior = a} :: FacetAttribute)

-- | A facet attribute consists of either a definition or a reference. This
-- structure contains the attribute definition. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
facetAttribute_attributeDefinition :: Lens.Lens' FacetAttribute (Core.Maybe FacetAttributeDefinition)
facetAttribute_attributeDefinition = Lens.lens (\FacetAttribute' {attributeDefinition} -> attributeDefinition) (\s@FacetAttribute' {} a -> s {attributeDefinition = a} :: FacetAttribute)

-- | The name of the facet attribute.
facetAttribute_name :: Lens.Lens' FacetAttribute Core.Text
facetAttribute_name = Lens.lens (\FacetAttribute' {name} -> name) (\s@FacetAttribute' {} a -> s {name = a} :: FacetAttribute)

instance Core.FromJSON FacetAttribute where
  parseJSON =
    Core.withObject
      "FacetAttribute"
      ( \x ->
          FacetAttribute'
            Core.<$> (x Core..:? "AttributeReference")
            Core.<*> (x Core..:? "RequiredBehavior")
            Core.<*> (x Core..:? "AttributeDefinition")
            Core.<*> (x Core..: "Name")
      )

instance Core.Hashable FacetAttribute

instance Core.NFData FacetAttribute

instance Core.ToJSON FacetAttribute where
  toJSON FacetAttribute' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AttributeReference" Core..=)
              Core.<$> attributeReference,
            ("RequiredBehavior" Core..=)
              Core.<$> requiredBehavior,
            ("AttributeDefinition" Core..=)
              Core.<$> attributeDefinition,
            Core.Just ("Name" Core..= name)
          ]
      )
