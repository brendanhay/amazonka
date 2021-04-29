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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An attribute that is associated with the Facet.
--
-- /See:/ 'newFacetAttribute' smart constructor.
data FacetAttribute = FacetAttribute'
  { -- | An attribute reference that is associated with the attribute. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
    -- for more information.
    attributeReference :: Prelude.Maybe FacetAttributeReference,
    -- | The required behavior of the @FacetAttribute@.
    requiredBehavior :: Prelude.Maybe RequiredAttributeBehavior,
    -- | A facet attribute consists of either a definition or a reference. This
    -- structure contains the attribute definition. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
    -- for more information.
    attributeDefinition :: Prelude.Maybe FacetAttributeDefinition,
    -- | The name of the facet attribute.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  FacetAttribute
newFacetAttribute pName_ =
  FacetAttribute'
    { attributeReference =
        Prelude.Nothing,
      requiredBehavior = Prelude.Nothing,
      attributeDefinition = Prelude.Nothing,
      name = pName_
    }

-- | An attribute reference that is associated with the attribute. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
facetAttribute_attributeReference :: Lens.Lens' FacetAttribute (Prelude.Maybe FacetAttributeReference)
facetAttribute_attributeReference = Lens.lens (\FacetAttribute' {attributeReference} -> attributeReference) (\s@FacetAttribute' {} a -> s {attributeReference = a} :: FacetAttribute)

-- | The required behavior of the @FacetAttribute@.
facetAttribute_requiredBehavior :: Lens.Lens' FacetAttribute (Prelude.Maybe RequiredAttributeBehavior)
facetAttribute_requiredBehavior = Lens.lens (\FacetAttribute' {requiredBehavior} -> requiredBehavior) (\s@FacetAttribute' {} a -> s {requiredBehavior = a} :: FacetAttribute)

-- | A facet attribute consists of either a definition or a reference. This
-- structure contains the attribute definition. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
facetAttribute_attributeDefinition :: Lens.Lens' FacetAttribute (Prelude.Maybe FacetAttributeDefinition)
facetAttribute_attributeDefinition = Lens.lens (\FacetAttribute' {attributeDefinition} -> attributeDefinition) (\s@FacetAttribute' {} a -> s {attributeDefinition = a} :: FacetAttribute)

-- | The name of the facet attribute.
facetAttribute_name :: Lens.Lens' FacetAttribute Prelude.Text
facetAttribute_name = Lens.lens (\FacetAttribute' {name} -> name) (\s@FacetAttribute' {} a -> s {name = a} :: FacetAttribute)

instance Prelude.FromJSON FacetAttribute where
  parseJSON =
    Prelude.withObject
      "FacetAttribute"
      ( \x ->
          FacetAttribute'
            Prelude.<$> (x Prelude..:? "AttributeReference")
            Prelude.<*> (x Prelude..:? "RequiredBehavior")
            Prelude.<*> (x Prelude..:? "AttributeDefinition")
            Prelude.<*> (x Prelude..: "Name")
      )

instance Prelude.Hashable FacetAttribute

instance Prelude.NFData FacetAttribute

instance Prelude.ToJSON FacetAttribute where
  toJSON FacetAttribute' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("AttributeReference" Prelude..=)
              Prelude.<$> attributeReference,
            ("RequiredBehavior" Prelude..=)
              Prelude.<$> requiredBehavior,
            ("AttributeDefinition" Prelude..=)
              Prelude.<$> attributeDefinition,
            Prelude.Just ("Name" Prelude..= name)
          ]
      )
