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
-- Module      : Amazonka.CloudDirectory.Types.FacetAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudDirectory.Types.FacetAttribute where

import Amazonka.CloudDirectory.Types.FacetAttributeDefinition
import Amazonka.CloudDirectory.Types.FacetAttributeReference
import Amazonka.CloudDirectory.Types.RequiredAttributeBehavior
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Data.FromJSON FacetAttribute where
  parseJSON =
    Data.withObject
      "FacetAttribute"
      ( \x ->
          FacetAttribute'
            Prelude.<$> (x Data..:? "AttributeReference")
            Prelude.<*> (x Data..:? "RequiredBehavior")
            Prelude.<*> (x Data..:? "AttributeDefinition")
            Prelude.<*> (x Data..: "Name")
      )

instance Prelude.Hashable FacetAttribute where
  hashWithSalt _salt FacetAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeReference
      `Prelude.hashWithSalt` requiredBehavior
      `Prelude.hashWithSalt` attributeDefinition
      `Prelude.hashWithSalt` name

instance Prelude.NFData FacetAttribute where
  rnf FacetAttribute' {..} =
    Prelude.rnf attributeReference
      `Prelude.seq` Prelude.rnf requiredBehavior
      `Prelude.seq` Prelude.rnf attributeDefinition
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON FacetAttribute where
  toJSON FacetAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AttributeReference" Data..=)
              Prelude.<$> attributeReference,
            ("RequiredBehavior" Data..=)
              Prelude.<$> requiredBehavior,
            ("AttributeDefinition" Data..=)
              Prelude.<$> attributeDefinition,
            Prelude.Just ("Name" Data..= name)
          ]
      )
