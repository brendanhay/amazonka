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
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeReference
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeReference where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The facet attribute reference that specifies the attribute definition
-- that contains the attribute facet name and attribute name.
--
-- /See:/ 'newFacetAttributeReference' smart constructor.
data FacetAttributeReference = FacetAttributeReference'
  { -- | The target facet name that is associated with the facet reference. See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
    -- for more information.
    targetFacetName :: Prelude.Text,
    -- | The target attribute name that is associated with the facet reference.
    -- See
    -- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
    -- for more information.
    targetAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'FacetAttributeReference' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetFacetName', 'facetAttributeReference_targetFacetName' - The target facet name that is associated with the facet reference. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
--
-- 'targetAttributeName', 'facetAttributeReference_targetAttributeName' - The target attribute name that is associated with the facet reference.
-- See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
newFacetAttributeReference ::
  -- | 'targetFacetName'
  Prelude.Text ->
  -- | 'targetAttributeName'
  Prelude.Text ->
  FacetAttributeReference
newFacetAttributeReference
  pTargetFacetName_
  pTargetAttributeName_ =
    FacetAttributeReference'
      { targetFacetName =
          pTargetFacetName_,
        targetAttributeName = pTargetAttributeName_
      }

-- | The target facet name that is associated with the facet reference. See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
facetAttributeReference_targetFacetName :: Lens.Lens' FacetAttributeReference Prelude.Text
facetAttributeReference_targetFacetName = Lens.lens (\FacetAttributeReference' {targetFacetName} -> targetFacetName) (\s@FacetAttributeReference' {} a -> s {targetFacetName = a} :: FacetAttributeReference)

-- | The target attribute name that is associated with the facet reference.
-- See
-- <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References>
-- for more information.
facetAttributeReference_targetAttributeName :: Lens.Lens' FacetAttributeReference Prelude.Text
facetAttributeReference_targetAttributeName = Lens.lens (\FacetAttributeReference' {targetAttributeName} -> targetAttributeName) (\s@FacetAttributeReference' {} a -> s {targetAttributeName = a} :: FacetAttributeReference)

instance Prelude.FromJSON FacetAttributeReference where
  parseJSON =
    Prelude.withObject
      "FacetAttributeReference"
      ( \x ->
          FacetAttributeReference'
            Prelude.<$> (x Prelude..: "TargetFacetName")
            Prelude.<*> (x Prelude..: "TargetAttributeName")
      )

instance Prelude.Hashable FacetAttributeReference

instance Prelude.NFData FacetAttributeReference

instance Prelude.ToJSON FacetAttributeReference where
  toJSON FacetAttributeReference' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("TargetFacetName" Prelude..= targetFacetName),
            Prelude.Just
              ( "TargetAttributeName"
                  Prelude..= targetAttributeName
              )
          ]
      )
