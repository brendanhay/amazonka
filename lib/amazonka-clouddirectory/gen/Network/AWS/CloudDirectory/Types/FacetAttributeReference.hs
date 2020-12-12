{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeReference
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeReference
  ( FacetAttributeReference (..),

    -- * Smart constructor
    mkFacetAttributeReference,

    -- * Lenses
    farTargetFacetName,
    farTargetAttributeName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The facet attribute reference that specifies the attribute definition that contains the attribute facet name and attribute name.
--
-- /See:/ 'mkFacetAttributeReference' smart constructor.
data FacetAttributeReference = FacetAttributeReference'
  { targetFacetName ::
      Lude.Text,
    targetAttributeName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FacetAttributeReference' with the minimum fields required to make a request.
--
-- * 'targetAttributeName' - The target attribute name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
-- * 'targetFacetName' - The target facet name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
mkFacetAttributeReference ::
  -- | 'targetFacetName'
  Lude.Text ->
  -- | 'targetAttributeName'
  Lude.Text ->
  FacetAttributeReference
mkFacetAttributeReference pTargetFacetName_ pTargetAttributeName_ =
  FacetAttributeReference'
    { targetFacetName = pTargetFacetName_,
      targetAttributeName = pTargetAttributeName_
    }

-- | The target facet name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'targetFacetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farTargetFacetName :: Lens.Lens' FacetAttributeReference Lude.Text
farTargetFacetName = Lens.lens (targetFacetName :: FacetAttributeReference -> Lude.Text) (\s a -> s {targetFacetName = a} :: FacetAttributeReference)
{-# DEPRECATED farTargetFacetName "Use generic-lens or generic-optics with 'targetFacetName' instead." #-}

-- | The target attribute name that is associated with the facet reference. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /Note:/ Consider using 'targetAttributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
farTargetAttributeName :: Lens.Lens' FacetAttributeReference Lude.Text
farTargetAttributeName = Lens.lens (targetAttributeName :: FacetAttributeReference -> Lude.Text) (\s a -> s {targetAttributeName = a} :: FacetAttributeReference)
{-# DEPRECATED farTargetAttributeName "Use generic-lens or generic-optics with 'targetAttributeName' instead." #-}

instance Lude.FromJSON FacetAttributeReference where
  parseJSON =
    Lude.withObject
      "FacetAttributeReference"
      ( \x ->
          FacetAttributeReference'
            Lude.<$> (x Lude..: "TargetFacetName")
            Lude.<*> (x Lude..: "TargetAttributeName")
      )

instance Lude.ToJSON FacetAttributeReference where
  toJSON FacetAttributeReference' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetFacetName" Lude..= targetFacetName),
            Lude.Just ("TargetAttributeName" Lude..= targetAttributeName)
          ]
      )
