{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ExclusionPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.ExclusionPreview
  ( ExclusionPreview (..),

    -- * Smart constructor
    mkExclusionPreview,

    -- * Lenses
    epScopes,
    epAttributes,
    epTitle,
    epDescription,
    epRecommendation,
  )
where

import Network.AWS.Inspector.Types.Attribute
import Network.AWS.Inspector.Types.Scope
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about what is excluded from an assessment run given the current state of the assessment template.
--
-- /See:/ 'mkExclusionPreview' smart constructor.
data ExclusionPreview = ExclusionPreview'
  { -- | The AWS resources for which the exclusion preview pertains.
    scopes :: Lude.NonEmpty Scope,
    -- | The system-defined attributes for the exclusion preview.
    attributes :: Lude.Maybe [Attribute],
    -- | The name of the exclusion preview.
    title :: Lude.Text,
    -- | The description of the exclusion preview.
    description :: Lude.Text,
    -- | The recommendation for the exclusion preview.
    recommendation :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExclusionPreview' with the minimum fields required to make a request.
--
-- * 'scopes' - The AWS resources for which the exclusion preview pertains.
-- * 'attributes' - The system-defined attributes for the exclusion preview.
-- * 'title' - The name of the exclusion preview.
-- * 'description' - The description of the exclusion preview.
-- * 'recommendation' - The recommendation for the exclusion preview.
mkExclusionPreview ::
  -- | 'scopes'
  Lude.NonEmpty Scope ->
  -- | 'title'
  Lude.Text ->
  -- | 'description'
  Lude.Text ->
  -- | 'recommendation'
  Lude.Text ->
  ExclusionPreview
mkExclusionPreview pScopes_ pTitle_ pDescription_ pRecommendation_ =
  ExclusionPreview'
    { scopes = pScopes_,
      attributes = Lude.Nothing,
      title = pTitle_,
      description = pDescription_,
      recommendation = pRecommendation_
    }

-- | The AWS resources for which the exclusion preview pertains.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epScopes :: Lens.Lens' ExclusionPreview (Lude.NonEmpty Scope)
epScopes = Lens.lens (scopes :: ExclusionPreview -> Lude.NonEmpty Scope) (\s a -> s {scopes = a} :: ExclusionPreview)
{-# DEPRECATED epScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

-- | The system-defined attributes for the exclusion preview.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epAttributes :: Lens.Lens' ExclusionPreview (Lude.Maybe [Attribute])
epAttributes = Lens.lens (attributes :: ExclusionPreview -> Lude.Maybe [Attribute]) (\s a -> s {attributes = a} :: ExclusionPreview)
{-# DEPRECATED epAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The name of the exclusion preview.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTitle :: Lens.Lens' ExclusionPreview Lude.Text
epTitle = Lens.lens (title :: ExclusionPreview -> Lude.Text) (\s a -> s {title = a} :: ExclusionPreview)
{-# DEPRECATED epTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The description of the exclusion preview.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epDescription :: Lens.Lens' ExclusionPreview Lude.Text
epDescription = Lens.lens (description :: ExclusionPreview -> Lude.Text) (\s a -> s {description = a} :: ExclusionPreview)
{-# DEPRECATED epDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The recommendation for the exclusion preview.
--
-- /Note:/ Consider using 'recommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epRecommendation :: Lens.Lens' ExclusionPreview Lude.Text
epRecommendation = Lens.lens (recommendation :: ExclusionPreview -> Lude.Text) (\s a -> s {recommendation = a} :: ExclusionPreview)
{-# DEPRECATED epRecommendation "Use generic-lens or generic-optics with 'recommendation' instead." #-}

instance Lude.FromJSON ExclusionPreview where
  parseJSON =
    Lude.withObject
      "ExclusionPreview"
      ( \x ->
          ExclusionPreview'
            Lude.<$> (x Lude..: "scopes")
            Lude.<*> (x Lude..:? "attributes" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "title")
            Lude.<*> (x Lude..: "description")
            Lude.<*> (x Lude..: "recommendation")
      )
