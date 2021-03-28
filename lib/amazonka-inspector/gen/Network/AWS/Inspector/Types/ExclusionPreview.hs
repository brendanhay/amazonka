{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.ExclusionPreview
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.ExclusionPreview
  ( ExclusionPreview (..)
  -- * Smart constructor
  , mkExclusionPreview
  -- * Lenses
  , epTitle
  , epDescription
  , epRecommendation
  , epScopes
  , epAttributes
  ) where

import qualified Network.AWS.Inspector.Types.Attribute as Types
import qualified Network.AWS.Inspector.Types.Scope as Types
import qualified Network.AWS.Inspector.Types.Text as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about what is excluded from an assessment run given the current state of the assessment template.
--
-- /See:/ 'mkExclusionPreview' smart constructor.
data ExclusionPreview = ExclusionPreview'
  { title :: Types.Text
    -- ^ The name of the exclusion preview.
  , description :: Types.Text
    -- ^ The description of the exclusion preview.
  , recommendation :: Types.Text
    -- ^ The recommendation for the exclusion preview.
  , scopes :: Core.NonEmpty Types.Scope
    -- ^ The AWS resources for which the exclusion preview pertains.
  , attributes :: Core.Maybe [Types.Attribute]
    -- ^ The system-defined attributes for the exclusion preview.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ExclusionPreview' value with any optional fields omitted.
mkExclusionPreview
    :: Types.Text -- ^ 'title'
    -> Types.Text -- ^ 'description'
    -> Types.Text -- ^ 'recommendation'
    -> Core.NonEmpty Types.Scope -- ^ 'scopes'
    -> ExclusionPreview
mkExclusionPreview title description recommendation scopes
  = ExclusionPreview'{title, description, recommendation, scopes,
                      attributes = Core.Nothing}

-- | The name of the exclusion preview.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epTitle :: Lens.Lens' ExclusionPreview Types.Text
epTitle = Lens.field @"title"
{-# INLINEABLE epTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The description of the exclusion preview.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epDescription :: Lens.Lens' ExclusionPreview Types.Text
epDescription = Lens.field @"description"
{-# INLINEABLE epDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The recommendation for the exclusion preview.
--
-- /Note:/ Consider using 'recommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epRecommendation :: Lens.Lens' ExclusionPreview Types.Text
epRecommendation = Lens.field @"recommendation"
{-# INLINEABLE epRecommendation #-}
{-# DEPRECATED recommendation "Use generic-lens or generic-optics with 'recommendation' instead"  #-}

-- | The AWS resources for which the exclusion preview pertains.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epScopes :: Lens.Lens' ExclusionPreview (Core.NonEmpty Types.Scope)
epScopes = Lens.field @"scopes"
{-# INLINEABLE epScopes #-}
{-# DEPRECATED scopes "Use generic-lens or generic-optics with 'scopes' instead"  #-}

-- | The system-defined attributes for the exclusion preview.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epAttributes :: Lens.Lens' ExclusionPreview (Core.Maybe [Types.Attribute])
epAttributes = Lens.field @"attributes"
{-# INLINEABLE epAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.FromJSON ExclusionPreview where
        parseJSON
          = Core.withObject "ExclusionPreview" Core.$
              \ x ->
                ExclusionPreview' Core.<$>
                  (x Core..: "title") Core.<*> x Core..: "description" Core.<*>
                    x Core..: "recommendation"
                    Core.<*> x Core..: "scopes"
                    Core.<*> x Core..:? "attributes"
