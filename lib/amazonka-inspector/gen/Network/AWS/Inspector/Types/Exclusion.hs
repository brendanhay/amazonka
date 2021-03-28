{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Exclusion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Inspector.Types.Exclusion
  ( Exclusion (..)
  -- * Smart constructor
  , mkExclusion
  -- * Lenses
  , eArn
  , eTitle
  , eDescription
  , eRecommendation
  , eScopes
  , eAttributes
  ) where

import qualified Network.AWS.Inspector.Types.Arn as Types
import qualified Network.AWS.Inspector.Types.Attribute as Types
import qualified Network.AWS.Inspector.Types.Description as Types
import qualified Network.AWS.Inspector.Types.Recommendation as Types
import qualified Network.AWS.Inspector.Types.Scope as Types
import qualified Network.AWS.Inspector.Types.Title as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about what was excluded from an assessment run.
--
-- /See:/ 'mkExclusion' smart constructor.
data Exclusion = Exclusion'
  { arn :: Types.Arn
    -- ^ The ARN that specifies the exclusion.
  , title :: Types.Title
    -- ^ The name of the exclusion.
  , description :: Types.Description
    -- ^ The description of the exclusion.
  , recommendation :: Types.Recommendation
    -- ^ The recommendation for the exclusion.
  , scopes :: Core.NonEmpty Types.Scope
    -- ^ The AWS resources for which the exclusion pertains.
  , attributes :: Core.Maybe [Types.Attribute]
    -- ^ The system-defined attributes for the exclusion.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Exclusion' value with any optional fields omitted.
mkExclusion
    :: Types.Arn -- ^ 'arn'
    -> Types.Title -- ^ 'title'
    -> Types.Description -- ^ 'description'
    -> Types.Recommendation -- ^ 'recommendation'
    -> Core.NonEmpty Types.Scope -- ^ 'scopes'
    -> Exclusion
mkExclusion arn title description recommendation scopes
  = Exclusion'{arn, title, description, recommendation, scopes,
               attributes = Core.Nothing}

-- | The ARN that specifies the exclusion.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' Exclusion Types.Arn
eArn = Lens.field @"arn"
{-# INLINEABLE eArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The name of the exclusion.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTitle :: Lens.Lens' Exclusion Types.Title
eTitle = Lens.field @"title"
{-# INLINEABLE eTitle #-}
{-# DEPRECATED title "Use generic-lens or generic-optics with 'title' instead"  #-}

-- | The description of the exclusion.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' Exclusion Types.Description
eDescription = Lens.field @"description"
{-# INLINEABLE eDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The recommendation for the exclusion.
--
-- /Note:/ Consider using 'recommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRecommendation :: Lens.Lens' Exclusion Types.Recommendation
eRecommendation = Lens.field @"recommendation"
{-# INLINEABLE eRecommendation #-}
{-# DEPRECATED recommendation "Use generic-lens or generic-optics with 'recommendation' instead"  #-}

-- | The AWS resources for which the exclusion pertains.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eScopes :: Lens.Lens' Exclusion (Core.NonEmpty Types.Scope)
eScopes = Lens.field @"scopes"
{-# INLINEABLE eScopes #-}
{-# DEPRECATED scopes "Use generic-lens or generic-optics with 'scopes' instead"  #-}

-- | The system-defined attributes for the exclusion.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Exclusion (Core.Maybe [Types.Attribute])
eAttributes = Lens.field @"attributes"
{-# INLINEABLE eAttributes #-}
{-# DEPRECATED attributes "Use generic-lens or generic-optics with 'attributes' instead"  #-}

instance Core.FromJSON Exclusion where
        parseJSON
          = Core.withObject "Exclusion" Core.$
              \ x ->
                Exclusion' Core.<$>
                  (x Core..: "arn") Core.<*> x Core..: "title" Core.<*>
                    x Core..: "description"
                    Core.<*> x Core..: "recommendation"
                    Core.<*> x Core..: "scopes"
                    Core.<*> x Core..:? "attributes"
