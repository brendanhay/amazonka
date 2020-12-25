{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Exclusion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Exclusion
  ( Exclusion (..),

    -- * Smart constructor
    mkExclusion,

    -- * Lenses
    eArn,
    eTitle,
    eDescription,
    eRecommendation,
    eScopes,
    eAttributes,
  )
where

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
  { -- | The ARN that specifies the exclusion.
    arn :: Types.Arn,
    -- | The name of the exclusion.
    title :: Types.Title,
    -- | The description of the exclusion.
    description :: Types.Description,
    -- | The recommendation for the exclusion.
    recommendation :: Types.Recommendation,
    -- | The AWS resources for which the exclusion pertains.
    scopes :: Core.NonEmpty Types.Scope,
    -- | The system-defined attributes for the exclusion.
    attributes :: Core.Maybe [Types.Attribute]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Exclusion' value with any optional fields omitted.
mkExclusion ::
  -- | 'arn'
  Types.Arn ->
  -- | 'title'
  Types.Title ->
  -- | 'description'
  Types.Description ->
  -- | 'recommendation'
  Types.Recommendation ->
  -- | 'scopes'
  Core.NonEmpty Types.Scope ->
  Exclusion
mkExclusion arn title description recommendation scopes =
  Exclusion'
    { arn,
      title,
      description,
      recommendation,
      scopes,
      attributes = Core.Nothing
    }

-- | The ARN that specifies the exclusion.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eArn :: Lens.Lens' Exclusion Types.Arn
eArn = Lens.field @"arn"
{-# DEPRECATED eArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the exclusion.
--
-- /Note:/ Consider using 'title' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTitle :: Lens.Lens' Exclusion Types.Title
eTitle = Lens.field @"title"
{-# DEPRECATED eTitle "Use generic-lens or generic-optics with 'title' instead." #-}

-- | The description of the exclusion.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDescription :: Lens.Lens' Exclusion Types.Description
eDescription = Lens.field @"description"
{-# DEPRECATED eDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The recommendation for the exclusion.
--
-- /Note:/ Consider using 'recommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eRecommendation :: Lens.Lens' Exclusion Types.Recommendation
eRecommendation = Lens.field @"recommendation"
{-# DEPRECATED eRecommendation "Use generic-lens or generic-optics with 'recommendation' instead." #-}

-- | The AWS resources for which the exclusion pertains.
--
-- /Note:/ Consider using 'scopes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eScopes :: Lens.Lens' Exclusion (Core.NonEmpty Types.Scope)
eScopes = Lens.field @"scopes"
{-# DEPRECATED eScopes "Use generic-lens or generic-optics with 'scopes' instead." #-}

-- | The system-defined attributes for the exclusion.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAttributes :: Lens.Lens' Exclusion (Core.Maybe [Types.Attribute])
eAttributes = Lens.field @"attributes"
{-# DEPRECATED eAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Core.FromJSON Exclusion where
  parseJSON =
    Core.withObject "Exclusion" Core.$
      \x ->
        Exclusion'
          Core.<$> (x Core..: "arn")
          Core.<*> (x Core..: "title")
          Core.<*> (x Core..: "description")
          Core.<*> (x Core..: "recommendation")
          Core.<*> (x Core..: "scopes")
          Core.<*> (x Core..:? "attributes")
