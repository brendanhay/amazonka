{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Expression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CostExplorer.Types.Expression
  ( Expression (..)
  -- * Smart constructor
  , mkExpression
  -- * Lenses
  , eAnd
  , eCostCategories
  , eDimensions
  , eNot
  , eOr
  , eTags
  ) where

import qualified Network.AWS.CostExplorer.Types.CostCategoryValues as Types
import qualified Network.AWS.CostExplorer.Types.DimensionValues as Types
import qualified Network.AWS.CostExplorer.Types.TagValues as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Use @Expression@ to filter by cost or by usage. There are two patterns: 
--
--
--     * Simple dimension values - You can set the dimension name and values for the filters that you plan to use. For example, you can filter for @REGION==us-east-1 OR REGION==us-west-1@ . For @GetRightsizingRecommendation@ , the Region is a full name (for example, @REGION==US East (N. Virginia)@ . The @Expression@ example looks like:
-- @{ "Dimensions": { "Key": "REGION", "Values": [ "us-east-1", “us-west-1” ] } }@ 
-- The list of dimension values are OR'd together to retrieve cost or usage data. You can create @Expression@ and @DimensionValues@ objects using either @with*@ methods or @set*@ methods in multiple lines. 
--
--
--     * Compound dimension values with logical operations - You can use multiple @Expression@ types and the logical operators @AND/OR/NOT@ to create a list of one or more @Expression@ objects. This allows you to filter on more advanced options. For example, you can filter on @((REGION == us-east-1 OR REGION == us-west-1) OR (TAG.Type == Type1)) AND (USAGE_TYPE != DataTransfer)@ . The @Expression@ for that looks like this:
-- @{ "And": [ {"Or": [ {"Dimensions": { "Key": "REGION", "Values": [ "us-east-1", "us-west-1" ] }}, {"Tags": { "Key": "TagName", "Values": ["Value1"] } } ]}, {"Not": {"Dimensions": { "Key": "USAGE_TYPE", "Values": ["DataTransfer"] }}} ] } @ 
-- @{ "And": [ ... ], "DimensionValues": { "Dimension": "USAGE_TYPE", "Values": [ "DataTransfer" ] } } @ 
--
--
--
-- /See:/ 'mkExpression' smart constructor.
data Expression = Expression'
  { and :: Core.Maybe [Expression]
    -- ^ Return results that match both @Dimension@ objects.
  , costCategories :: Core.Maybe Types.CostCategoryValues
    -- ^ The filter based on @CostCategory@ values.
  , dimensions :: Core.Maybe Types.DimensionValues
    -- ^ The specific @Dimension@ to use for @Expression@ .
  , not :: Core.Maybe Expression
    -- ^ Return results that don't match a @Dimension@ object.
  , or :: Core.Maybe [Expression]
    -- ^ Return results that match either @Dimension@ object.
  , tags :: Core.Maybe Types.TagValues
    -- ^ The specific @Tag@ to use for @Expression@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Expression' value with any optional fields omitted.
mkExpression
    :: Expression
mkExpression
  = Expression'{and = Core.Nothing, costCategories = Core.Nothing,
                dimensions = Core.Nothing, not = Core.Nothing, or = Core.Nothing,
                tags = Core.Nothing}

-- | Return results that match both @Dimension@ objects.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAnd :: Lens.Lens' Expression (Core.Maybe [Expression])
eAnd = Lens.field @"and"
{-# INLINEABLE eAnd #-}
{-# DEPRECATED and "Use generic-lens or generic-optics with 'and' instead"  #-}

-- | The filter based on @CostCategory@ values.
--
-- /Note:/ Consider using 'costCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCostCategories :: Lens.Lens' Expression (Core.Maybe Types.CostCategoryValues)
eCostCategories = Lens.field @"costCategories"
{-# INLINEABLE eCostCategories #-}
{-# DEPRECATED costCategories "Use generic-lens or generic-optics with 'costCategories' instead"  #-}

-- | The specific @Dimension@ to use for @Expression@ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDimensions :: Lens.Lens' Expression (Core.Maybe Types.DimensionValues)
eDimensions = Lens.field @"dimensions"
{-# INLINEABLE eDimensions #-}
{-# DEPRECATED dimensions "Use generic-lens or generic-optics with 'dimensions' instead"  #-}

-- | Return results that don't match a @Dimension@ object.
--
-- /Note:/ Consider using 'not' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eNot :: Lens.Lens' Expression (Core.Maybe Expression)
eNot = Lens.field @"not"
{-# INLINEABLE eNot #-}
{-# DEPRECATED not "Use generic-lens or generic-optics with 'not' instead"  #-}

-- | Return results that match either @Dimension@ object.
--
-- /Note:/ Consider using 'or' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOr :: Lens.Lens' Expression (Core.Maybe [Expression])
eOr = Lens.field @"or"
{-# INLINEABLE eOr #-}
{-# DEPRECATED or "Use generic-lens or generic-optics with 'or' instead"  #-}

-- | The specific @Tag@ to use for @Expression@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' Expression (Core.Maybe Types.TagValues)
eTags = Lens.field @"tags"
{-# INLINEABLE eTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromJSON Expression where
        toJSON Expression{..}
          = Core.object
              (Core.catMaybes
                 [("And" Core..=) Core.<$> and,
                  ("CostCategories" Core..=) Core.<$> costCategories,
                  ("Dimensions" Core..=) Core.<$> dimensions,
                  ("Not" Core..=) Core.<$> not, ("Or" Core..=) Core.<$> or,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.FromJSON Expression where
        parseJSON
          = Core.withObject "Expression" Core.$
              \ x ->
                Expression' Core.<$>
                  (x Core..:? "And") Core.<*> x Core..:? "CostCategories" Core.<*>
                    x Core..:? "Dimensions"
                    Core.<*> x Core..:? "Not"
                    Core.<*> x Core..:? "Or"
                    Core.<*> x Core..:? "Tags"
