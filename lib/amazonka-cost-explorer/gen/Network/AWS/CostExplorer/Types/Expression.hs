-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.Expression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.Expression
  ( Expression (..),

    -- * Smart constructor
    mkExpression,

    -- * Lenses
    eNot,
    eAnd,
    eOr,
    eCostCategories,
    eDimensions,
    eTags,
  )
where

import Network.AWS.CostExplorer.Types.CostCategoryValues
import Network.AWS.CostExplorer.Types.DimensionValues
import Network.AWS.CostExplorer.Types.TagValues
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
  { not :: Lude.Maybe Expression,
    and :: Lude.Maybe [Expression],
    or :: Lude.Maybe [Expression],
    costCategories :: Lude.Maybe CostCategoryValues,
    dimensions :: Lude.Maybe DimensionValues,
    tags :: Lude.Maybe TagValues
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Expression' with the minimum fields required to make a request.
--
-- * 'and' - Return results that match both @Dimension@ objects.
-- * 'costCategories' - The filter based on @CostCategory@ values.
-- * 'dimensions' - The specific @Dimension@ to use for @Expression@ .
-- * 'not' - Return results that don't match a @Dimension@ object.
-- * 'or' - Return results that match either @Dimension@ object.
-- * 'tags' - The specific @Tag@ to use for @Expression@ .
mkExpression ::
  Expression
mkExpression =
  Expression'
    { not = Lude.Nothing,
      and = Lude.Nothing,
      or = Lude.Nothing,
      costCategories = Lude.Nothing,
      dimensions = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | Return results that don't match a @Dimension@ object.
--
-- /Note:/ Consider using 'not' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eNot :: Lens.Lens' Expression (Lude.Maybe Expression)
eNot = Lens.lens (not :: Expression -> Lude.Maybe Expression) (\s a -> s {not = a} :: Expression)
{-# DEPRECATED eNot "Use generic-lens or generic-optics with 'not' instead." #-}

-- | Return results that match both @Dimension@ objects.
--
-- /Note:/ Consider using 'and' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eAnd :: Lens.Lens' Expression (Lude.Maybe [Expression])
eAnd = Lens.lens (and :: Expression -> Lude.Maybe [Expression]) (\s a -> s {and = a} :: Expression)
{-# DEPRECATED eAnd "Use generic-lens or generic-optics with 'and' instead." #-}

-- | Return results that match either @Dimension@ object.
--
-- /Note:/ Consider using 'or' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eOr :: Lens.Lens' Expression (Lude.Maybe [Expression])
eOr = Lens.lens (or :: Expression -> Lude.Maybe [Expression]) (\s a -> s {or = a} :: Expression)
{-# DEPRECATED eOr "Use generic-lens or generic-optics with 'or' instead." #-}

-- | The filter based on @CostCategory@ values.
--
-- /Note:/ Consider using 'costCategories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eCostCategories :: Lens.Lens' Expression (Lude.Maybe CostCategoryValues)
eCostCategories = Lens.lens (costCategories :: Expression -> Lude.Maybe CostCategoryValues) (\s a -> s {costCategories = a} :: Expression)
{-# DEPRECATED eCostCategories "Use generic-lens or generic-optics with 'costCategories' instead." #-}

-- | The specific @Dimension@ to use for @Expression@ .
--
-- /Note:/ Consider using 'dimensions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eDimensions :: Lens.Lens' Expression (Lude.Maybe DimensionValues)
eDimensions = Lens.lens (dimensions :: Expression -> Lude.Maybe DimensionValues) (\s a -> s {dimensions = a} :: Expression)
{-# DEPRECATED eDimensions "Use generic-lens or generic-optics with 'dimensions' instead." #-}

-- | The specific @Tag@ to use for @Expression@ .
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eTags :: Lens.Lens' Expression (Lude.Maybe TagValues)
eTags = Lens.lens (tags :: Expression -> Lude.Maybe TagValues) (\s a -> s {tags = a} :: Expression)
{-# DEPRECATED eTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromJSON Expression where
  parseJSON =
    Lude.withObject
      "Expression"
      ( \x ->
          Expression'
            Lude.<$> (x Lude..:? "Not")
            Lude.<*> (x Lude..:? "And" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Or" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CostCategories")
            Lude.<*> (x Lude..:? "Dimensions")
            Lude.<*> (x Lude..:? "Tags")
      )

instance Lude.ToJSON Expression where
  toJSON Expression' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Not" Lude..=) Lude.<$> not,
            ("And" Lude..=) Lude.<$> and,
            ("Or" Lude..=) Lude.<$> or,
            ("CostCategories" Lude..=) Lude.<$> costCategories,
            ("Dimensions" Lude..=) Lude.<$> dimensions,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )
