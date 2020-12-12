{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsAggregator
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsAggregator
  ( OpsAggregator (..),

    -- * Smart constructor
    mkOpsAggregator,

    -- * Lenses
    oaTypeName,
    oaAggregators,
    oaValues,
    oaFilters,
    oaAttributeName,
    oaAggregatorType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SSM.Types.OpsFilter

-- | One or more aggregators for viewing counts of OpsItems using different dimensions such as @Source@ , @CreatedTime@ , or @Source and CreatedTime@ , to name a few.
--
-- /See:/ 'mkOpsAggregator' smart constructor.
data OpsAggregator = OpsAggregator'
  { typeName ::
      Lude.Maybe Lude.Text,
    aggregators :: Lude.Maybe (Lude.NonEmpty OpsAggregator),
    values :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    filters :: Lude.Maybe (Lude.NonEmpty OpsFilter),
    attributeName :: Lude.Maybe Lude.Text,
    aggregatorType :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsAggregator' with the minimum fields required to make a request.
--
-- * 'aggregatorType' - Either a Range or Count aggregator for limiting an OpsItem summary.
-- * 'aggregators' - A nested aggregator for viewing counts of OpsItems.
-- * 'attributeName' - The name of an OpsItem attribute on which to limit the count of OpsItems.
-- * 'filters' - The aggregator filters.
-- * 'typeName' - The data type name to use for viewing counts of OpsItems.
-- * 'values' - The aggregator value.
mkOpsAggregator ::
  OpsAggregator
mkOpsAggregator =
  OpsAggregator'
    { typeName = Lude.Nothing,
      aggregators = Lude.Nothing,
      values = Lude.Nothing,
      filters = Lude.Nothing,
      attributeName = Lude.Nothing,
      aggregatorType = Lude.Nothing
    }

-- | The data type name to use for viewing counts of OpsItems.
--
-- /Note:/ Consider using 'typeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaTypeName :: Lens.Lens' OpsAggregator (Lude.Maybe Lude.Text)
oaTypeName = Lens.lens (typeName :: OpsAggregator -> Lude.Maybe Lude.Text) (\s a -> s {typeName = a} :: OpsAggregator)
{-# DEPRECATED oaTypeName "Use generic-lens or generic-optics with 'typeName' instead." #-}

-- | A nested aggregator for viewing counts of OpsItems.
--
-- /Note:/ Consider using 'aggregators' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaAggregators :: Lens.Lens' OpsAggregator (Lude.Maybe (Lude.NonEmpty OpsAggregator))
oaAggregators = Lens.lens (aggregators :: OpsAggregator -> Lude.Maybe (Lude.NonEmpty OpsAggregator)) (\s a -> s {aggregators = a} :: OpsAggregator)
{-# DEPRECATED oaAggregators "Use generic-lens or generic-optics with 'aggregators' instead." #-}

-- | The aggregator value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaValues :: Lens.Lens' OpsAggregator (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
oaValues = Lens.lens (values :: OpsAggregator -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {values = a} :: OpsAggregator)
{-# DEPRECATED oaValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The aggregator filters.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaFilters :: Lens.Lens' OpsAggregator (Lude.Maybe (Lude.NonEmpty OpsFilter))
oaFilters = Lens.lens (filters :: OpsAggregator -> Lude.Maybe (Lude.NonEmpty OpsFilter)) (\s a -> s {filters = a} :: OpsAggregator)
{-# DEPRECATED oaFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | The name of an OpsItem attribute on which to limit the count of OpsItems.
--
-- /Note:/ Consider using 'attributeName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaAttributeName :: Lens.Lens' OpsAggregator (Lude.Maybe Lude.Text)
oaAttributeName = Lens.lens (attributeName :: OpsAggregator -> Lude.Maybe Lude.Text) (\s a -> s {attributeName = a} :: OpsAggregator)
{-# DEPRECATED oaAttributeName "Use generic-lens or generic-optics with 'attributeName' instead." #-}

-- | Either a Range or Count aggregator for limiting an OpsItem summary.
--
-- /Note:/ Consider using 'aggregatorType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oaAggregatorType :: Lens.Lens' OpsAggregator (Lude.Maybe Lude.Text)
oaAggregatorType = Lens.lens (aggregatorType :: OpsAggregator -> Lude.Maybe Lude.Text) (\s a -> s {aggregatorType = a} :: OpsAggregator)
{-# DEPRECATED oaAggregatorType "Use generic-lens or generic-optics with 'aggregatorType' instead." #-}

instance Lude.ToJSON OpsAggregator where
  toJSON OpsAggregator' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TypeName" Lude..=) Lude.<$> typeName,
            ("Aggregators" Lude..=) Lude.<$> aggregators,
            ("Values" Lude..=) Lude.<$> values,
            ("Filters" Lude..=) Lude.<$> filters,
            ("AttributeName" Lude..=) Lude.<$> attributeName,
            ("AggregatorType" Lude..=) Lude.<$> aggregatorType
          ]
      )
