{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.SearchFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.SearchFilter
  ( SearchFilter (..),

    -- * Smart constructor
    mkSearchFilter,

    -- * Lenses
    sfAttribute,
    sfValues,
    sfOperator,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes criteria to restrict a list of results.
--
-- For operators that apply a single value to the attribute, the filter is evaluated as follows: @Attribute Operator Values[1]@
-- Some operators, e.g. @in@ , can apply multiple values. In this case, the filter is evaluated as a logical union (OR) of applications of the operator to the attribute with each one of the values: @(Attribute Operator Values[1]) OR (Attribute Operator Values[2]) OR ...@
-- The valid values for attributes of @SearchFilter@ depend on the API action. For valid values, see the reference page for the API action you're calling that takes a @SearchFilter@ parameter.
--
-- /See:/ 'mkSearchFilter' smart constructor.
data SearchFilter = SearchFilter'
  { attribute ::
      Lude.Maybe Lude.Text,
    values :: Lude.Maybe [Lude.Text],
    operator :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SearchFilter' with the minimum fields required to make a request.
--
-- * 'attribute' - The result attribute to which the filter values are applied. Valid values vary by API action.
-- * 'operator' - The operator to apply to the @Attribute@ with each of the @Values@ . Valid values vary by @Attribute@ .
-- * 'values' - The list of values applied to the @Attribute@ and @Operator@ attributes. Number of values and valid values vary by @Attribute@ .
mkSearchFilter ::
  SearchFilter
mkSearchFilter =
  SearchFilter'
    { attribute = Lude.Nothing,
      values = Lude.Nothing,
      operator = Lude.Nothing
    }

-- | The result attribute to which the filter values are applied. Valid values vary by API action.
--
-- /Note:/ Consider using 'attribute' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfAttribute :: Lens.Lens' SearchFilter (Lude.Maybe Lude.Text)
sfAttribute = Lens.lens (attribute :: SearchFilter -> Lude.Maybe Lude.Text) (\s a -> s {attribute = a} :: SearchFilter)
{-# DEPRECATED sfAttribute "Use generic-lens or generic-optics with 'attribute' instead." #-}

-- | The list of values applied to the @Attribute@ and @Operator@ attributes. Number of values and valid values vary by @Attribute@ .
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfValues :: Lens.Lens' SearchFilter (Lude.Maybe [Lude.Text])
sfValues = Lens.lens (values :: SearchFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: SearchFilter)
{-# DEPRECATED sfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The operator to apply to the @Attribute@ with each of the @Values@ . Valid values vary by @Attribute@ .
--
-- /Note:/ Consider using 'operator' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfOperator :: Lens.Lens' SearchFilter (Lude.Maybe Lude.Text)
sfOperator = Lens.lens (operator :: SearchFilter -> Lude.Maybe Lude.Text) (\s a -> s {operator = a} :: SearchFilter)
{-# DEPRECATED sfOperator "Use generic-lens or generic-optics with 'operator' instead." #-}

instance Lude.ToQuery SearchFilter where
  toQuery SearchFilter' {..} =
    Lude.mconcat
      [ "Attribute" Lude.=: attribute,
        "Values"
          Lude.=: Lude.toQuery (Lude.toQueryList "member" Lude.<$> values),
        "Operator" Lude.=: operator
      ]
