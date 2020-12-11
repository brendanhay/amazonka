-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Discovery.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fName,
    fValues,
    fCondition,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A filter that can use conditional operators.
--
-- For more information about filters, see <https://docs.aws.amazon.com/application-discovery/latest/userguide/discovery-api-queries.html Querying Discovered Configuration Items> in the /AWS Application Discovery Service User Guide/ .
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { name :: Lude.Text,
    values :: [Lude.Text],
    condition :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'condition' - A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
-- * 'name' - The name of the filter.
-- * 'values' - A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
mkFilter ::
  -- | 'name'
  Lude.Text ->
  -- | 'condition'
  Lude.Text ->
  Filter
mkFilter pName_ pCondition_ =
  Filter'
    { name = pName_,
      values = Lude.mempty,
      condition = pCondition_
    }

-- | The name of the filter.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Lude.Text
fName = Lens.lens (name :: Filter -> Lude.Text) (\s a -> s {name = a} :: Filter)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A string value on which to filter. For example, if you choose the @destinationServer.osVersion@ filter name, you could specify @Ubuntu@ for the value.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Lude.Text]
fValues = Lens.lens (values :: Filter -> [Lude.Text]) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | A conditional operator. The following operators are valid: EQUALS, NOT_EQUALS, CONTAINS, NOT_CONTAINS. If you specify multiple filters, the system utilizes all filters as though concatenated by /AND/ . If you specify multiple values for a particular filter, the system differentiates the values using /OR/ . Calling either /DescribeConfigurations/ or /ListConfigurations/ returns attributes of matching configuration items.
--
-- /Note:/ Consider using 'condition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fCondition :: Lens.Lens' Filter Lude.Text
fCondition = Lens.lens (condition :: Filter -> Lude.Text) (\s a -> s {condition = a} :: Filter)
{-# DEPRECATED fCondition "Use generic-lens or generic-optics with 'condition' instead." #-}

instance Lude.ToJSON Filter where
  toJSON Filter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("name" Lude..= name),
            Lude.Just ("values" Lude..= values),
            Lude.Just ("condition" Lude..= condition)
          ]
      )
