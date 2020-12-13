{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Filter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Filter
  ( Filter (..),

    -- * Smart constructor
    mkFilter,

    -- * Lenses
    fValues,
    fName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Identifies the name and value of a filter object. This filter is used to limit the number and type of AWS DMS objects that are returned for a particular @Describe*@ call or similar operation. Filters are used as an optional parameter to the following APIs.
--
-- /See:/ 'mkFilter' smart constructor.
data Filter = Filter'
  { -- | The filter value, which can specify one or more values used to narrow the returned results.
    values :: [Lude.Text],
    -- | The name of the filter as specified for a @Describe*@ or similar operation.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Filter' with the minimum fields required to make a request.
--
-- * 'values' - The filter value, which can specify one or more values used to narrow the returned results.
-- * 'name' - The name of the filter as specified for a @Describe*@ or similar operation.
mkFilter ::
  -- | 'name'
  Lude.Text ->
  Filter
mkFilter pName_ = Filter' {values = Lude.mempty, name = pName_}

-- | The filter value, which can specify one or more values used to narrow the returned results.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fValues :: Lens.Lens' Filter [Lude.Text]
fValues = Lens.lens (values :: Filter -> [Lude.Text]) (\s a -> s {values = a} :: Filter)
{-# DEPRECATED fValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter as specified for a @Describe*@ or similar operation.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fName :: Lens.Lens' Filter Lude.Text
fName = Lens.lens (name :: Filter -> Lude.Text) (\s a -> s {name = a} :: Filter)
{-# DEPRECATED fName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON Filter where
  toJSON Filter' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Values" Lude..= values),
            Lude.Just ("Name" Lude..= name)
          ]
      )
