{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.FilterActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.FilterActivity
  ( FilterActivity (..),

    -- * Smart constructor
    mkFilterActivity,

    -- * Lenses
    faNext,
    faName,
    faFilter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An activity that filters a message based on its attributes.
--
-- /See:/ 'mkFilterActivity' smart constructor.
data FilterActivity = FilterActivity'
  { -- | The next activity in the pipeline.
    next :: Lude.Maybe Lude.Text,
    -- | The name of the filter activity.
    name :: Lude.Text,
    -- | An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity.
    filter :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FilterActivity' with the minimum fields required to make a request.
--
-- * 'next' - The next activity in the pipeline.
-- * 'name' - The name of the filter activity.
-- * 'filter' - An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity.
mkFilterActivity ::
  -- | 'name'
  Lude.Text ->
  -- | 'filter'
  Lude.Text ->
  FilterActivity
mkFilterActivity pName_ pFilter_ =
  FilterActivity'
    { next = Lude.Nothing,
      name = pName_,
      filter = pFilter_
    }

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faNext :: Lens.Lens' FilterActivity (Lude.Maybe Lude.Text)
faNext = Lens.lens (next :: FilterActivity -> Lude.Maybe Lude.Text) (\s a -> s {next = a} :: FilterActivity)
{-# DEPRECATED faNext "Use generic-lens or generic-optics with 'next' instead." #-}

-- | The name of the filter activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faName :: Lens.Lens' FilterActivity Lude.Text
faName = Lens.lens (name :: FilterActivity -> Lude.Text) (\s a -> s {name = a} :: FilterActivity)
{-# DEPRECATED faName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An expression that looks like a SQL WHERE clause that must return a Boolean value. Messages that satisfy the condition are passed to the next activity.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
faFilter :: Lens.Lens' FilterActivity Lude.Text
faFilter = Lens.lens (filter :: FilterActivity -> Lude.Text) (\s a -> s {filter = a} :: FilterActivity)
{-# DEPRECATED faFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.FromJSON FilterActivity where
  parseJSON =
    Lude.withObject
      "FilterActivity"
      ( \x ->
          FilterActivity'
            Lude.<$> (x Lude..:? "next")
            Lude.<*> (x Lude..: "name")
            Lude.<*> (x Lude..: "filter")
      )

instance Lude.ToJSON FilterActivity where
  toJSON FilterActivity' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("next" Lude..=) Lude.<$> next,
            Lude.Just ("name" Lude..= name),
            Lude.Just ("filter" Lude..= filter)
          ]
      )
