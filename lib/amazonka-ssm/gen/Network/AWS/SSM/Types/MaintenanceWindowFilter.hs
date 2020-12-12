{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowFilter
  ( MaintenanceWindowFilter (..),

    -- * Smart constructor
    mkMaintenanceWindowFilter,

    -- * Lenses
    mwfValues,
    mwfKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Filter used in the request. Supported filter keys are Name and Enabled.
--
-- /See:/ 'mkMaintenanceWindowFilter' smart constructor.
data MaintenanceWindowFilter = MaintenanceWindowFilter'
  { values ::
      Lude.Maybe [Lude.Text],
    key :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowFilter' with the minimum fields required to make a request.
--
-- * 'key' - The name of the filter.
-- * 'values' - The filter values.
mkMaintenanceWindowFilter ::
  MaintenanceWindowFilter
mkMaintenanceWindowFilter =
  MaintenanceWindowFilter'
    { values = Lude.Nothing,
      key = Lude.Nothing
    }

-- | The filter values.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwfValues :: Lens.Lens' MaintenanceWindowFilter (Lude.Maybe [Lude.Text])
mwfValues = Lens.lens (values :: MaintenanceWindowFilter -> Lude.Maybe [Lude.Text]) (\s a -> s {values = a} :: MaintenanceWindowFilter)
{-# DEPRECATED mwfValues "Use generic-lens or generic-optics with 'values' instead." #-}

-- | The name of the filter.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwfKey :: Lens.Lens' MaintenanceWindowFilter (Lude.Maybe Lude.Text)
mwfKey = Lens.lens (key :: MaintenanceWindowFilter -> Lude.Maybe Lude.Text) (\s a -> s {key = a} :: MaintenanceWindowFilter)
{-# DEPRECATED mwfKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.ToJSON MaintenanceWindowFilter where
  toJSON MaintenanceWindowFilter' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Values" Lude..=) Lude.<$> values, ("Key" Lude..=) Lude.<$> key]
      )
