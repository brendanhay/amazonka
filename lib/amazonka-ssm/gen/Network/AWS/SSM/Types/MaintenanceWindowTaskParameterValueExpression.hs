{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskParameterValueExpression
  ( MaintenanceWindowTaskParameterValueExpression (..),

    -- * Smart constructor
    mkMaintenanceWindowTaskParameterValueExpression,

    -- * Lenses
    mwtpveValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines the values for a task parameter.
--
-- /See:/ 'mkMaintenanceWindowTaskParameterValueExpression' smart constructor.
newtype MaintenanceWindowTaskParameterValueExpression = MaintenanceWindowTaskParameterValueExpression'
  { -- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
    values :: Lude.Maybe [Lude.Sensitive Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowTaskParameterValueExpression' with the minimum fields required to make a request.
--
-- * 'values' - This field contains an array of 0 or more strings, each 1 to 255 characters in length.
mkMaintenanceWindowTaskParameterValueExpression ::
  MaintenanceWindowTaskParameterValueExpression
mkMaintenanceWindowTaskParameterValueExpression =
  MaintenanceWindowTaskParameterValueExpression'
    { values =
        Lude.Nothing
    }

-- | This field contains an array of 0 or more strings, each 1 to 255 characters in length.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwtpveValues :: Lens.Lens' MaintenanceWindowTaskParameterValueExpression (Lude.Maybe [Lude.Sensitive Lude.Text])
mwtpveValues = Lens.lens (values :: MaintenanceWindowTaskParameterValueExpression -> Lude.Maybe [Lude.Sensitive Lude.Text]) (\s a -> s {values = a} :: MaintenanceWindowTaskParameterValueExpression)
{-# DEPRECATED mwtpveValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance
  Lude.FromJSON
    MaintenanceWindowTaskParameterValueExpression
  where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowTaskParameterValueExpression"
      ( \x ->
          MaintenanceWindowTaskParameterValueExpression'
            Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON MaintenanceWindowTaskParameterValueExpression where
  toJSON MaintenanceWindowTaskParameterValueExpression' {..} =
    Lude.object (Lude.catMaybes [("Values" Lude..=) Lude.<$> values])
