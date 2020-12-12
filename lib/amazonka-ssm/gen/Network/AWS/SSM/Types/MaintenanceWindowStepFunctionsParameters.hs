{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
  ( MaintenanceWindowStepFunctionsParameters (..),

    -- * Smart constructor
    mkMaintenanceWindowStepFunctionsParameters,

    -- * Lenses
    mwsfpInput,
    mwsfpName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The parameters for a STEP_FUNCTIONS task.
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- /See:/ 'mkMaintenanceWindowStepFunctionsParameters' smart constructor.
data MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters'
  { input ::
      Lude.Maybe
        ( Lude.Sensitive
            Lude.Text
        ),
    name ::
      Lude.Maybe
        Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MaintenanceWindowStepFunctionsParameters' with the minimum fields required to make a request.
--
-- * 'input' - The inputs for the STEP_FUNCTIONS task.
-- * 'name' - The name of the STEP_FUNCTIONS task.
mkMaintenanceWindowStepFunctionsParameters ::
  MaintenanceWindowStepFunctionsParameters
mkMaintenanceWindowStepFunctionsParameters =
  MaintenanceWindowStepFunctionsParameters'
    { input = Lude.Nothing,
      name = Lude.Nothing
    }

-- | The inputs for the STEP_FUNCTIONS task.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsfpInput :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Lude.Maybe (Lude.Sensitive Lude.Text))
mwsfpInput = Lens.lens (input :: MaintenanceWindowStepFunctionsParameters -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: MaintenanceWindowStepFunctionsParameters)
{-# DEPRECATED mwsfpInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | The name of the STEP_FUNCTIONS task.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mwsfpName :: Lens.Lens' MaintenanceWindowStepFunctionsParameters (Lude.Maybe Lude.Text)
mwsfpName = Lens.lens (name :: MaintenanceWindowStepFunctionsParameters -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: MaintenanceWindowStepFunctionsParameters)
{-# DEPRECATED mwsfpName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON MaintenanceWindowStepFunctionsParameters where
  parseJSON =
    Lude.withObject
      "MaintenanceWindowStepFunctionsParameters"
      ( \x ->
          MaintenanceWindowStepFunctionsParameters'
            Lude.<$> (x Lude..:? "Input") Lude.<*> (x Lude..:? "Name")
      )

instance Lude.ToJSON MaintenanceWindowStepFunctionsParameters where
  toJSON MaintenanceWindowStepFunctionsParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Input" Lude..=) Lude.<$> input, ("Name" Lude..=) Lude.<$> name]
      )
