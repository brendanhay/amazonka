{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.TimeBasedLinear
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.TimeBasedLinear
  ( TimeBasedLinear (..),

    -- * Smart constructor
    mkTimeBasedLinear,

    -- * Lenses
    tblLinearInterval,
    tblLinearPercentage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A configuration that shifts traffic from one version of a Lambda function or ECS task set to another in equal increments, with an equal number of minutes between each increment. The original and target Lambda function versions or ECS task sets are specified in the deployment's AppSpec file.
--
-- /See:/ 'mkTimeBasedLinear' smart constructor.
data TimeBasedLinear = TimeBasedLinear'
  { -- | The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
    linearInterval :: Lude.Maybe Lude.Int,
    -- | The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
    linearPercentage :: Lude.Maybe Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimeBasedLinear' with the minimum fields required to make a request.
--
-- * 'linearInterval' - The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
-- * 'linearPercentage' - The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
mkTimeBasedLinear ::
  TimeBasedLinear
mkTimeBasedLinear =
  TimeBasedLinear'
    { linearInterval = Lude.Nothing,
      linearPercentage = Lude.Nothing
    }

-- | The number of minutes between each incremental traffic shift of a @TimeBasedLinear@ deployment.
--
-- /Note:/ Consider using 'linearInterval' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tblLinearInterval :: Lens.Lens' TimeBasedLinear (Lude.Maybe Lude.Int)
tblLinearInterval = Lens.lens (linearInterval :: TimeBasedLinear -> Lude.Maybe Lude.Int) (\s a -> s {linearInterval = a} :: TimeBasedLinear)
{-# DEPRECATED tblLinearInterval "Use generic-lens or generic-optics with 'linearInterval' instead." #-}

-- | The percentage of traffic that is shifted at the start of each increment of a @TimeBasedLinear@ deployment.
--
-- /Note:/ Consider using 'linearPercentage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tblLinearPercentage :: Lens.Lens' TimeBasedLinear (Lude.Maybe Lude.Int)
tblLinearPercentage = Lens.lens (linearPercentage :: TimeBasedLinear -> Lude.Maybe Lude.Int) (\s a -> s {linearPercentage = a} :: TimeBasedLinear)
{-# DEPRECATED tblLinearPercentage "Use generic-lens or generic-optics with 'linearPercentage' instead." #-}

instance Lude.FromJSON TimeBasedLinear where
  parseJSON =
    Lude.withObject
      "TimeBasedLinear"
      ( \x ->
          TimeBasedLinear'
            Lude.<$> (x Lude..:? "linearInterval")
            Lude.<*> (x Lude..:? "linearPercentage")
      )

instance Lude.ToJSON TimeBasedLinear where
  toJSON TimeBasedLinear' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("linearInterval" Lude..=) Lude.<$> linearInterval,
            ("linearPercentage" Lude..=) Lude.<$> linearPercentage
          ]
      )
