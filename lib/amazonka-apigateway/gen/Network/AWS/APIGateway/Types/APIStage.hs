{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.APIStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.APIStage
  ( APIStage (..),

    -- * Smart constructor
    mkAPIStage,

    -- * Lenses
    asStage,
    asApiId,
    asThrottle,
  )
where

import Network.AWS.APIGateway.Types.ThrottleSettings
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | API stage name of the associated API stage in a usage plan.
--
-- /See:/ 'mkAPIStage' smart constructor.
data APIStage = APIStage'
  { stage :: Lude.Maybe Lude.Text,
    apiId :: Lude.Maybe Lude.Text,
    throttle :: Lude.Maybe (Lude.HashMap Lude.Text (ThrottleSettings))
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'APIStage' with the minimum fields required to make a request.
--
-- * 'apiId' - API Id of the associated API stage in a usage plan.
-- * 'stage' - API stage name of the associated API stage in a usage plan.
-- * 'throttle' - Map containing method level throttling information for API stage in a usage plan.
mkAPIStage ::
  APIStage
mkAPIStage =
  APIStage'
    { stage = Lude.Nothing,
      apiId = Lude.Nothing,
      throttle = Lude.Nothing
    }

-- | API stage name of the associated API stage in a usage plan.
--
-- /Note:/ Consider using 'stage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asStage :: Lens.Lens' APIStage (Lude.Maybe Lude.Text)
asStage = Lens.lens (stage :: APIStage -> Lude.Maybe Lude.Text) (\s a -> s {stage = a} :: APIStage)
{-# DEPRECATED asStage "Use generic-lens or generic-optics with 'stage' instead." #-}

-- | API Id of the associated API stage in a usage plan.
--
-- /Note:/ Consider using 'apiId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApiId :: Lens.Lens' APIStage (Lude.Maybe Lude.Text)
asApiId = Lens.lens (apiId :: APIStage -> Lude.Maybe Lude.Text) (\s a -> s {apiId = a} :: APIStage)
{-# DEPRECATED asApiId "Use generic-lens or generic-optics with 'apiId' instead." #-}

-- | Map containing method level throttling information for API stage in a usage plan.
--
-- /Note:/ Consider using 'throttle' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asThrottle :: Lens.Lens' APIStage (Lude.Maybe (Lude.HashMap Lude.Text (ThrottleSettings)))
asThrottle = Lens.lens (throttle :: APIStage -> Lude.Maybe (Lude.HashMap Lude.Text (ThrottleSettings))) (\s a -> s {throttle = a} :: APIStage)
{-# DEPRECATED asThrottle "Use generic-lens or generic-optics with 'throttle' instead." #-}

instance Lude.FromJSON APIStage where
  parseJSON =
    Lude.withObject
      "APIStage"
      ( \x ->
          APIStage'
            Lude.<$> (x Lude..:? "stage")
            Lude.<*> (x Lude..:? "apiId")
            Lude.<*> (x Lude..:? "throttle" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON APIStage where
  toJSON APIStage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("stage" Lude..=) Lude.<$> stage,
            ("apiId" Lude..=) Lude.<$> apiId,
            ("throttle" Lude..=) Lude.<$> throttle
          ]
      )
