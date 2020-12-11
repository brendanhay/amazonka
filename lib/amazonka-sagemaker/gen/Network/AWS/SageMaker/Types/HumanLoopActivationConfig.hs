-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanLoopActivationConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanLoopActivationConfig
  ( HumanLoopActivationConfig (..),

    -- * Smart constructor
    mkHumanLoopActivationConfig,

    -- * Lenses
    hlacHumanLoopActivationConditionsConfig,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig

-- | Provides information about how and under what conditions SageMaker creates a human loop. If @HumanLoopActivationConfig@ is not given, then all requests go to humans.
--
-- /See:/ 'mkHumanLoopActivationConfig' smart constructor.
newtype HumanLoopActivationConfig = HumanLoopActivationConfig'
  { humanLoopActivationConditionsConfig ::
      HumanLoopActivationConditionsConfig
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HumanLoopActivationConfig' with the minimum fields required to make a request.
--
-- * 'humanLoopActivationConditionsConfig' - Container structure for defining under what conditions SageMaker creates a human loop.
mkHumanLoopActivationConfig ::
  -- | 'humanLoopActivationConditionsConfig'
  HumanLoopActivationConditionsConfig ->
  HumanLoopActivationConfig
mkHumanLoopActivationConfig pHumanLoopActivationConditionsConfig_ =
  HumanLoopActivationConfig'
    { humanLoopActivationConditionsConfig =
        pHumanLoopActivationConditionsConfig_
    }

-- | Container structure for defining under what conditions SageMaker creates a human loop.
--
-- /Note:/ Consider using 'humanLoopActivationConditionsConfig' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hlacHumanLoopActivationConditionsConfig :: Lens.Lens' HumanLoopActivationConfig HumanLoopActivationConditionsConfig
hlacHumanLoopActivationConditionsConfig = Lens.lens (humanLoopActivationConditionsConfig :: HumanLoopActivationConfig -> HumanLoopActivationConditionsConfig) (\s a -> s {humanLoopActivationConditionsConfig = a} :: HumanLoopActivationConfig)
{-# DEPRECATED hlacHumanLoopActivationConditionsConfig "Use generic-lens or generic-optics with 'humanLoopActivationConditionsConfig' instead." #-}

instance Lude.FromJSON HumanLoopActivationConfig where
  parseJSON =
    Lude.withObject
      "HumanLoopActivationConfig"
      ( \x ->
          HumanLoopActivationConfig'
            Lude.<$> (x Lude..: "HumanLoopActivationConditionsConfig")
      )

instance Lude.ToJSON HumanLoopActivationConfig where
  toJSON HumanLoopActivationConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "HumanLoopActivationConditionsConfig"
                  Lude..= humanLoopActivationConditionsConfig
              )
          ]
      )
