{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelClientConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelClientConfig
  ( ModelClientConfig (..),

    -- * Smart constructor
    mkModelClientConfig,

    -- * Lenses
    mccInvocationsTimeoutInSeconds,
    mccInvocationsMaxRetries,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Configures the timeout and maximum number of retries for processing a transform job invocation.
--
-- /See:/ 'mkModelClientConfig' smart constructor.
data ModelClientConfig = ModelClientConfig'
  { -- | The timeout value in seconds for an invocation request.
    invocationsTimeoutInSeconds :: Lude.Maybe Lude.Natural,
    -- | The maximum number of retries when invocation requests are failing.
    invocationsMaxRetries :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelClientConfig' with the minimum fields required to make a request.
--
-- * 'invocationsTimeoutInSeconds' - The timeout value in seconds for an invocation request.
-- * 'invocationsMaxRetries' - The maximum number of retries when invocation requests are failing.
mkModelClientConfig ::
  ModelClientConfig
mkModelClientConfig =
  ModelClientConfig'
    { invocationsTimeoutInSeconds = Lude.Nothing,
      invocationsMaxRetries = Lude.Nothing
    }

-- | The timeout value in seconds for an invocation request.
--
-- /Note:/ Consider using 'invocationsTimeoutInSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInvocationsTimeoutInSeconds :: Lens.Lens' ModelClientConfig (Lude.Maybe Lude.Natural)
mccInvocationsTimeoutInSeconds = Lens.lens (invocationsTimeoutInSeconds :: ModelClientConfig -> Lude.Maybe Lude.Natural) (\s a -> s {invocationsTimeoutInSeconds = a} :: ModelClientConfig)
{-# DEPRECATED mccInvocationsTimeoutInSeconds "Use generic-lens or generic-optics with 'invocationsTimeoutInSeconds' instead." #-}

-- | The maximum number of retries when invocation requests are failing.
--
-- /Note:/ Consider using 'invocationsMaxRetries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mccInvocationsMaxRetries :: Lens.Lens' ModelClientConfig (Lude.Maybe Lude.Natural)
mccInvocationsMaxRetries = Lens.lens (invocationsMaxRetries :: ModelClientConfig -> Lude.Maybe Lude.Natural) (\s a -> s {invocationsMaxRetries = a} :: ModelClientConfig)
{-# DEPRECATED mccInvocationsMaxRetries "Use generic-lens or generic-optics with 'invocationsMaxRetries' instead." #-}

instance Lude.FromJSON ModelClientConfig where
  parseJSON =
    Lude.withObject
      "ModelClientConfig"
      ( \x ->
          ModelClientConfig'
            Lude.<$> (x Lude..:? "InvocationsTimeoutInSeconds")
            Lude.<*> (x Lude..:? "InvocationsMaxRetries")
      )

instance Lude.ToJSON ModelClientConfig where
  toJSON ModelClientConfig' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InvocationsTimeoutInSeconds" Lude..=)
              Lude.<$> invocationsTimeoutInSeconds,
            ("InvocationsMaxRetries" Lude..=) Lude.<$> invocationsMaxRetries
          ]
      )
