-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputLossFailoverSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputLossFailoverSettings
  ( InputLossFailoverSettings (..),

    -- * Smart constructor
    mkInputLossFailoverSettings,

    -- * Lenses
    ilfsInputLossThresholdMsec,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | MediaLive will perform a failover if content is not detected in this input for the specified period.
--
-- /See:/ 'mkInputLossFailoverSettings' smart constructor.
newtype InputLossFailoverSettings = InputLossFailoverSettings'
  { inputLossThresholdMsec ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLossFailoverSettings' with the minimum fields required to make a request.
--
-- * 'inputLossThresholdMsec' - The amount of time (in milliseconds) that no input is detected. After that time, an input failover will occur.
mkInputLossFailoverSettings ::
  InputLossFailoverSettings
mkInputLossFailoverSettings =
  InputLossFailoverSettings' {inputLossThresholdMsec = Lude.Nothing}

-- | The amount of time (in milliseconds) that no input is detected. After that time, an input failover will occur.
--
-- /Note:/ Consider using 'inputLossThresholdMsec' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilfsInputLossThresholdMsec :: Lens.Lens' InputLossFailoverSettings (Lude.Maybe Lude.Natural)
ilfsInputLossThresholdMsec = Lens.lens (inputLossThresholdMsec :: InputLossFailoverSettings -> Lude.Maybe Lude.Natural) (\s a -> s {inputLossThresholdMsec = a} :: InputLossFailoverSettings)
{-# DEPRECATED ilfsInputLossThresholdMsec "Use generic-lens or generic-optics with 'inputLossThresholdMsec' instead." #-}

instance Lude.FromJSON InputLossFailoverSettings where
  parseJSON =
    Lude.withObject
      "InputLossFailoverSettings"
      ( \x ->
          InputLossFailoverSettings'
            Lude.<$> (x Lude..:? "inputLossThresholdMsec")
      )

instance Lude.ToJSON InputLossFailoverSettings where
  toJSON InputLossFailoverSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputLossThresholdMsec" Lude..=)
              Lude.<$> inputLossThresholdMsec
          ]
      )
