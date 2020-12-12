{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.FailoverConditionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.FailoverConditionSettings
  ( FailoverConditionSettings (..),

    -- * Smart constructor
    mkFailoverConditionSettings,

    -- * Lenses
    fcsInputLossSettings,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputLossFailoverSettings
import qualified Network.AWS.Prelude as Lude

-- | Settings for one failover condition.
--
-- /See:/ 'mkFailoverConditionSettings' smart constructor.
newtype FailoverConditionSettings = FailoverConditionSettings'
  { inputLossSettings ::
      Lude.Maybe InputLossFailoverSettings
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FailoverConditionSettings' with the minimum fields required to make a request.
--
-- * 'inputLossSettings' - MediaLive will perform a failover if content is not detected in this input for the specified period.
mkFailoverConditionSettings ::
  FailoverConditionSettings
mkFailoverConditionSettings =
  FailoverConditionSettings' {inputLossSettings = Lude.Nothing}

-- | MediaLive will perform a failover if content is not detected in this input for the specified period.
--
-- /Note:/ Consider using 'inputLossSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fcsInputLossSettings :: Lens.Lens' FailoverConditionSettings (Lude.Maybe InputLossFailoverSettings)
fcsInputLossSettings = Lens.lens (inputLossSettings :: FailoverConditionSettings -> Lude.Maybe InputLossFailoverSettings) (\s a -> s {inputLossSettings = a} :: FailoverConditionSettings)
{-# DEPRECATED fcsInputLossSettings "Use generic-lens or generic-optics with 'inputLossSettings' instead." #-}

instance Lude.FromJSON FailoverConditionSettings where
  parseJSON =
    Lude.withObject
      "FailoverConditionSettings"
      ( \x ->
          FailoverConditionSettings'
            Lude.<$> (x Lude..:? "inputLossSettings")
      )

instance Lude.ToJSON FailoverConditionSettings where
  toJSON FailoverConditionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [("inputLossSettings" Lude..=) Lude.<$> inputLossSettings]
      )
