{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.MeetingSetting
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.MeetingSetting
  ( MeetingSetting (..),

    -- * Smart constructor
    mkMeetingSetting,

    -- * Lenses
    msRequirePin,
  )
where

import Network.AWS.AlexaBusiness.Types.RequirePin
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The values that indicate whether a pin is always required (YES), never required (NO), or OPTIONAL.
--
--
--     * If YES, Alexa will always ask for a meeting pin.
--
--
--     * If NO, Alexa will never ask for a meeting pin.
--
--
--     * If OPTIONAL, Alexa will ask if you have a meeting pin and if the customer responds with yes, it will ask for the meeting pin.
--
--
--
-- /See:/ 'mkMeetingSetting' smart constructor.
newtype MeetingSetting = MeetingSetting' {requirePin :: RequirePin}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MeetingSetting' with the minimum fields required to make a request.
--
-- * 'requirePin' - The values that indicate whether the pin is always required.
mkMeetingSetting ::
  -- | 'requirePin'
  RequirePin ->
  MeetingSetting
mkMeetingSetting pRequirePin_ =
  MeetingSetting' {requirePin = pRequirePin_}

-- | The values that indicate whether the pin is always required.
--
-- /Note:/ Consider using 'requirePin' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msRequirePin :: Lens.Lens' MeetingSetting RequirePin
msRequirePin = Lens.lens (requirePin :: MeetingSetting -> RequirePin) (\s a -> s {requirePin = a} :: MeetingSetting)
{-# DEPRECATED msRequirePin "Use generic-lens or generic-optics with 'requirePin' instead." #-}

instance Lude.FromJSON MeetingSetting where
  parseJSON =
    Lude.withObject
      "MeetingSetting"
      (\x -> MeetingSetting' Lude.<$> (x Lude..: "RequirePin"))

instance Lude.ToJSON MeetingSetting where
  toJSON MeetingSetting' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("RequirePin" Lude..= requirePin)])
