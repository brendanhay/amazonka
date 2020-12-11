-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputSwitchScheduleActionSettings
  ( InputSwitchScheduleActionSettings (..),

    -- * Smart constructor
    mkInputSwitchScheduleActionSettings,

    -- * Lenses
    issasInputClippingSettings,
    issasURLPath,
    issasInputAttachmentNameReference,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputClippingSettings
import qualified Network.AWS.Prelude as Lude

-- | Settings for the "switch input" action: to switch from ingesting one input to ingesting another input.
--
-- /See:/ 'mkInputSwitchScheduleActionSettings' smart constructor.
data InputSwitchScheduleActionSettings = InputSwitchScheduleActionSettings'
  { inputClippingSettings ::
      Lude.Maybe
        InputClippingSettings,
    urlPath ::
      Lude.Maybe [Lude.Text],
    inputAttachmentNameReference ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputSwitchScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'inputAttachmentNameReference' - The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
-- * 'inputClippingSettings' - Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
-- * 'urlPath' - The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
mkInputSwitchScheduleActionSettings ::
  -- | 'inputAttachmentNameReference'
  Lude.Text ->
  InputSwitchScheduleActionSettings
mkInputSwitchScheduleActionSettings pInputAttachmentNameReference_ =
  InputSwitchScheduleActionSettings'
    { inputClippingSettings =
        Lude.Nothing,
      urlPath = Lude.Nothing,
      inputAttachmentNameReference =
        pInputAttachmentNameReference_
    }

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /Note:/ Consider using 'inputClippingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issasInputClippingSettings :: Lens.Lens' InputSwitchScheduleActionSettings (Lude.Maybe InputClippingSettings)
issasInputClippingSettings = Lens.lens (inputClippingSettings :: InputSwitchScheduleActionSettings -> Lude.Maybe InputClippingSettings) (\s a -> s {inputClippingSettings = a} :: InputSwitchScheduleActionSettings)
{-# DEPRECATED issasInputClippingSettings "Use generic-lens or generic-optics with 'inputClippingSettings' instead." #-}

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- /Note:/ Consider using 'urlPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issasURLPath :: Lens.Lens' InputSwitchScheduleActionSettings (Lude.Maybe [Lude.Text])
issasURLPath = Lens.lens (urlPath :: InputSwitchScheduleActionSettings -> Lude.Maybe [Lude.Text]) (\s a -> s {urlPath = a} :: InputSwitchScheduleActionSettings)
{-# DEPRECATED issasURLPath "Use generic-lens or generic-optics with 'urlPath' instead." #-}

-- | The name of the input attachment (not the name of the input!) to switch to. The name is specified in the channel configuration.
--
-- /Note:/ Consider using 'inputAttachmentNameReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
issasInputAttachmentNameReference :: Lens.Lens' InputSwitchScheduleActionSettings Lude.Text
issasInputAttachmentNameReference = Lens.lens (inputAttachmentNameReference :: InputSwitchScheduleActionSettings -> Lude.Text) (\s a -> s {inputAttachmentNameReference = a} :: InputSwitchScheduleActionSettings)
{-# DEPRECATED issasInputAttachmentNameReference "Use generic-lens or generic-optics with 'inputAttachmentNameReference' instead." #-}

instance Lude.FromJSON InputSwitchScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "InputSwitchScheduleActionSettings"
      ( \x ->
          InputSwitchScheduleActionSettings'
            Lude.<$> (x Lude..:? "inputClippingSettings")
            Lude.<*> (x Lude..:? "urlPath" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "inputAttachmentNameReference")
      )

instance Lude.ToJSON InputSwitchScheduleActionSettings where
  toJSON InputSwitchScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputClippingSettings" Lude..=) Lude.<$> inputClippingSettings,
            ("urlPath" Lude..=) Lude.<$> urlPath,
            Lude.Just
              ( "inputAttachmentNameReference"
                  Lude..= inputAttachmentNameReference
              )
          ]
      )
