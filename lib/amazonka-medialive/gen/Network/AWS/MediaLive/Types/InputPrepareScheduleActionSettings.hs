-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.InputPrepareScheduleActionSettings
  ( InputPrepareScheduleActionSettings (..),

    -- * Smart constructor
    mkInputPrepareScheduleActionSettings,

    -- * Lenses
    ipsasInputAttachmentNameReference,
    ipsasInputClippingSettings,
    ipsasURLPath,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaLive.Types.InputClippingSettings
import qualified Network.AWS.Prelude as Lude

-- | Action to prepare an input for a future immediate input switch.
--
-- /See:/ 'mkInputPrepareScheduleActionSettings' smart constructor.
data InputPrepareScheduleActionSettings = InputPrepareScheduleActionSettings'
  { inputAttachmentNameReference ::
      Lude.Maybe Lude.Text,
    inputClippingSettings ::
      Lude.Maybe
        InputClippingSettings,
    urlPath ::
      Lude.Maybe
        [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputPrepareScheduleActionSettings' with the minimum fields required to make a request.
--
-- * 'inputAttachmentNameReference' - The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
-- * 'inputClippingSettings' - Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
-- * 'urlPath' - The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
mkInputPrepareScheduleActionSettings ::
  InputPrepareScheduleActionSettings
mkInputPrepareScheduleActionSettings =
  InputPrepareScheduleActionSettings'
    { inputAttachmentNameReference =
        Lude.Nothing,
      inputClippingSettings = Lude.Nothing,
      urlPath = Lude.Nothing
    }

-- | The name of the input attachment that should be prepared by this action. If no name is provided, the action will stop the most recent prepare (if any) when activated.
--
-- /Note:/ Consider using 'inputAttachmentNameReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsasInputAttachmentNameReference :: Lens.Lens' InputPrepareScheduleActionSettings (Lude.Maybe Lude.Text)
ipsasInputAttachmentNameReference = Lens.lens (inputAttachmentNameReference :: InputPrepareScheduleActionSettings -> Lude.Maybe Lude.Text) (\s a -> s {inputAttachmentNameReference = a} :: InputPrepareScheduleActionSettings)
{-# DEPRECATED ipsasInputAttachmentNameReference "Use generic-lens or generic-optics with 'inputAttachmentNameReference' instead." #-}

-- | Settings to let you create a clip of the file input, in order to set up the input to ingest only a portion of the file.
--
-- /Note:/ Consider using 'inputClippingSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsasInputClippingSettings :: Lens.Lens' InputPrepareScheduleActionSettings (Lude.Maybe InputClippingSettings)
ipsasInputClippingSettings = Lens.lens (inputClippingSettings :: InputPrepareScheduleActionSettings -> Lude.Maybe InputClippingSettings) (\s a -> s {inputClippingSettings = a} :: InputPrepareScheduleActionSettings)
{-# DEPRECATED ipsasInputClippingSettings "Use generic-lens or generic-optics with 'inputClippingSettings' instead." #-}

-- | The value for the variable portion of the URL for the dynamic input, for this instance of the input. Each time you use the same dynamic input in an input switch action, you can provide a different value, in order to connect the input to a different content source.
--
-- /Note:/ Consider using 'urlPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ipsasURLPath :: Lens.Lens' InputPrepareScheduleActionSettings (Lude.Maybe [Lude.Text])
ipsasURLPath = Lens.lens (urlPath :: InputPrepareScheduleActionSettings -> Lude.Maybe [Lude.Text]) (\s a -> s {urlPath = a} :: InputPrepareScheduleActionSettings)
{-# DEPRECATED ipsasURLPath "Use generic-lens or generic-optics with 'urlPath' instead." #-}

instance Lude.FromJSON InputPrepareScheduleActionSettings where
  parseJSON =
    Lude.withObject
      "InputPrepareScheduleActionSettings"
      ( \x ->
          InputPrepareScheduleActionSettings'
            Lude.<$> (x Lude..:? "inputAttachmentNameReference")
            Lude.<*> (x Lude..:? "inputClippingSettings")
            Lude.<*> (x Lude..:? "urlPath" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON InputPrepareScheduleActionSettings where
  toJSON InputPrepareScheduleActionSettings' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("inputAttachmentNameReference" Lude..=)
              Lude.<$> inputAttachmentNameReference,
            ("inputClippingSettings" Lude..=) Lude.<$> inputClippingSettings,
            ("urlPath" Lude..=) Lude.<$> urlPath
          ]
      )
