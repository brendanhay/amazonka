{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceMessage
  ( VoiceMessage (..),

    -- * Smart constructor
    mkVoiceMessage,

    -- * Lenses
    vmSubstitutions,
    vmLanguageCode,
    vmOriginationNumber,
    vmBody,
    vmVoiceId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Specifies the settings for a one-time voice message that's sent directly to an endpoint through the voice channel.
--
-- /See:/ 'mkVoiceMessage' smart constructor.
data VoiceMessage = VoiceMessage'
  { substitutions ::
      Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])),
    languageCode :: Lude.Maybe Lude.Text,
    originationNumber :: Lude.Maybe Lude.Text,
    body :: Lude.Maybe Lude.Text,
    voiceId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VoiceMessage' with the minimum fields required to make a request.
--
-- * 'body' - The text of the script to use for the voice message.
-- * 'languageCode' - The code for the language to use when synthesizing the text of the message script. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
-- * 'originationNumber' - The long code to send the voice message from. This value should be one of the dedicated long codes that's assigned to your AWS account. Although it isn't required, we recommend that you specify the long code in E.164 format, for example +12065550100, to ensure prompt and accurate delivery of the message.
-- * 'substitutions' - The default message variables to use in the voice message. You can override the default variables with individual address variables.
-- * 'voiceId' - The name of the voice to use when delivering the message. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
mkVoiceMessage ::
  VoiceMessage
mkVoiceMessage =
  VoiceMessage'
    { substitutions = Lude.Nothing,
      languageCode = Lude.Nothing,
      originationNumber = Lude.Nothing,
      body = Lude.Nothing,
      voiceId = Lude.Nothing
    }

-- | The default message variables to use in the voice message. You can override the default variables with individual address variables.
--
-- /Note:/ Consider using 'substitutions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmSubstitutions :: Lens.Lens' VoiceMessage (Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text])))
vmSubstitutions = Lens.lens (substitutions :: VoiceMessage -> Lude.Maybe (Lude.HashMap Lude.Text ([Lude.Text]))) (\s a -> s {substitutions = a} :: VoiceMessage)
{-# DEPRECATED vmSubstitutions "Use generic-lens or generic-optics with 'substitutions' instead." #-}

-- | The code for the language to use when synthesizing the text of the message script. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'languageCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmLanguageCode :: Lens.Lens' VoiceMessage (Lude.Maybe Lude.Text)
vmLanguageCode = Lens.lens (languageCode :: VoiceMessage -> Lude.Maybe Lude.Text) (\s a -> s {languageCode = a} :: VoiceMessage)
{-# DEPRECATED vmLanguageCode "Use generic-lens or generic-optics with 'languageCode' instead." #-}

-- | The long code to send the voice message from. This value should be one of the dedicated long codes that's assigned to your AWS account. Although it isn't required, we recommend that you specify the long code in E.164 format, for example +12065550100, to ensure prompt and accurate delivery of the message.
--
-- /Note:/ Consider using 'originationNumber' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmOriginationNumber :: Lens.Lens' VoiceMessage (Lude.Maybe Lude.Text)
vmOriginationNumber = Lens.lens (originationNumber :: VoiceMessage -> Lude.Maybe Lude.Text) (\s a -> s {originationNumber = a} :: VoiceMessage)
{-# DEPRECATED vmOriginationNumber "Use generic-lens or generic-optics with 'originationNumber' instead." #-}

-- | The text of the script to use for the voice message.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmBody :: Lens.Lens' VoiceMessage (Lude.Maybe Lude.Text)
vmBody = Lens.lens (body :: VoiceMessage -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: VoiceMessage)
{-# DEPRECATED vmBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The name of the voice to use when delivering the message. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- /Note:/ Consider using 'voiceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vmVoiceId :: Lens.Lens' VoiceMessage (Lude.Maybe Lude.Text)
vmVoiceId = Lens.lens (voiceId :: VoiceMessage -> Lude.Maybe Lude.Text) (\s a -> s {voiceId = a} :: VoiceMessage)
{-# DEPRECATED vmVoiceId "Use generic-lens or generic-optics with 'voiceId' instead." #-}

instance Lude.ToJSON VoiceMessage where
  toJSON VoiceMessage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Substitutions" Lude..=) Lude.<$> substitutions,
            ("LanguageCode" Lude..=) Lude.<$> languageCode,
            ("OriginationNumber" Lude..=) Lude.<$> originationNumber,
            ("Body" Lude..=) Lude.<$> body,
            ("VoiceId" Lude..=) Lude.<$> voiceId
          ]
      )
