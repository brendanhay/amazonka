{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Pinpoint.Types.VoiceMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.VoiceMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the settings for a one-time voice message that's sent directly to an endpoint through the voice channel.
--
--
--
-- /See:/ 'voiceMessage' smart constructor.
data VoiceMessage = VoiceMessage'
  { _vmSubstitutions ::
      !(Maybe (Map Text ([Text]))),
    _vmLanguageCode :: !(Maybe Text),
    _vmOriginationNumber :: !(Maybe Text),
    _vmBody :: !(Maybe Text),
    _vmVoiceId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VoiceMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vmSubstitutions' - The default message variables to use in the voice message. You can override the default variables with individual address variables.
--
-- * 'vmLanguageCode' - The code for the language to use when synthesizing the text of the message script. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
--
-- * 'vmOriginationNumber' - The long code to send the voice message from. This value should be one of the dedicated long codes that's assigned to your AWS account. Although it isn't required, we recommend that you specify the long code in E.164 format, for example +12065550100, to ensure prompt and accurate delivery of the message.
--
-- * 'vmBody' - The text of the script to use for the voice message.
--
-- * 'vmVoiceId' - The name of the voice to use when delivering the message. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
voiceMessage ::
  VoiceMessage
voiceMessage =
  VoiceMessage'
    { _vmSubstitutions = Nothing,
      _vmLanguageCode = Nothing,
      _vmOriginationNumber = Nothing,
      _vmBody = Nothing,
      _vmVoiceId = Nothing
    }

-- | The default message variables to use in the voice message. You can override the default variables with individual address variables.
vmSubstitutions :: Lens' VoiceMessage (HashMap Text ([Text]))
vmSubstitutions = lens _vmSubstitutions (\s a -> s {_vmSubstitutions = a}) . _Default . _Map

-- | The code for the language to use when synthesizing the text of the message script. For a list of supported languages and the code for each one, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
vmLanguageCode :: Lens' VoiceMessage (Maybe Text)
vmLanguageCode = lens _vmLanguageCode (\s a -> s {_vmLanguageCode = a})

-- | The long code to send the voice message from. This value should be one of the dedicated long codes that's assigned to your AWS account. Although it isn't required, we recommend that you specify the long code in E.164 format, for example +12065550100, to ensure prompt and accurate delivery of the message.
vmOriginationNumber :: Lens' VoiceMessage (Maybe Text)
vmOriginationNumber = lens _vmOriginationNumber (\s a -> s {_vmOriginationNumber = a})

-- | The text of the script to use for the voice message.
vmBody :: Lens' VoiceMessage (Maybe Text)
vmBody = lens _vmBody (\s a -> s {_vmBody = a})

-- | The name of the voice to use when delivering the message. For a list of supported voices, see the <https://docs.aws.amazon.com/polly/latest/dg/what-is.html Amazon Polly Developer Guide> .
vmVoiceId :: Lens' VoiceMessage (Maybe Text)
vmVoiceId = lens _vmVoiceId (\s a -> s {_vmVoiceId = a})

instance Hashable VoiceMessage

instance NFData VoiceMessage

instance ToJSON VoiceMessage where
  toJSON VoiceMessage' {..} =
    object
      ( catMaybes
          [ ("Substitutions" .=) <$> _vmSubstitutions,
            ("LanguageCode" .=) <$> _vmLanguageCode,
            ("OriginationNumber" .=) <$> _vmOriginationNumber,
            ("Body" .=) <$> _vmBody,
            ("VoiceId" .=) <$> _vmVoiceId
          ]
      )
