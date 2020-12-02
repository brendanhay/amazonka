{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.SynthesizeSpeech
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Synthesizes UTF-8 input, plain text or SSML, to a stream of bytes. SSML input must be valid, well-formed SSML. Some alphabets might not be available with all the voices (for example, Cyrillic might not be read at all by English voices) unless phoneme mapping is used. For more information, see <http://docs.aws.amazon.com/polly/latest/dg/how-text-to-speech-works.html How it Works> .
--
--
module Network.AWS.Polly.SynthesizeSpeech
    (
    -- * Creating a Request
      synthesizeSpeech
    , SynthesizeSpeech
    -- * Request Lenses
    , ssSpeechMarkTypes
    , ssSampleRate
    , ssTextType
    , ssLexiconNames
    , ssOutputFormat
    , ssText
    , ssVoiceId

    -- * Destructuring the Response
    , synthesizeSpeechResponse
    , SynthesizeSpeechResponse
    -- * Response Lenses
    , ssrsRequestCharacters
    , ssrsContentType
    , ssrsResponseStatus
    , ssrsAudioStream
    ) where

import Network.AWS.Lens
import Network.AWS.Polly.Types
import Network.AWS.Polly.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'synthesizeSpeech' smart constructor.
data SynthesizeSpeech = SynthesizeSpeech'
  { _ssSpeechMarkTypes :: !(Maybe [SpeechMarkType])
  , _ssSampleRate      :: !(Maybe Text)
  , _ssTextType        :: !(Maybe TextType)
  , _ssLexiconNames    :: !(Maybe [Sensitive Text])
  , _ssOutputFormat    :: !OutputFormat
  , _ssText            :: !Text
  , _ssVoiceId         :: !VoiceId
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SynthesizeSpeech' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssSpeechMarkTypes' - The type of speech marks returned for the input text.
--
-- * 'ssSampleRate' - The audio frequency specified in Hz.  The valid values for @mp3@ and @ogg_vorbis@ are "8000", "16000", and "22050". The default value is "22050".  Valid values for @pcm@ are "8000" and "16000" The default value is "16000".
--
-- * 'ssTextType' - Specifies whether the input text is plain text or SSML. The default value is plain text. For more information, see <http://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML> .
--
-- * 'ssLexiconNames' - List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. For information about storing lexicons, see <http://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon> .
--
-- * 'ssOutputFormat' - The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
--
-- * 'ssText' - Input text to synthesize. If you specify @ssml@ as the @TextType@ , follow the SSML format for the input text.
--
-- * 'ssVoiceId' - Voice ID to use for the synthesis. You can get a list of available voice IDs by calling the <http://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation.
synthesizeSpeech
    :: OutputFormat -- ^ 'ssOutputFormat'
    -> Text -- ^ 'ssText'
    -> VoiceId -- ^ 'ssVoiceId'
    -> SynthesizeSpeech
synthesizeSpeech pOutputFormat_ pText_ pVoiceId_ =
  SynthesizeSpeech'
    { _ssSpeechMarkTypes = Nothing
    , _ssSampleRate = Nothing
    , _ssTextType = Nothing
    , _ssLexiconNames = Nothing
    , _ssOutputFormat = pOutputFormat_
    , _ssText = pText_
    , _ssVoiceId = pVoiceId_
    }


-- | The type of speech marks returned for the input text.
ssSpeechMarkTypes :: Lens' SynthesizeSpeech [SpeechMarkType]
ssSpeechMarkTypes = lens _ssSpeechMarkTypes (\ s a -> s{_ssSpeechMarkTypes = a}) . _Default . _Coerce

-- | The audio frequency specified in Hz.  The valid values for @mp3@ and @ogg_vorbis@ are "8000", "16000", and "22050". The default value is "22050".  Valid values for @pcm@ are "8000" and "16000" The default value is "16000".
ssSampleRate :: Lens' SynthesizeSpeech (Maybe Text)
ssSampleRate = lens _ssSampleRate (\ s a -> s{_ssSampleRate = a})

-- | Specifies whether the input text is plain text or SSML. The default value is plain text. For more information, see <http://docs.aws.amazon.com/polly/latest/dg/ssml.html Using SSML> .
ssTextType :: Lens' SynthesizeSpeech (Maybe TextType)
ssTextType = lens _ssTextType (\ s a -> s{_ssTextType = a})

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice. For information about storing lexicons, see <http://docs.aws.amazon.com/polly/latest/dg/API_PutLexicon.html PutLexicon> .
ssLexiconNames :: Lens' SynthesizeSpeech [Text]
ssLexiconNames = lens _ssLexiconNames (\ s a -> s{_ssLexiconNames = a}) . _Default . _Coerce

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
ssOutputFormat :: Lens' SynthesizeSpeech OutputFormat
ssOutputFormat = lens _ssOutputFormat (\ s a -> s{_ssOutputFormat = a})

-- | Input text to synthesize. If you specify @ssml@ as the @TextType@ , follow the SSML format for the input text.
ssText :: Lens' SynthesizeSpeech Text
ssText = lens _ssText (\ s a -> s{_ssText = a})

-- | Voice ID to use for the synthesis. You can get a list of available voice IDs by calling the <http://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation.
ssVoiceId :: Lens' SynthesizeSpeech VoiceId
ssVoiceId = lens _ssVoiceId (\ s a -> s{_ssVoiceId = a})

instance AWSRequest SynthesizeSpeech where
        type Rs SynthesizeSpeech = SynthesizeSpeechResponse
        request = postJSON polly
        response
          = receiveBody
              (\ s h x ->
                 SynthesizeSpeechResponse' <$>
                   (h .#? "x-amzn-RequestCharacters") <*>
                     (h .#? "Content-Type")
                     <*> (pure (fromEnum s))
                     <*> (pure x))

instance Hashable SynthesizeSpeech where

instance NFData SynthesizeSpeech where

instance ToHeaders SynthesizeSpeech where
        toHeaders = const mempty

instance ToJSON SynthesizeSpeech where
        toJSON SynthesizeSpeech'{..}
          = object
              (catMaybes
                 [("SpeechMarkTypes" .=) <$> _ssSpeechMarkTypes,
                  ("SampleRate" .=) <$> _ssSampleRate,
                  ("TextType" .=) <$> _ssTextType,
                  ("LexiconNames" .=) <$> _ssLexiconNames,
                  Just ("OutputFormat" .= _ssOutputFormat),
                  Just ("Text" .= _ssText),
                  Just ("VoiceId" .= _ssVoiceId)])

instance ToPath SynthesizeSpeech where
        toPath = const "/v1/speech"

instance ToQuery SynthesizeSpeech where
        toQuery = const mempty

-- | /See:/ 'synthesizeSpeechResponse' smart constructor.
data SynthesizeSpeechResponse = SynthesizeSpeechResponse'
  { _ssrsRequestCharacters :: !(Maybe Int)
  , _ssrsContentType       :: !(Maybe Text)
  , _ssrsResponseStatus    :: !Int
  , _ssrsAudioStream       :: !RsBody
  } deriving (Show, Generic)


-- | Creates a value of 'SynthesizeSpeechResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ssrsRequestCharacters' - Number of characters synthesized.
--
-- * 'ssrsContentType' - Specifies the type audio stream. This should reflect the @OutputFormat@ parameter in your request.      * If you request @mp3@ as the @OutputFormat@ , the @ContentType@ returned is audio/mpeg.      * If you request @ogg_vorbis@ as the @OutputFormat@ , the @ContentType@ returned is audio/ogg.      * If you request @pcm@ as the @OutputFormat@ , the @ContentType@ returned is audio/pcm in a signed 16-bit, 1 channel (mono), little-endian format.      * If you request @json@ as the @OutputFormat@ , the @ContentType@ returned is audio/json.
--
-- * 'ssrsResponseStatus' - -- | The response status code.
--
-- * 'ssrsAudioStream' - Stream containing the synthesized speech.
synthesizeSpeechResponse
    :: Int -- ^ 'ssrsResponseStatus'
    -> RsBody -- ^ 'ssrsAudioStream'
    -> SynthesizeSpeechResponse
synthesizeSpeechResponse pResponseStatus_ pAudioStream_ =
  SynthesizeSpeechResponse'
    { _ssrsRequestCharacters = Nothing
    , _ssrsContentType = Nothing
    , _ssrsResponseStatus = pResponseStatus_
    , _ssrsAudioStream = pAudioStream_
    }


-- | Number of characters synthesized.
ssrsRequestCharacters :: Lens' SynthesizeSpeechResponse (Maybe Int)
ssrsRequestCharacters = lens _ssrsRequestCharacters (\ s a -> s{_ssrsRequestCharacters = a})

-- | Specifies the type audio stream. This should reflect the @OutputFormat@ parameter in your request.      * If you request @mp3@ as the @OutputFormat@ , the @ContentType@ returned is audio/mpeg.      * If you request @ogg_vorbis@ as the @OutputFormat@ , the @ContentType@ returned is audio/ogg.      * If you request @pcm@ as the @OutputFormat@ , the @ContentType@ returned is audio/pcm in a signed 16-bit, 1 channel (mono), little-endian format.      * If you request @json@ as the @OutputFormat@ , the @ContentType@ returned is audio/json.
ssrsContentType :: Lens' SynthesizeSpeechResponse (Maybe Text)
ssrsContentType = lens _ssrsContentType (\ s a -> s{_ssrsContentType = a})

-- | -- | The response status code.
ssrsResponseStatus :: Lens' SynthesizeSpeechResponse Int
ssrsResponseStatus = lens _ssrsResponseStatus (\ s a -> s{_ssrsResponseStatus = a})

-- | Stream containing the synthesized speech.
ssrsAudioStream :: Lens' SynthesizeSpeechResponse RsBody
ssrsAudioStream = lens _ssrsAudioStream (\ s a -> s{_ssrsAudioStream = a})
