{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.Product
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Polly.Types.Product where

import Network.AWS.Lens
import Network.AWS.Polly.Types.Sum
import Network.AWS.Prelude

-- | Provides lexicon name and lexicon content in string format. For more information, see <https://www.w3.org/TR/pronunciation-lexicon/ Pronunciation Lexicon Specification (PLS) Version 1.0> .
--
--
--
-- /See:/ 'lexicon' smart constructor.
data Lexicon = Lexicon'
  { _lContent :: !(Maybe Text)
  , _lName    :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'Lexicon' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lContent' - Lexicon content in string format. The content of a lexicon must be in PLS format.
--
-- * 'lName' - Name of the lexicon.
lexicon
    :: Lexicon
lexicon = Lexicon' {_lContent = Nothing, _lName = Nothing}


-- | Lexicon content in string format. The content of a lexicon must be in PLS format.
lContent :: Lens' Lexicon (Maybe Text)
lContent = lens _lContent (\ s a -> s{_lContent = a})

-- | Name of the lexicon.
lName :: Lens' Lexicon (Maybe Text)
lName = lens _lName (\ s a -> s{_lName = a}) . mapping _Sensitive

instance FromJSON Lexicon where
        parseJSON
          = withObject "Lexicon"
              (\ x ->
                 Lexicon' <$> (x .:? "Content") <*> (x .:? "Name"))

instance Hashable Lexicon where

instance NFData Lexicon where

-- | Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <http://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
--
--
-- /See:/ 'lexiconAttributes' smart constructor.
data LexiconAttributes = LexiconAttributes'
  { _laLanguageCode :: !(Maybe LanguageCode)
  , _laSize         :: !(Maybe Int)
  , _laLexemesCount :: !(Maybe Int)
  , _laLexiconARN   :: !(Maybe Text)
  , _laAlphabet     :: !(Maybe Text)
  , _laLastModified :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'LexiconAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laLanguageCode' - Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
--
-- * 'laSize' - Total size of the lexicon, in characters.
--
-- * 'laLexemesCount' - Number of lexemes in the lexicon.
--
-- * 'laLexiconARN' - Amazon Resource Name (ARN) of the lexicon.
--
-- * 'laAlphabet' - Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
--
-- * 'laLastModified' - Date lexicon was last modified (a timestamp value).
lexiconAttributes
    :: LexiconAttributes
lexiconAttributes =
  LexiconAttributes'
    { _laLanguageCode = Nothing
    , _laSize = Nothing
    , _laLexemesCount = Nothing
    , _laLexiconARN = Nothing
    , _laAlphabet = Nothing
    , _laLastModified = Nothing
    }


-- | Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
laLanguageCode :: Lens' LexiconAttributes (Maybe LanguageCode)
laLanguageCode = lens _laLanguageCode (\ s a -> s{_laLanguageCode = a})

-- | Total size of the lexicon, in characters.
laSize :: Lens' LexiconAttributes (Maybe Int)
laSize = lens _laSize (\ s a -> s{_laSize = a})

-- | Number of lexemes in the lexicon.
laLexemesCount :: Lens' LexiconAttributes (Maybe Int)
laLexemesCount = lens _laLexemesCount (\ s a -> s{_laLexemesCount = a})

-- | Amazon Resource Name (ARN) of the lexicon.
laLexiconARN :: Lens' LexiconAttributes (Maybe Text)
laLexiconARN = lens _laLexiconARN (\ s a -> s{_laLexiconARN = a})

-- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
laAlphabet :: Lens' LexiconAttributes (Maybe Text)
laAlphabet = lens _laAlphabet (\ s a -> s{_laAlphabet = a})

-- | Date lexicon was last modified (a timestamp value).
laLastModified :: Lens' LexiconAttributes (Maybe UTCTime)
laLastModified = lens _laLastModified (\ s a -> s{_laLastModified = a}) . mapping _Time

instance FromJSON LexiconAttributes where
        parseJSON
          = withObject "LexiconAttributes"
              (\ x ->
                 LexiconAttributes' <$>
                   (x .:? "LanguageCode") <*> (x .:? "Size") <*>
                     (x .:? "LexemesCount")
                     <*> (x .:? "LexiconArn")
                     <*> (x .:? "Alphabet")
                     <*> (x .:? "LastModified"))

instance Hashable LexiconAttributes where

instance NFData LexiconAttributes where

-- | Describes the content of the lexicon.
--
--
--
-- /See:/ 'lexiconDescription' smart constructor.
data LexiconDescription = LexiconDescription'
  { _ldAttributes :: !(Maybe LexiconAttributes)
  , _ldName       :: !(Maybe (Sensitive Text))
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'LexiconDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldAttributes' - Provides lexicon metadata.
--
-- * 'ldName' - Name of the lexicon.
lexiconDescription
    :: LexiconDescription
lexiconDescription =
  LexiconDescription' {_ldAttributes = Nothing, _ldName = Nothing}


-- | Provides lexicon metadata.
ldAttributes :: Lens' LexiconDescription (Maybe LexiconAttributes)
ldAttributes = lens _ldAttributes (\ s a -> s{_ldAttributes = a})

-- | Name of the lexicon.
ldName :: Lens' LexiconDescription (Maybe Text)
ldName = lens _ldName (\ s a -> s{_ldName = a}) . mapping _Sensitive

instance FromJSON LexiconDescription where
        parseJSON
          = withObject "LexiconDescription"
              (\ x ->
                 LexiconDescription' <$>
                   (x .:? "Attributes") <*> (x .:? "Name"))

instance Hashable LexiconDescription where

instance NFData LexiconDescription where

-- | SynthesisTask object that provides information about a speech synthesis task.
--
--
--
-- /See:/ 'synthesisTask' smart constructor.
data SynthesisTask = SynthesisTask'
  { _stCreationTime      :: !(Maybe POSIX)
  , _stLanguageCode      :: !(Maybe LanguageCode)
  , _stSNSTopicARN       :: !(Maybe Text)
  , _stTaskStatusReason  :: !(Maybe Text)
  , _stTaskId            :: !(Maybe Text)
  , _stRequestCharacters :: !(Maybe Int)
  , _stSpeechMarkTypes   :: !(Maybe [SpeechMarkType])
  , _stSampleRate        :: !(Maybe Text)
  , _stOutputFormat      :: !(Maybe OutputFormat)
  , _stTextType          :: !(Maybe TextType)
  , _stVoiceId           :: !(Maybe VoiceId)
  , _stLexiconNames      :: !(Maybe [Sensitive Text])
  , _stTaskStatus        :: !(Maybe TaskStatus)
  , _stOutputURI         :: !(Maybe Text)
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'SynthesisTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'stCreationTime' - Timestamp for the time the synthesis task was started.
--
-- * 'stLanguageCode' - Optional language code for a synthesis task. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).  If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
--
-- * 'stSNSTopicARN' - ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
--
-- * 'stTaskStatusReason' - Reason for the current status of a specific speech synthesis task, including errors if the task has failed.
--
-- * 'stTaskId' - The Amazon Polly generated identifier for a speech synthesis task.
--
-- * 'stRequestCharacters' - Number of billable characters synthesized.
--
-- * 'stSpeechMarkTypes' - The type of speech marks returned for the input text.
--
-- * 'stSampleRate' - The audio frequency specified in Hz. The valid values for mp3 and ogg_vorbis are "8000", "16000", and "22050". The default value is "22050". Valid values for pcm are "8000" and "16000" The default value is "16000".
--
-- * 'stOutputFormat' - The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
--
-- * 'stTextType' - Specifies whether the input text is plain text or SSML. The default value is plain text.
--
-- * 'stVoiceId' - Voice ID to use for the synthesis.
--
-- * 'stLexiconNames' - List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice.
--
-- * 'stTaskStatus' - Current status of the individual speech synthesis task.
--
-- * 'stOutputURI' - Pathway for the output speech file.
synthesisTask
    :: SynthesisTask
synthesisTask =
  SynthesisTask'
    { _stCreationTime = Nothing
    , _stLanguageCode = Nothing
    , _stSNSTopicARN = Nothing
    , _stTaskStatusReason = Nothing
    , _stTaskId = Nothing
    , _stRequestCharacters = Nothing
    , _stSpeechMarkTypes = Nothing
    , _stSampleRate = Nothing
    , _stOutputFormat = Nothing
    , _stTextType = Nothing
    , _stVoiceId = Nothing
    , _stLexiconNames = Nothing
    , _stTaskStatus = Nothing
    , _stOutputURI = Nothing
    }


-- | Timestamp for the time the synthesis task was started.
stCreationTime :: Lens' SynthesisTask (Maybe UTCTime)
stCreationTime = lens _stCreationTime (\ s a -> s{_stCreationTime = a}) . mapping _Time

-- | Optional language code for a synthesis task. This is only necessary if using a bilingual voice, such as Aditi, which can be used for either Indian English (en-IN) or Hindi (hi-IN).  If a bilingual voice is used and no language code is specified, Amazon Polly will use the default language of the bilingual voice. The default language for any voice is the one returned by the <https://docs.aws.amazon.com/polly/latest/dg/API_DescribeVoices.html DescribeVoices> operation for the @LanguageCode@ parameter. For example, if no language code is specified, Aditi will use Indian English rather than Hindi.
stLanguageCode :: Lens' SynthesisTask (Maybe LanguageCode)
stLanguageCode = lens _stLanguageCode (\ s a -> s{_stLanguageCode = a})

-- | ARN for the SNS topic optionally used for providing status notification for a speech synthesis task.
stSNSTopicARN :: Lens' SynthesisTask (Maybe Text)
stSNSTopicARN = lens _stSNSTopicARN (\ s a -> s{_stSNSTopicARN = a})

-- | Reason for the current status of a specific speech synthesis task, including errors if the task has failed.
stTaskStatusReason :: Lens' SynthesisTask (Maybe Text)
stTaskStatusReason = lens _stTaskStatusReason (\ s a -> s{_stTaskStatusReason = a})

-- | The Amazon Polly generated identifier for a speech synthesis task.
stTaskId :: Lens' SynthesisTask (Maybe Text)
stTaskId = lens _stTaskId (\ s a -> s{_stTaskId = a})

-- | Number of billable characters synthesized.
stRequestCharacters :: Lens' SynthesisTask (Maybe Int)
stRequestCharacters = lens _stRequestCharacters (\ s a -> s{_stRequestCharacters = a})

-- | The type of speech marks returned for the input text.
stSpeechMarkTypes :: Lens' SynthesisTask [SpeechMarkType]
stSpeechMarkTypes = lens _stSpeechMarkTypes (\ s a -> s{_stSpeechMarkTypes = a}) . _Default . _Coerce

-- | The audio frequency specified in Hz. The valid values for mp3 and ogg_vorbis are "8000", "16000", and "22050". The default value is "22050". Valid values for pcm are "8000" and "16000" The default value is "16000".
stSampleRate :: Lens' SynthesisTask (Maybe Text)
stSampleRate = lens _stSampleRate (\ s a -> s{_stSampleRate = a})

-- | The format in which the returned output will be encoded. For audio stream, this will be mp3, ogg_vorbis, or pcm. For speech marks, this will be json.
stOutputFormat :: Lens' SynthesisTask (Maybe OutputFormat)
stOutputFormat = lens _stOutputFormat (\ s a -> s{_stOutputFormat = a})

-- | Specifies whether the input text is plain text or SSML. The default value is plain text.
stTextType :: Lens' SynthesisTask (Maybe TextType)
stTextType = lens _stTextType (\ s a -> s{_stTextType = a})

-- | Voice ID to use for the synthesis.
stVoiceId :: Lens' SynthesisTask (Maybe VoiceId)
stVoiceId = lens _stVoiceId (\ s a -> s{_stVoiceId = a})

-- | List of one or more pronunciation lexicon names you want the service to apply during synthesis. Lexicons are applied only if the language of the lexicon is the same as the language of the voice.
stLexiconNames :: Lens' SynthesisTask [Text]
stLexiconNames = lens _stLexiconNames (\ s a -> s{_stLexiconNames = a}) . _Default . _Coerce

-- | Current status of the individual speech synthesis task.
stTaskStatus :: Lens' SynthesisTask (Maybe TaskStatus)
stTaskStatus = lens _stTaskStatus (\ s a -> s{_stTaskStatus = a})

-- | Pathway for the output speech file.
stOutputURI :: Lens' SynthesisTask (Maybe Text)
stOutputURI = lens _stOutputURI (\ s a -> s{_stOutputURI = a})

instance FromJSON SynthesisTask where
        parseJSON
          = withObject "SynthesisTask"
              (\ x ->
                 SynthesisTask' <$>
                   (x .:? "CreationTime") <*> (x .:? "LanguageCode") <*>
                     (x .:? "SnsTopicArn")
                     <*> (x .:? "TaskStatusReason")
                     <*> (x .:? "TaskId")
                     <*> (x .:? "RequestCharacters")
                     <*> (x .:? "SpeechMarkTypes" .!= mempty)
                     <*> (x .:? "SampleRate")
                     <*> (x .:? "OutputFormat")
                     <*> (x .:? "TextType")
                     <*> (x .:? "VoiceId")
                     <*> (x .:? "LexiconNames" .!= mempty)
                     <*> (x .:? "TaskStatus")
                     <*> (x .:? "OutputUri"))

instance Hashable SynthesisTask where

instance NFData SynthesisTask where

-- | Description of the voice.
--
--
--
-- /See:/ 'voice' smart constructor.
data Voice = Voice'
  { _vLanguageCode            :: !(Maybe LanguageCode)
  , _vLanguageName            :: !(Maybe Text)
  , _vGender                  :: !(Maybe Gender)
  , _vName                    :: !(Maybe Text)
  , _vId                      :: !(Maybe VoiceId)
  , _vAdditionalLanguageCodes :: !(Maybe [LanguageCode])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'Voice' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vLanguageCode' - Language code of the voice.
--
-- * 'vLanguageName' - Human readable name of the language in English.
--
-- * 'vGender' - Gender of the voice.
--
-- * 'vName' - Name of the voice (for example, Salli, Kendra, etc.). This provides a human readable voice name that you might display in your application.
--
-- * 'vId' - Amazon Polly assigned voice ID. This is the ID that you specify when calling the @SynthesizeSpeech@ operation.
--
-- * 'vAdditionalLanguageCodes' - Additional codes for languages available for the specified voice in addition to its default language.  For example, the default language for Aditi is Indian English (en-IN) because it was first used for that language. Since Aditi is bilingual and fluent in both Indian English and Hindi, this parameter would show the code @hi-IN@ .
voice
    :: Voice
voice =
  Voice'
    { _vLanguageCode = Nothing
    , _vLanguageName = Nothing
    , _vGender = Nothing
    , _vName = Nothing
    , _vId = Nothing
    , _vAdditionalLanguageCodes = Nothing
    }


-- | Language code of the voice.
vLanguageCode :: Lens' Voice (Maybe LanguageCode)
vLanguageCode = lens _vLanguageCode (\ s a -> s{_vLanguageCode = a})

-- | Human readable name of the language in English.
vLanguageName :: Lens' Voice (Maybe Text)
vLanguageName = lens _vLanguageName (\ s a -> s{_vLanguageName = a})

-- | Gender of the voice.
vGender :: Lens' Voice (Maybe Gender)
vGender = lens _vGender (\ s a -> s{_vGender = a})

-- | Name of the voice (for example, Salli, Kendra, etc.). This provides a human readable voice name that you might display in your application.
vName :: Lens' Voice (Maybe Text)
vName = lens _vName (\ s a -> s{_vName = a})

-- | Amazon Polly assigned voice ID. This is the ID that you specify when calling the @SynthesizeSpeech@ operation.
vId :: Lens' Voice (Maybe VoiceId)
vId = lens _vId (\ s a -> s{_vId = a})

-- | Additional codes for languages available for the specified voice in addition to its default language.  For example, the default language for Aditi is Indian English (en-IN) because it was first used for that language. Since Aditi is bilingual and fluent in both Indian English and Hindi, this parameter would show the code @hi-IN@ .
vAdditionalLanguageCodes :: Lens' Voice [LanguageCode]
vAdditionalLanguageCodes = lens _vAdditionalLanguageCodes (\ s a -> s{_vAdditionalLanguageCodes = a}) . _Default . _Coerce

instance FromJSON Voice where
        parseJSON
          = withObject "Voice"
              (\ x ->
                 Voice' <$>
                   (x .:? "LanguageCode") <*> (x .:? "LanguageName") <*>
                     (x .:? "Gender")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id")
                     <*> (x .:? "AdditionalLanguageCodes" .!= mempty))

instance Hashable Voice where

instance NFData Voice where
