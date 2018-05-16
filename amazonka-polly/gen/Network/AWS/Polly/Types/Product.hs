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

-- | Description of the voice.
--
--
--
-- /See:/ 'voice' smart constructor.
data Voice = Voice'
  { _vLanguageCode :: !(Maybe LanguageCode)
  , _vLanguageName :: !(Maybe Text)
  , _vGender       :: !(Maybe Gender)
  , _vName         :: !(Maybe Text)
  , _vId           :: !(Maybe VoiceId)
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
voice
    :: Voice
voice =
  Voice'
    { _vLanguageCode = Nothing
    , _vLanguageName = Nothing
    , _vGender = Nothing
    , _vName = Nothing
    , _vId = Nothing
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

instance FromJSON Voice where
        parseJSON
          = withObject "Voice"
              (\ x ->
                 Voice' <$>
                   (x .:? "LanguageCode") <*> (x .:? "LanguageName") <*>
                     (x .:? "Gender")
                     <*> (x .:? "Name")
                     <*> (x .:? "Id"))

instance Hashable Voice where

instance NFData Voice where
