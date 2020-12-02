{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Polly.Types.LexiconAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Polly.Types.LexiconAttributes where

import Network.AWS.Lens
import Network.AWS.Polly.Types.LanguageCode
import Network.AWS.Prelude

-- | Contains metadata describing the lexicon such as the number of lexemes, language code, and so on. For more information, see <https://docs.aws.amazon.com/polly/latest/dg/managing-lexicons.html Managing Lexicons> .
--
--
--
-- /See:/ 'lexiconAttributes' smart constructor.
data LexiconAttributes = LexiconAttributes'
  { _laLanguageCode ::
      !(Maybe LanguageCode),
    _laSize :: !(Maybe Int),
    _laLexemesCount :: !(Maybe Int),
    _laLexiconARN :: !(Maybe Text),
    _laAlphabet :: !(Maybe Text),
    _laLastModified :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

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
lexiconAttributes ::
  LexiconAttributes
lexiconAttributes =
  LexiconAttributes'
    { _laLanguageCode = Nothing,
      _laSize = Nothing,
      _laLexemesCount = Nothing,
      _laLexiconARN = Nothing,
      _laAlphabet = Nothing,
      _laLastModified = Nothing
    }

-- | Language code that the lexicon applies to. A lexicon with a language code such as "en" would be applied to all English languages (en-GB, en-US, en-AUS, en-WLS, and so on.
laLanguageCode :: Lens' LexiconAttributes (Maybe LanguageCode)
laLanguageCode = lens _laLanguageCode (\s a -> s {_laLanguageCode = a})

-- | Total size of the lexicon, in characters.
laSize :: Lens' LexiconAttributes (Maybe Int)
laSize = lens _laSize (\s a -> s {_laSize = a})

-- | Number of lexemes in the lexicon.
laLexemesCount :: Lens' LexiconAttributes (Maybe Int)
laLexemesCount = lens _laLexemesCount (\s a -> s {_laLexemesCount = a})

-- | Amazon Resource Name (ARN) of the lexicon.
laLexiconARN :: Lens' LexiconAttributes (Maybe Text)
laLexiconARN = lens _laLexiconARN (\s a -> s {_laLexiconARN = a})

-- | Phonetic alphabet used in the lexicon. Valid values are @ipa@ and @x-sampa@ .
laAlphabet :: Lens' LexiconAttributes (Maybe Text)
laAlphabet = lens _laAlphabet (\s a -> s {_laAlphabet = a})

-- | Date lexicon was last modified (a timestamp value).
laLastModified :: Lens' LexiconAttributes (Maybe UTCTime)
laLastModified = lens _laLastModified (\s a -> s {_laLastModified = a}) . mapping _Time

instance FromJSON LexiconAttributes where
  parseJSON =
    withObject
      "LexiconAttributes"
      ( \x ->
          LexiconAttributes'
            <$> (x .:? "LanguageCode")
            <*> (x .:? "Size")
            <*> (x .:? "LexemesCount")
            <*> (x .:? "LexiconArn")
            <*> (x .:? "Alphabet")
            <*> (x .:? "LastModified")
      )

instance Hashable LexiconAttributes

instance NFData LexiconAttributes
