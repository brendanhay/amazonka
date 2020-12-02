{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexPatternSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexPatternSet where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @RegexPatternSet@ specifies the regular expression (regex) pattern that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ . You can then configure AWS WAF to reject those requests.
--
--
--
-- /See:/ 'regexPatternSet' smart constructor.
data RegexPatternSet = RegexPatternSet'
  { _rpsName :: !(Maybe Text),
    _rpsRegexPatternSetId :: !Text,
    _rpsRegexPatternStrings :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexPatternSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpsName' - A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
--
-- * 'rpsRegexPatternSetId' - The identifier for the @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF. @RegexMatchSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- * 'rpsRegexPatternStrings' - Specifies the regular expression (regex) patterns that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
regexPatternSet ::
  -- | 'rpsRegexPatternSetId'
  Text ->
  RegexPatternSet
regexPatternSet pRegexPatternSetId_ =
  RegexPatternSet'
    { _rpsName = Nothing,
      _rpsRegexPatternSetId = pRegexPatternSetId_,
      _rpsRegexPatternStrings = mempty
    }

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
rpsName :: Lens' RegexPatternSet (Maybe Text)
rpsName = lens _rpsName (\s a -> s {_rpsName = a})

-- | The identifier for the @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF. @RegexMatchSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
rpsRegexPatternSetId :: Lens' RegexPatternSet Text
rpsRegexPatternSetId = lens _rpsRegexPatternSetId (\s a -> s {_rpsRegexPatternSetId = a})

-- | Specifies the regular expression (regex) patterns that you want AWS WAF to search for, such as @B[a@]dB[o0]t@ .
rpsRegexPatternStrings :: Lens' RegexPatternSet [Text]
rpsRegexPatternStrings = lens _rpsRegexPatternStrings (\s a -> s {_rpsRegexPatternStrings = a}) . _Coerce

instance FromJSON RegexPatternSet where
  parseJSON =
    withObject
      "RegexPatternSet"
      ( \x ->
          RegexPatternSet'
            <$> (x .:? "Name")
            <*> (x .: "RegexPatternSetId")
            <*> (x .:? "RegexPatternStrings" .!= mempty)
      )

instance Hashable RegexPatternSet

instance NFData RegexPatternSet
