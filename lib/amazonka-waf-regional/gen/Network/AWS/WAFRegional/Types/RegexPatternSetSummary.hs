{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexPatternSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexPatternSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returned by 'ListRegexPatternSets' . Each @RegexPatternSetSummary@ object includes the @Name@ and @RegexPatternSetId@ for one 'RegexPatternSet' .
--
--
--
-- /See:/ 'regexPatternSetSummary' smart constructor.
data RegexPatternSetSummary = RegexPatternSetSummary'
  { _rpssRegexPatternSetId ::
      !Text,
    _rpssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexPatternSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rpssRegexPatternSetId' - The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
--
-- * 'rpssName' - A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
regexPatternSetSummary ::
  -- | 'rpssRegexPatternSetId'
  Text ->
  -- | 'rpssName'
  Text ->
  RegexPatternSetSummary
regexPatternSetSummary pRegexPatternSetId_ pName_ =
  RegexPatternSetSummary'
    { _rpssRegexPatternSetId =
        pRegexPatternSetId_,
      _rpssName = pName_
    }

-- | The @RegexPatternSetId@ for a @RegexPatternSet@ . You use @RegexPatternSetId@ to get information about a @RegexPatternSet@ , update a @RegexPatternSet@ , remove a @RegexPatternSet@ from a @RegexMatchSet@ , and delete a @RegexPatternSet@ from AWS WAF. @RegexPatternSetId@ is returned by 'CreateRegexPatternSet' and by 'ListRegexPatternSets' .
rpssRegexPatternSetId :: Lens' RegexPatternSetSummary Text
rpssRegexPatternSetId = lens _rpssRegexPatternSetId (\s a -> s {_rpssRegexPatternSetId = a})

-- | A friendly name or description of the 'RegexPatternSet' . You can't change @Name@ after you create a @RegexPatternSet@ .
rpssName :: Lens' RegexPatternSetSummary Text
rpssName = lens _rpssName (\s a -> s {_rpssName = a})

instance FromJSON RegexPatternSetSummary where
  parseJSON =
    withObject
      "RegexPatternSetSummary"
      ( \x ->
          RegexPatternSetSummary'
            <$> (x .: "RegexPatternSetId") <*> (x .: "Name")
      )

instance Hashable RegexPatternSetSummary

instance NFData RegexPatternSetSummary
