{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.RegexMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.RegexMatchSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returned by 'ListRegexMatchSets' . Each @RegexMatchSetSummary@ object includes the @Name@ and @RegexMatchSetId@ for one 'RegexMatchSet' .
--
--
--
-- /See:/ 'regexMatchSetSummary' smart constructor.
data RegexMatchSetSummary = RegexMatchSetSummary'
  { _rmssRegexMatchSetId ::
      !Text,
    _rmssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmssRegexMatchSetId' - The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
--
-- * 'rmssName' - A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
regexMatchSetSummary ::
  -- | 'rmssRegexMatchSetId'
  Text ->
  -- | 'rmssName'
  Text ->
  RegexMatchSetSummary
regexMatchSetSummary pRegexMatchSetId_ pName_ =
  RegexMatchSetSummary'
    { _rmssRegexMatchSetId = pRegexMatchSetId_,
      _rmssName = pName_
    }

-- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ , update a @RegexMatchSet@ , remove a @RegexMatchSet@ from a @Rule@ , and delete a @RegexMatchSet@ from AWS WAF. @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
rmssRegexMatchSetId :: Lens' RegexMatchSetSummary Text
rmssRegexMatchSetId = lens _rmssRegexMatchSetId (\s a -> s {_rmssRegexMatchSetId = a})

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
rmssName :: Lens' RegexMatchSetSummary Text
rmssName = lens _rmssName (\s a -> s {_rmssName = a})

instance FromJSON RegexMatchSetSummary where
  parseJSON =
    withObject
      "RegexMatchSetSummary"
      ( \x ->
          RegexMatchSetSummary'
            <$> (x .: "RegexMatchSetId") <*> (x .: "Name")
      )

instance Hashable RegexMatchSetSummary

instance NFData RegexMatchSetSummary
