{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.RegexMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.RegexMatchSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.RegexMatchTuple

-- | In a 'GetRegexMatchSet' request, @RegexMatchSet@ is a complex type that contains the @RegexMatchSetId@ and @Name@ of a @RegexMatchSet@ , and the values that you specified when you updated the @RegexMatchSet@ .
--
--
-- The values are contained in a @RegexMatchTuple@ object, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @RegexMatchSet@ contains more than one @RegexMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
--
-- /See:/ 'regexMatchSet' smart constructor.
data RegexMatchSet = RegexMatchSet'
  { _rmsName :: !(Maybe Text),
    _rmsRegexMatchTuples :: !(Maybe [RegexMatchTuple]),
    _rmsRegexMatchSetId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegexMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rmsName' - A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
--
-- * 'rmsRegexMatchTuples' - Contains an array of 'RegexMatchTuple' objects. Each @RegexMatchTuple@ object contains:      * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.      * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
--
-- * 'rmsRegexMatchSetId' - The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ (see 'GetRegexMatchSet' ), update a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), insert a @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @RegexMatchSet@ from AWS WAF (see 'DeleteRegexMatchSet' ). @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
regexMatchSet ::
  RegexMatchSet
regexMatchSet =
  RegexMatchSet'
    { _rmsName = Nothing,
      _rmsRegexMatchTuples = Nothing,
      _rmsRegexMatchSetId = Nothing
    }

-- | A friendly name or description of the 'RegexMatchSet' . You can't change @Name@ after you create a @RegexMatchSet@ .
rmsName :: Lens' RegexMatchSet (Maybe Text)
rmsName = lens _rmsName (\s a -> s {_rmsName = a})

-- | Contains an array of 'RegexMatchTuple' objects. Each @RegexMatchTuple@ object contains:      * The part of a web request that you want AWS WAF to inspect, such as a query string or the value of the @User-Agent@ header.      * The identifier of the pattern (a regular expression) that you want AWS WAF to look for. For more information, see 'RegexPatternSet' .     * Whether to perform any conversions on the request, such as converting it to lowercase, before inspecting it for the specified string.
rmsRegexMatchTuples :: Lens' RegexMatchSet [RegexMatchTuple]
rmsRegexMatchTuples = lens _rmsRegexMatchTuples (\s a -> s {_rmsRegexMatchTuples = a}) . _Default . _Coerce

-- | The @RegexMatchSetId@ for a @RegexMatchSet@ . You use @RegexMatchSetId@ to get information about a @RegexMatchSet@ (see 'GetRegexMatchSet' ), update a @RegexMatchSet@ (see 'UpdateRegexMatchSet' ), insert a @RegexMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @RegexMatchSet@ from AWS WAF (see 'DeleteRegexMatchSet' ). @RegexMatchSetId@ is returned by 'CreateRegexMatchSet' and by 'ListRegexMatchSets' .
rmsRegexMatchSetId :: Lens' RegexMatchSet (Maybe Text)
rmsRegexMatchSetId = lens _rmsRegexMatchSetId (\s a -> s {_rmsRegexMatchSetId = a})

instance FromJSON RegexMatchSet where
  parseJSON =
    withObject
      "RegexMatchSet"
      ( \x ->
          RegexMatchSet'
            <$> (x .:? "Name")
            <*> (x .:? "RegexMatchTuples" .!= mempty)
            <*> (x .:? "RegexMatchSetId")
      )

instance Hashable RegexMatchSet

instance NFData RegexMatchSet
