{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XSSMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XSSMatchSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.XSSMatchTuple

-- | A complex type that contains @XssMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect for cross-site scripting attacks and, if you want AWS WAF to inspect a header, the name of the header. If a @XssMatchSet@ contains more than one @XssMatchTuple@ object, a request needs to include cross-site scripting attacks in only one of the specified parts of the request to be considered a match.
--
--
--
-- /See:/ 'xssMatchSet' smart constructor.
data XSSMatchSet = XSSMatchSet'
  { _xmsName :: !(Maybe Text),
    _xmsXSSMatchSetId :: !Text,
    _xmsXSSMatchTuples :: ![XSSMatchTuple]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'XSSMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmsName' - The name, if any, of the @XssMatchSet@ .
--
-- * 'xmsXSSMatchSetId' - A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- * 'xmsXSSMatchTuples' - Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
xssMatchSet ::
  -- | 'xmsXSSMatchSetId'
  Text ->
  XSSMatchSet
xssMatchSet pXSSMatchSetId_ =
  XSSMatchSet'
    { _xmsName = Nothing,
      _xmsXSSMatchSetId = pXSSMatchSetId_,
      _xmsXSSMatchTuples = mempty
    }

-- | The name, if any, of the @XssMatchSet@ .
xmsName :: Lens' XSSMatchSet (Maybe Text)
xmsName = lens _xmsName (\s a -> s {_xmsName = a})

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about an @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
xmsXSSMatchSetId :: Lens' XSSMatchSet Text
xmsXSSMatchSetId = lens _xmsXSSMatchSetId (\s a -> s {_xmsXSSMatchSetId = a})

-- | Specifies the parts of web requests that you want to inspect for cross-site scripting attacks.
xmsXSSMatchTuples :: Lens' XSSMatchSet [XSSMatchTuple]
xmsXSSMatchTuples = lens _xmsXSSMatchTuples (\s a -> s {_xmsXSSMatchTuples = a}) . _Coerce

instance FromJSON XSSMatchSet where
  parseJSON =
    withObject
      "XSSMatchSet"
      ( \x ->
          XSSMatchSet'
            <$> (x .:? "Name")
            <*> (x .: "XssMatchSetId")
            <*> (x .:? "XssMatchTuples" .!= mempty)
      )

instance Hashable XSSMatchSet

instance NFData XSSMatchSet
