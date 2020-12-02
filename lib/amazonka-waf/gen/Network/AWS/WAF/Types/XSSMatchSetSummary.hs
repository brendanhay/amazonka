{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.XSSMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.XSSMatchSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @Id@ and @Name@ of an @XssMatchSet@ .
--
--
--
-- /See:/ 'xssMatchSetSummary' smart constructor.
data XSSMatchSetSummary = XSSMatchSetSummary'
  { _xmssXSSMatchSetId ::
      !Text,
    _xmssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'XSSMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'xmssXSSMatchSetId' - A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
--
-- * 'xmssName' - The name of the @XssMatchSet@ , if any, specified by @Id@ .
xssMatchSetSummary ::
  -- | 'xmssXSSMatchSetId'
  Text ->
  -- | 'xmssName'
  Text ->
  XSSMatchSetSummary
xssMatchSetSummary pXSSMatchSetId_ pName_ =
  XSSMatchSetSummary'
    { _xmssXSSMatchSetId = pXSSMatchSetId_,
      _xmssName = pName_
    }

-- | A unique identifier for an @XssMatchSet@ . You use @XssMatchSetId@ to get information about a @XssMatchSet@ (see 'GetXssMatchSet' ), update an @XssMatchSet@ (see 'UpdateXssMatchSet' ), insert an @XssMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete an @XssMatchSet@ from AWS WAF (see 'DeleteXssMatchSet' ). @XssMatchSetId@ is returned by 'CreateXssMatchSet' and by 'ListXssMatchSets' .
xmssXSSMatchSetId :: Lens' XSSMatchSetSummary Text
xmssXSSMatchSetId = lens _xmssXSSMatchSetId (\s a -> s {_xmssXSSMatchSetId = a})

-- | The name of the @XssMatchSet@ , if any, specified by @Id@ .
xmssName :: Lens' XSSMatchSetSummary Text
xmssName = lens _xmssName (\s a -> s {_xmssName = a})

instance FromJSON XSSMatchSetSummary where
  parseJSON =
    withObject
      "XSSMatchSetSummary"
      ( \x ->
          XSSMatchSetSummary' <$> (x .: "XssMatchSetId") <*> (x .: "Name")
      )

instance Hashable XSSMatchSetSummary

instance NFData XSSMatchSetSummary
