{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.ByteMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.ByteMatchSetSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Returned by 'ListByteMatchSets' . Each @ByteMatchSetSummary@ object includes the @Name@ and @ByteMatchSetId@ for one 'ByteMatchSet' .
--
--
--
-- /See:/ 'byteMatchSetSummary' smart constructor.
data ByteMatchSetSummary = ByteMatchSetSummary'
  { _bmssByteMatchSetId ::
      !Text,
    _bmssName :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ByteMatchSetSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmssByteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ , update a @ByteMatchSet@ , remove a @ByteMatchSet@ from a @Rule@ , and delete a @ByteMatchSet@ from AWS WAF. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- * 'bmssName' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
byteMatchSetSummary ::
  -- | 'bmssByteMatchSetId'
  Text ->
  -- | 'bmssName'
  Text ->
  ByteMatchSetSummary
byteMatchSetSummary pByteMatchSetId_ pName_ =
  ByteMatchSetSummary'
    { _bmssByteMatchSetId = pByteMatchSetId_,
      _bmssName = pName_
    }

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ , update a @ByteMatchSet@ , remove a @ByteMatchSet@ from a @Rule@ , and delete a @ByteMatchSet@ from AWS WAF. @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
bmssByteMatchSetId :: Lens' ByteMatchSetSummary Text
bmssByteMatchSetId = lens _bmssByteMatchSetId (\s a -> s {_bmssByteMatchSetId = a})

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
bmssName :: Lens' ByteMatchSetSummary Text
bmssName = lens _bmssName (\s a -> s {_bmssName = a})

instance FromJSON ByteMatchSetSummary where
  parseJSON =
    withObject
      "ByteMatchSetSummary"
      ( \x ->
          ByteMatchSetSummary' <$> (x .: "ByteMatchSetId") <*> (x .: "Name")
      )

instance Hashable ByteMatchSetSummary

instance NFData ByteMatchSetSummary
