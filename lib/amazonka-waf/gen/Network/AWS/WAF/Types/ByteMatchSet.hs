{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.ByteMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.ByteMatchSet where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.WAF.Types.ByteMatchTuple

-- | In a 'GetByteMatchSet' request, @ByteMatchSet@ is a complex type that contains the @ByteMatchSetId@ and @Name@ of a @ByteMatchSet@ , and the values that you specified when you updated the @ByteMatchSet@ .
--
--
-- A complex type that contains @ByteMatchTuple@ objects, which specify the parts of web requests that you want AWS WAF to inspect and the values that you want AWS WAF to search for. If a @ByteMatchSet@ contains more than one @ByteMatchTuple@ object, a request needs to match the settings in only one @ByteMatchTuple@ to be considered a match.
--
--
-- /See:/ 'byteMatchSet' smart constructor.
data ByteMatchSet = ByteMatchSet'
  { _bmsName :: !(Maybe Text),
    _bmsByteMatchSetId :: !Text,
    _bmsByteMatchTuples :: ![ByteMatchTuple]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ByteMatchSet' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bmsName' - A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
--
-- * 'bmsByteMatchSetId' - The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ). @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
--
-- * 'bmsByteMatchTuples' - Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
byteMatchSet ::
  -- | 'bmsByteMatchSetId'
  Text ->
  ByteMatchSet
byteMatchSet pByteMatchSetId_ =
  ByteMatchSet'
    { _bmsName = Nothing,
      _bmsByteMatchSetId = pByteMatchSetId_,
      _bmsByteMatchTuples = mempty
    }

-- | A friendly name or description of the 'ByteMatchSet' . You can't change @Name@ after you create a @ByteMatchSet@ .
bmsName :: Lens' ByteMatchSet (Maybe Text)
bmsName = lens _bmsName (\s a -> s {_bmsName = a})

-- | The @ByteMatchSetId@ for a @ByteMatchSet@ . You use @ByteMatchSetId@ to get information about a @ByteMatchSet@ (see 'GetByteMatchSet' ), update a @ByteMatchSet@ (see 'UpdateByteMatchSet' ), insert a @ByteMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @ByteMatchSet@ from AWS WAF (see 'DeleteByteMatchSet' ). @ByteMatchSetId@ is returned by 'CreateByteMatchSet' and by 'ListByteMatchSets' .
bmsByteMatchSetId :: Lens' ByteMatchSet Text
bmsByteMatchSetId = lens _bmsByteMatchSetId (\s a -> s {_bmsByteMatchSetId = a})

-- | Specifies the bytes (typically a string that corresponds with ASCII characters) that you want AWS WAF to search for in web requests, the location in requests that you want AWS WAF to search, and other settings.
bmsByteMatchTuples :: Lens' ByteMatchSet [ByteMatchTuple]
bmsByteMatchTuples = lens _bmsByteMatchTuples (\s a -> s {_bmsByteMatchTuples = a}) . _Coerce

instance FromJSON ByteMatchSet where
  parseJSON =
    withObject
      "ByteMatchSet"
      ( \x ->
          ByteMatchSet'
            <$> (x .:? "Name")
            <*> (x .: "ByteMatchSetId")
            <*> (x .:? "ByteMatchTuples" .!= mempty)
      )

instance Hashable ByteMatchSet

instance NFData ByteMatchSet
