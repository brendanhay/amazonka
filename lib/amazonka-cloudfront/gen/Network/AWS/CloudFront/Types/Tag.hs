{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Tag
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Tag where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A complex type that contains @Tag@ key and @Tag@ value.
--
--
--
-- /See:/ 'tag' smart constructor.
data Tag = Tag' {_tagValue :: !(Maybe Text), _tagKey :: !Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Tag' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'tagValue' - A string that contains an optional @Tag@ value. The string length should be between 0 and 256 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
--
-- * 'tagKey' - A string that contains @Tag@ key. The string length should be between 1 and 128 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
tag ::
  -- | 'tagKey'
  Text ->
  Tag
tag pKey_ = Tag' {_tagValue = Nothing, _tagKey = pKey_}

-- | A string that contains an optional @Tag@ value. The string length should be between 0 and 256 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
tagValue :: Lens' Tag (Maybe Text)
tagValue = lens _tagValue (\s a -> s {_tagValue = a})

-- | A string that contains @Tag@ key. The string length should be between 1 and 128 characters. Valid characters include @a-z@ , @A-Z@ , @0-9@ , space, and the special characters @_ - . : / = + @@ .
tagKey :: Lens' Tag Text
tagKey = lens _tagKey (\s a -> s {_tagKey = a})

instance FromXML Tag where
  parseXML x = Tag' <$> (x .@? "Value") <*> (x .@ "Key")

instance Hashable Tag

instance NFData Tag

instance ToXML Tag where
  toXML Tag' {..} = mconcat ["Value" @= _tagValue, "Key" @= _tagKey]
