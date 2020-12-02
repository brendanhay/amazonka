{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.HTTPActionHeader
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.HTTPActionHeader where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The HTTP action header.
--
--
--
-- /See:/ 'hTTPActionHeader' smart constructor.
data HTTPActionHeader = HTTPActionHeader'
  { _httpahKey :: !Text,
    _httpahValue :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'HTTPActionHeader' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'httpahKey' - The HTTP header key.
--
-- * 'httpahValue' - The HTTP header value. Substitution templates are supported.
hTTPActionHeader ::
  -- | 'httpahKey'
  Text ->
  -- | 'httpahValue'
  Text ->
  HTTPActionHeader
hTTPActionHeader pKey_ pValue_ =
  HTTPActionHeader' {_httpahKey = pKey_, _httpahValue = pValue_}

-- | The HTTP header key.
httpahKey :: Lens' HTTPActionHeader Text
httpahKey = lens _httpahKey (\s a -> s {_httpahKey = a})

-- | The HTTP header value. Substitution templates are supported.
httpahValue :: Lens' HTTPActionHeader Text
httpahValue = lens _httpahValue (\s a -> s {_httpahValue = a})

instance FromJSON HTTPActionHeader where
  parseJSON =
    withObject
      "HTTPActionHeader"
      (\x -> HTTPActionHeader' <$> (x .: "key") <*> (x .: "value"))

instance Hashable HTTPActionHeader

instance NFData HTTPActionHeader

instance ToJSON HTTPActionHeader where
  toJSON HTTPActionHeader' {..} =
    object
      ( catMaybes
          [Just ("key" .= _httpahKey), Just ("value" .= _httpahValue)]
      )
