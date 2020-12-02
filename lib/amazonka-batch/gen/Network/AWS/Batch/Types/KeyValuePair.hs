{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.KeyValuePair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.KeyValuePair where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A key-value pair object.
--
--
--
-- /See:/ 'keyValuePair' smart constructor.
data KeyValuePair = KeyValuePair'
  { _kvpValue :: !(Maybe Text),
    _kvpName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'KeyValuePair' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'kvpValue' - The value of the key-value pair. For environment variables, this is the value of the environment variable.
--
-- * 'kvpName' - The name of the key-value pair. For environment variables, this is the name of the environment variable.
keyValuePair ::
  KeyValuePair
keyValuePair =
  KeyValuePair' {_kvpValue = Nothing, _kvpName = Nothing}

-- | The value of the key-value pair. For environment variables, this is the value of the environment variable.
kvpValue :: Lens' KeyValuePair (Maybe Text)
kvpValue = lens _kvpValue (\s a -> s {_kvpValue = a})

-- | The name of the key-value pair. For environment variables, this is the name of the environment variable.
kvpName :: Lens' KeyValuePair (Maybe Text)
kvpName = lens _kvpName (\s a -> s {_kvpName = a})

instance FromJSON KeyValuePair where
  parseJSON =
    withObject
      "KeyValuePair"
      (\x -> KeyValuePair' <$> (x .:? "value") <*> (x .:? "name"))

instance Hashable KeyValuePair

instance NFData KeyValuePair

instance ToJSON KeyValuePair where
  toJSON KeyValuePair' {..} =
    object
      (catMaybes [("value" .=) <$> _kvpValue, ("name" .=) <$> _kvpName])
