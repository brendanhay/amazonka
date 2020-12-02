{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APIKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APIKey where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an API key.
--
--
-- Customers invoke AWS AppSync GraphQL API operations with API keys as an identity mechanism. There are two key versions:
--
-- __da1__ : This version was introduced at launch in November 2017. These keys always expire after 7 days. Key expiration is managed by Amazon DynamoDB TTL. The keys ceased to be valid after February 21, 2018 and should not be used after that date.
--
--     * @ListApiKeys@ returns the expiration time in milliseconds.
--
--     * @CreateApiKey@ returns the expiration time in milliseconds.
--
--     * @UpdateApiKey@ is not available for this key version.
--
--     * @DeleteApiKey@ deletes the item from the table.
--
--     * Expiration is stored in Amazon DynamoDB as milliseconds. This results in a bug where keys are not automatically deleted because DynamoDB expects the TTL to be stored in seconds. As a one-time action, we will delete these keys from the table after February 21, 2018.
--
--
--
-- __da2__ : This version was introduced in February 2018 when AppSync added support to extend key expiration.
--
--     * @ListApiKeys@ returns the expiration time and deletion time in seconds.
--
--     * @CreateApiKey@ returns the expiration time and deletion time in seconds and accepts a user-provided expiration time in seconds.
--
--     * @UpdateApiKey@ returns the expiration time and and deletion time in seconds and accepts a user-provided expiration time in seconds. Expired API keys are kept for 60 days after the expiration time. Key expiration time can be updated while the key is not deleted.
--
--     * @DeleteApiKey@ deletes the item from the table.
--
--     * Expiration is stored in Amazon DynamoDB as seconds. After the expiration time, using the key to authenticate will fail. But the key can be reinstated before deletion.
--
--     * Deletion is stored in Amazon DynamoDB as seconds. The key will be deleted after deletion time.
--
--
--
--
-- /See:/ 'apiKey' smart constructor.
data APIKey = APIKey'
  { _akExpires :: !(Maybe Integer),
    _akDeletes :: !(Maybe Integer),
    _akId :: !(Maybe Text),
    _akDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APIKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'akExpires' - The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- * 'akDeletes' - The time after which the API key is deleted. The date is represented as seconds since the epoch, rounded down to the nearest hour.
--
-- * 'akId' - The API key ID.
--
-- * 'akDescription' - A description of the purpose of the API key.
apiKey ::
  APIKey
apiKey =
  APIKey'
    { _akExpires = Nothing,
      _akDeletes = Nothing,
      _akId = Nothing,
      _akDescription = Nothing
    }

-- | The time after which the API key expires. The date is represented as seconds since the epoch, rounded down to the nearest hour.
akExpires :: Lens' APIKey (Maybe Integer)
akExpires = lens _akExpires (\s a -> s {_akExpires = a})

-- | The time after which the API key is deleted. The date is represented as seconds since the epoch, rounded down to the nearest hour.
akDeletes :: Lens' APIKey (Maybe Integer)
akDeletes = lens _akDeletes (\s a -> s {_akDeletes = a})

-- | The API key ID.
akId :: Lens' APIKey (Maybe Text)
akId = lens _akId (\s a -> s {_akId = a})

-- | A description of the purpose of the API key.
akDescription :: Lens' APIKey (Maybe Text)
akDescription = lens _akDescription (\s a -> s {_akDescription = a})

instance FromJSON APIKey where
  parseJSON =
    withObject
      "APIKey"
      ( \x ->
          APIKey'
            <$> (x .:? "expires")
            <*> (x .:? "deletes")
            <*> (x .:? "id")
            <*> (x .:? "description")
      )

instance Hashable APIKey

instance NFData APIKey
