{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.CacheAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.CacheAttributes where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Lists refresh cache information.
--
--
--
-- /See:/ 'cacheAttributes' smart constructor.
newtype CacheAttributes = CacheAttributes'
  { _caCacheStaleTimeoutInSeconds ::
      Maybe Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'caCacheStaleTimeoutInSeconds' - Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds. Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
cacheAttributes ::
  CacheAttributes
cacheAttributes =
  CacheAttributes' {_caCacheStaleTimeoutInSeconds = Nothing}

-- | Refreshes a file share's cache by using Time To Live (TTL). TTL is the length of time since the last refresh after which access to the directory would cause the file gateway to first refresh that directory's contents from the Amazon S3 bucket. The TTL duration is in seconds. Valid Values: 300 to 2,592,000 seconds (5 minutes to 30 days)
caCacheStaleTimeoutInSeconds :: Lens' CacheAttributes (Maybe Int)
caCacheStaleTimeoutInSeconds = lens _caCacheStaleTimeoutInSeconds (\s a -> s {_caCacheStaleTimeoutInSeconds = a})

instance FromJSON CacheAttributes where
  parseJSON =
    withObject
      "CacheAttributes"
      (\x -> CacheAttributes' <$> (x .:? "CacheStaleTimeoutInSeconds"))

instance Hashable CacheAttributes

instance NFData CacheAttributes

instance ToJSON CacheAttributes where
  toJSON CacheAttributes' {..} =
    object
      ( catMaybes
          [ ("CacheStaleTimeoutInSeconds" .=)
              <$> _caCacheStaleTimeoutInSeconds
          ]
      )
