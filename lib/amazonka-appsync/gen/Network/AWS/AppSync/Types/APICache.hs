{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.APICache
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.APICache where

import Network.AWS.AppSync.Types.APICacheStatus
import Network.AWS.AppSync.Types.APICacheType
import Network.AWS.AppSync.Types.APICachingBehavior
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @ApiCache@ object.
--
--
--
-- /See:/ 'apiCache' smart constructor.
data APICache = APICache'
  { _acTtl :: !(Maybe Integer),
    _acStatus :: !(Maybe APICacheStatus),
    _acAtRestEncryptionEnabled :: !(Maybe Bool),
    _acTransitEncryptionEnabled :: !(Maybe Bool),
    _acApiCachingBehavior :: !(Maybe APICachingBehavior),
    _acType :: !(Maybe APICacheType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'APICache' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'acTtl' - TTL in seconds for cache entries. Valid values are between 1 and 3600 seconds.
--
-- * 'acStatus' - The cache instance status.     * __AVAILABLE__ : The instance is available for use.     * __CREATING__ : The instance is currently creating.     * __DELETING__ : The instance is currently deleting.     * __MODIFYING__ : The instance is currently modifying.     * __FAILED__ : The instance has failed creation.
--
-- * 'acAtRestEncryptionEnabled' - At rest encryption flag for cache. This setting cannot be updated after creation.
--
-- * 'acTransitEncryptionEnabled' - Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
--
-- * 'acApiCachingBehavior' - Caching behavior.     * __FULL_REQUEST_CACHING__ : All requests are fully cached.     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
--
-- * 'acType' - The cache instance type. Valid values are      * @SMALL@      * @MEDIUM@      * @LARGE@      * @XLARGE@      * @LARGE_2X@      * @LARGE_4X@      * @LARGE_8X@ (not available in all regions)     * @LARGE_12X@  Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used. The following legacy instance types are available, but their use is discouraged:     * __T2_SMALL__ : A t2.small instance type.     * __T2_MEDIUM__ : A t2.medium instance type.     * __R4_LARGE__ : A r4.large instance type.     * __R4_XLARGE__ : A r4.xlarge instance type.     * __R4_2XLARGE__ : A r4.2xlarge instance type.     * __R4_4XLARGE__ : A r4.4xlarge instance type.     * __R4_8XLARGE__ : A r4.8xlarge instance type.
apiCache ::
  APICache
apiCache =
  APICache'
    { _acTtl = Nothing,
      _acStatus = Nothing,
      _acAtRestEncryptionEnabled = Nothing,
      _acTransitEncryptionEnabled = Nothing,
      _acApiCachingBehavior = Nothing,
      _acType = Nothing
    }

-- | TTL in seconds for cache entries. Valid values are between 1 and 3600 seconds.
acTtl :: Lens' APICache (Maybe Integer)
acTtl = lens _acTtl (\s a -> s {_acTtl = a})

-- | The cache instance status.     * __AVAILABLE__ : The instance is available for use.     * __CREATING__ : The instance is currently creating.     * __DELETING__ : The instance is currently deleting.     * __MODIFYING__ : The instance is currently modifying.     * __FAILED__ : The instance has failed creation.
acStatus :: Lens' APICache (Maybe APICacheStatus)
acStatus = lens _acStatus (\s a -> s {_acStatus = a})

-- | At rest encryption flag for cache. This setting cannot be updated after creation.
acAtRestEncryptionEnabled :: Lens' APICache (Maybe Bool)
acAtRestEncryptionEnabled = lens _acAtRestEncryptionEnabled (\s a -> s {_acAtRestEncryptionEnabled = a})

-- | Transit encryption flag when connecting to cache. This setting cannot be updated after creation.
acTransitEncryptionEnabled :: Lens' APICache (Maybe Bool)
acTransitEncryptionEnabled = lens _acTransitEncryptionEnabled (\s a -> s {_acTransitEncryptionEnabled = a})

-- | Caching behavior.     * __FULL_REQUEST_CACHING__ : All requests are fully cached.     * __PER_RESOLVER_CACHING__ : Individual resolvers that you specify are cached.
acApiCachingBehavior :: Lens' APICache (Maybe APICachingBehavior)
acApiCachingBehavior = lens _acApiCachingBehavior (\s a -> s {_acApiCachingBehavior = a})

-- | The cache instance type. Valid values are      * @SMALL@      * @MEDIUM@      * @LARGE@      * @XLARGE@      * @LARGE_2X@      * @LARGE_4X@      * @LARGE_8X@ (not available in all regions)     * @LARGE_12X@  Historically, instance types were identified by an EC2-style value. As of July 2020, this is deprecated, and the generic identifiers above should be used. The following legacy instance types are available, but their use is discouraged:     * __T2_SMALL__ : A t2.small instance type.     * __T2_MEDIUM__ : A t2.medium instance type.     * __R4_LARGE__ : A r4.large instance type.     * __R4_XLARGE__ : A r4.xlarge instance type.     * __R4_2XLARGE__ : A r4.2xlarge instance type.     * __R4_4XLARGE__ : A r4.4xlarge instance type.     * __R4_8XLARGE__ : A r4.8xlarge instance type.
acType :: Lens' APICache (Maybe APICacheType)
acType = lens _acType (\s a -> s {_acType = a})

instance FromJSON APICache where
  parseJSON =
    withObject
      "APICache"
      ( \x ->
          APICache'
            <$> (x .:? "ttl")
            <*> (x .:? "status")
            <*> (x .:? "atRestEncryptionEnabled")
            <*> (x .:? "transitEncryptionEnabled")
            <*> (x .:? "apiCachingBehavior")
            <*> (x .:? "type")
      )

instance Hashable APICache

instance NFData APICache
