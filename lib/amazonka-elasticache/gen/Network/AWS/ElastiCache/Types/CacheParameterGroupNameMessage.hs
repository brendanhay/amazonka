{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.CacheParameterGroupNameMessage where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the output of one of the following operations:
--
--
--     * @ModifyCacheParameterGroup@
--
--     * @ResetCacheParameterGroup@
--
--
--
--
-- /See:/ 'cacheParameterGroupNameMessage' smart constructor.
newtype CacheParameterGroupNameMessage = CacheParameterGroupNameMessage'
  { _cpgnmCacheParameterGroupName ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CacheParameterGroupNameMessage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgnmCacheParameterGroupName' - The name of the cache parameter group.
cacheParameterGroupNameMessage ::
  CacheParameterGroupNameMessage
cacheParameterGroupNameMessage =
  CacheParameterGroupNameMessage'
    { _cpgnmCacheParameterGroupName =
        Nothing
    }

-- | The name of the cache parameter group.
cpgnmCacheParameterGroupName :: Lens' CacheParameterGroupNameMessage (Maybe Text)
cpgnmCacheParameterGroupName = lens _cpgnmCacheParameterGroupName (\s a -> s {_cpgnmCacheParameterGroupName = a})

instance FromXML CacheParameterGroupNameMessage where
  parseXML x =
    CacheParameterGroupNameMessage'
      <$> (x .@? "CacheParameterGroupName")

instance Hashable CacheParameterGroupNameMessage

instance NFData CacheParameterGroupNameMessage
