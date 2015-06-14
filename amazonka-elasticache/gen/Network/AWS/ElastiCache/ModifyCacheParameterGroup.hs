{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /ModifyCacheParameterGroup/ action modifies the parameters of a
-- cache parameter group. You can modify up to 20 parameters in a single
-- request by submitting a list parameter name and value pairs.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheParameterGroup.html>
module Network.AWS.ElastiCache.ModifyCacheParameterGroup
    (
    -- * Request
      ModifyCacheParameterGroup
    -- ** Request constructor
    , modifyCacheParameterGroup
    -- ** Request lenses
    , mcpgCacheParameterGroupName
    , mcpgParameterNameValues

    -- * Response
    , CacheParameterGroupNameMessage
    -- ** Response constructor
    , cacheParameterGroupNameMessage
    -- ** Response lenses
    , cpgnmCacheParameterGroupName
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElastiCache.Types

-- | /See:/ 'modifyCacheParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcpgCacheParameterGroupName'
--
-- * 'mcpgParameterNameValues'
data ModifyCacheParameterGroup = ModifyCacheParameterGroup'{_mcpgCacheParameterGroupName :: Text, _mcpgParameterNameValues :: [ParameterNameValue]} deriving (Eq, Read, Show)

-- | 'ModifyCacheParameterGroup' smart constructor.
modifyCacheParameterGroup :: Text -> [ParameterNameValue] -> ModifyCacheParameterGroup
modifyCacheParameterGroup pCacheParameterGroupName pParameterNameValues = ModifyCacheParameterGroup'{_mcpgCacheParameterGroupName = pCacheParameterGroupName, _mcpgParameterNameValues = pParameterNameValues};

-- | The name of the cache parameter group to modify.
mcpgCacheParameterGroupName :: Lens' ModifyCacheParameterGroup Text
mcpgCacheParameterGroupName = lens _mcpgCacheParameterGroupName (\ s a -> s{_mcpgCacheParameterGroupName = a});

-- | An array of parameter names and values for the parameter update. You
-- must supply at least one parameter name and value; subsequent arguments
-- are optional. A maximum of 20 parameters may be modified per request.
mcpgParameterNameValues :: Lens' ModifyCacheParameterGroup [ParameterNameValue]
mcpgParameterNameValues = lens _mcpgParameterNameValues (\ s a -> s{_mcpgParameterNameValues = a});

instance AWSRequest ModifyCacheParameterGroup where
        type Sv ModifyCacheParameterGroup = ElastiCache
        type Rs ModifyCacheParameterGroup =
             CacheParameterGroupNameMessage
        request = post
        response
          = receiveXMLWrapper "ModifyCacheParameterGroupResult"
              (\ s h x -> parseXML x)

instance ToHeaders ModifyCacheParameterGroup where
        toHeaders = const mempty

instance ToPath ModifyCacheParameterGroup where
        toPath = const "/"

instance ToQuery ModifyCacheParameterGroup where
        toQuery ModifyCacheParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyCacheParameterGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _mcpgCacheParameterGroupName,
               "ParameterNameValues" =:
                 "ParameterNameValue" =: _mcpgParameterNameValues]
