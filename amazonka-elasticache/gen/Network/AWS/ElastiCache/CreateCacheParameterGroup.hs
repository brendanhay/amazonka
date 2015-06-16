{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
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

-- | The /CreateCacheParameterGroup/ action creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you
-- apply to all of the nodes in a cache cluster.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_CreateCacheParameterGroup.html>
module Network.AWS.ElastiCache.CreateCacheParameterGroup
    (
    -- * Request
      CreateCacheParameterGroup
    -- ** Request constructor
    , createCacheParameterGroup
    -- ** Request lenses
    , ccpgCacheParameterGroupName
    , ccpgCacheParameterGroupFamily
    , ccpgDescription

    -- * Response
    , CreateCacheParameterGroupResponse
    -- ** Response constructor
    , createCacheParameterGroupResponse
    -- ** Response lenses
    , ccpgrCacheParameterGroup
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElastiCache.Types

-- | /See:/ 'createCacheParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgCacheParameterGroupName'
--
-- * 'ccpgCacheParameterGroupFamily'
--
-- * 'ccpgDescription'
data CreateCacheParameterGroup = CreateCacheParameterGroup'{_ccpgCacheParameterGroupName :: Text, _ccpgCacheParameterGroupFamily :: Text, _ccpgDescription :: Text} deriving (Eq, Read, Show)

-- | 'CreateCacheParameterGroup' smart constructor.
createCacheParameterGroup :: Text -> Text -> Text -> CreateCacheParameterGroup
createCacheParameterGroup pCacheParameterGroupName pCacheParameterGroupFamily pDescription = CreateCacheParameterGroup'{_ccpgCacheParameterGroupName = pCacheParameterGroupName, _ccpgCacheParameterGroupFamily = pCacheParameterGroupFamily, _ccpgDescription = pDescription};

-- | A user-specified name for the cache parameter group.
ccpgCacheParameterGroupName :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupName = lens _ccpgCacheParameterGroupName (\ s a -> s{_ccpgCacheParameterGroupName = a});

-- | The name of the cache parameter group family the cache parameter group
-- can be used with.
--
-- Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@
ccpgCacheParameterGroupFamily :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupFamily = lens _ccpgCacheParameterGroupFamily (\ s a -> s{_ccpgCacheParameterGroupFamily = a});

-- | A user-specified description for the cache parameter group.
ccpgDescription :: Lens' CreateCacheParameterGroup Text
ccpgDescription = lens _ccpgDescription (\ s a -> s{_ccpgDescription = a});

instance AWSRequest CreateCacheParameterGroup where
        type Sv CreateCacheParameterGroup = ElastiCache
        type Rs CreateCacheParameterGroup =
             CreateCacheParameterGroupResponse
        request = post
        response
          = receiveXMLWrapper "CreateCacheParameterGroupResult"
              (\ s h x ->
                 CreateCacheParameterGroupResponse' <$>
                   (x .@? "CacheParameterGroup"))

instance ToHeaders CreateCacheParameterGroup where
        toHeaders = const mempty

instance ToPath CreateCacheParameterGroup where
        toPath = const "/"

instance ToQuery CreateCacheParameterGroup where
        toQuery CreateCacheParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateCacheParameterGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _ccpgCacheParameterGroupName,
               "CacheParameterGroupFamily" =:
                 _ccpgCacheParameterGroupFamily,
               "Description" =: _ccpgDescription]

-- | /See:/ 'createCacheParameterGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccpgrCacheParameterGroup'
newtype CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse'{_ccpgrCacheParameterGroup :: Maybe CacheParameterGroup} deriving (Eq, Read, Show)

-- | 'CreateCacheParameterGroupResponse' smart constructor.
createCacheParameterGroupResponse :: CreateCacheParameterGroupResponse
createCacheParameterGroupResponse = CreateCacheParameterGroupResponse'{_ccpgrCacheParameterGroup = Nothing};

-- | FIXME: Undocumented member.
ccpgrCacheParameterGroup :: Lens' CreateCacheParameterGroupResponse (Maybe CacheParameterGroup)
ccpgrCacheParameterGroup = lens _ccpgrCacheParameterGroup (\ s a -> s{_ccpgrCacheParameterGroup = a});
