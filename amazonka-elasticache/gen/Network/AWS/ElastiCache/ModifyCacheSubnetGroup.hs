{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
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

-- | The /ModifyCacheSubnetGroup/ action modifies an existing cache subnet
-- group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_ModifyCacheSubnetGroup.html>
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    (
    -- * Request
      ModifyCacheSubnetGroup
    -- ** Request constructor
    , modifyCacheSubnetGroup
    -- ** Request lenses
    , mcsgSubnetIds
    , mcsgCacheSubnetGroupDescription
    , mcsgCacheSubnetGroupName

    -- * Response
    , ModifyCacheSubnetGroupResponse
    -- ** Response constructor
    , modifyCacheSubnetGroupResponse
    -- ** Response lenses
    , mcsgrCacheSubnetGroup
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElastiCache.Types

-- | /See:/ 'modifyCacheSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgSubnetIds'
--
-- * 'mcsgCacheSubnetGroupDescription'
--
-- * 'mcsgCacheSubnetGroupName'
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'{_mcsgSubnetIds :: Maybe [Text], _mcsgCacheSubnetGroupDescription :: Maybe Text, _mcsgCacheSubnetGroupName :: Text} deriving (Eq, Read, Show)

-- | 'ModifyCacheSubnetGroup' smart constructor.
modifyCacheSubnetGroup :: Text -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup pCacheSubnetGroupName = ModifyCacheSubnetGroup'{_mcsgSubnetIds = Nothing, _mcsgCacheSubnetGroupDescription = Nothing, _mcsgCacheSubnetGroupName = pCacheSubnetGroupName};

-- | The EC2 subnet IDs for the cache subnet group.
mcsgSubnetIds :: Lens' ModifyCacheSubnetGroup [Text]
mcsgSubnetIds = lens _mcsgSubnetIds (\ s a -> s{_mcsgSubnetIds = a}) . _Default;

-- | A description for the cache subnet group.
mcsgCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroup (Maybe Text)
mcsgCacheSubnetGroupDescription = lens _mcsgCacheSubnetGroupDescription (\ s a -> s{_mcsgCacheSubnetGroupDescription = a});

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
--
-- Example: @mysubnetgroup@
mcsgCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroup Text
mcsgCacheSubnetGroupName = lens _mcsgCacheSubnetGroupName (\ s a -> s{_mcsgCacheSubnetGroupName = a});

instance AWSRequest ModifyCacheSubnetGroup where
        type Sv ModifyCacheSubnetGroup = ElastiCache
        type Rs ModifyCacheSubnetGroup =
             ModifyCacheSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "ModifyCacheSubnetGroupResult"
              (\ s h x ->
                 ModifyCacheSubnetGroupResponse' <$>
                   (x .@? "CacheSubnetGroup"))

instance ToHeaders ModifyCacheSubnetGroup where
        toHeaders = const mempty

instance ToPath ModifyCacheSubnetGroup where
        toPath = const "/"

instance ToQuery ModifyCacheSubnetGroup where
        toQuery ModifyCacheSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("ModifyCacheSubnetGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "SubnetIds" =:
                 toQuery
                   (toQueryList "SubnetIdentifier" <$> _mcsgSubnetIds),
               "CacheSubnetGroupDescription" =:
                 _mcsgCacheSubnetGroupDescription,
               "CacheSubnetGroupName" =: _mcsgCacheSubnetGroupName]

-- | /See:/ 'modifyCacheSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrCacheSubnetGroup'
newtype ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'{_mcsgrCacheSubnetGroup :: Maybe CacheSubnetGroup} deriving (Eq, Read, Show)

-- | 'ModifyCacheSubnetGroupResponse' smart constructor.
modifyCacheSubnetGroupResponse :: ModifyCacheSubnetGroupResponse
modifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'{_mcsgrCacheSubnetGroup = Nothing};

-- | FIXME: Undocumented member.
mcsgrCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
mcsgrCacheSubnetGroup = lens _mcsgrCacheSubnetGroup (\ s a -> s{_mcsgrCacheSubnetGroup = a});
