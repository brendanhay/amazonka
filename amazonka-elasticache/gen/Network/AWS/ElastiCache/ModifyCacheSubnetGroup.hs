{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /ModifyCacheSubnetGroup/ action modifies an existing cache subnet
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
    , mcsgrqSubnetIds
    , mcsgrqCacheSubnetGroupDescription
    , mcsgrqCacheSubnetGroupName

    -- * Response
    , ModifyCacheSubnetGroupResponse
    -- ** Response constructor
    , modifyCacheSubnetGroupResponse
    -- ** Response lenses
    , mcsgrsCacheSubnetGroup
    , mcsgrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /ModifyCacheSubnetGroup/ action.
--
-- /See:/ 'modifyCacheSubnetGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrqSubnetIds'
--
-- * 'mcsgrqCacheSubnetGroupDescription'
--
-- * 'mcsgrqCacheSubnetGroupName'
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
    { _mcsgrqSubnetIds                   :: !(Maybe [Text])
    , _mcsgrqCacheSubnetGroupDescription :: !(Maybe Text)
    , _mcsgrqCacheSubnetGroupName        :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyCacheSubnetGroup' smart constructor.
modifyCacheSubnetGroup :: Text -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup pCacheSubnetGroupName_ =
    ModifyCacheSubnetGroup'
    { _mcsgrqSubnetIds = Nothing
    , _mcsgrqCacheSubnetGroupDescription = Nothing
    , _mcsgrqCacheSubnetGroupName = pCacheSubnetGroupName_
    }

-- | The EC2 subnet IDs for the cache subnet group.
mcsgrqSubnetIds :: Lens' ModifyCacheSubnetGroup [Text]
mcsgrqSubnetIds = lens _mcsgrqSubnetIds (\ s a -> s{_mcsgrqSubnetIds = a}) . _Default;

-- | A description for the cache subnet group.
mcsgrqCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroup (Maybe Text)
mcsgrqCacheSubnetGroupDescription = lens _mcsgrqCacheSubnetGroupDescription (\ s a -> s{_mcsgrqCacheSubnetGroupDescription = a});

-- | The name for the cache subnet group. This value is stored as a lowercase
-- string.
--
-- Constraints: Must contain no more than 255 alphanumeric characters or
-- hyphens.
--
-- Example: @mysubnetgroup@
mcsgrqCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroup Text
mcsgrqCacheSubnetGroupName = lens _mcsgrqCacheSubnetGroupName (\ s a -> s{_mcsgrqCacheSubnetGroupName = a});

instance AWSRequest ModifyCacheSubnetGroup where
        type Sv ModifyCacheSubnetGroup = ElastiCache
        type Rs ModifyCacheSubnetGroup =
             ModifyCacheSubnetGroupResponse
        request = post
        response
          = receiveXMLWrapper "ModifyCacheSubnetGroupResult"
              (\ s h x ->
                 ModifyCacheSubnetGroupResponse' <$>
                   (x .@? "CacheSubnetGroup") <*> (pure (fromEnum s)))

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
                   (toQueryList "SubnetIdentifier" <$>
                      _mcsgrqSubnetIds),
               "CacheSubnetGroupDescription" =:
                 _mcsgrqCacheSubnetGroupDescription,
               "CacheSubnetGroupName" =:
                 _mcsgrqCacheSubnetGroupName]

-- | /See:/ 'modifyCacheSubnetGroupResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrsCacheSubnetGroup'
--
-- * 'mcsgrsStatus'
data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'
    { _mcsgrsCacheSubnetGroup :: !(Maybe CacheSubnetGroup)
    , _mcsgrsStatus           :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyCacheSubnetGroupResponse' smart constructor.
modifyCacheSubnetGroupResponse :: Int -> ModifyCacheSubnetGroupResponse
modifyCacheSubnetGroupResponse pStatus_ =
    ModifyCacheSubnetGroupResponse'
    { _mcsgrsCacheSubnetGroup = Nothing
    , _mcsgrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
mcsgrsCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
mcsgrsCacheSubnetGroup = lens _mcsgrsCacheSubnetGroup (\ s a -> s{_mcsgrsCacheSubnetGroup = a});

-- | FIXME: Undocumented member.
mcsgrsStatus :: Lens' ModifyCacheSubnetGroupResponse Int
mcsgrsStatus = lens _mcsgrsStatus (\ s a -> s{_mcsgrsStatus = a});
