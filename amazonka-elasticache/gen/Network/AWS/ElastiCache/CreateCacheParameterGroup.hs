{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.CreateCacheParameterGroup
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /CreateCacheParameterGroup/ action creates a new cache parameter
-- group. A cache parameter group is a collection of parameters that you
-- apply to all of the nodes in a cache cluster.
module Network.AWS.ElastiCache.CreateCacheParameterGroup
    (
    -- * Creating a Request
      createCacheParameterGroup
    , CreateCacheParameterGroup
    -- * Request Lenses
    , ccpgCacheParameterGroupName
    , ccpgCacheParameterGroupFamily
    , ccpgDescription

    -- * Destructuring the Response
    , createCacheParameterGroupResponse
    , CreateCacheParameterGroupResponse
    -- * Response Lenses
    , ccpgrsCacheParameterGroup
    , ccpgrsResponseStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.ElastiCache.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /CreateCacheParameterGroup/ action.
--
-- /See:/ 'createCacheParameterGroup' smart constructor.
data CreateCacheParameterGroup = CreateCacheParameterGroup'
    { _ccpgCacheParameterGroupName   :: !Text
    , _ccpgCacheParameterGroupFamily :: !Text
    , _ccpgDescription               :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpgCacheParameterGroupName'
--
-- * 'ccpgCacheParameterGroupFamily'
--
-- * 'ccpgDescription'
createCacheParameterGroup
    :: Text -- ^ 'ccpgCacheParameterGroupName'
    -> Text -- ^ 'ccpgCacheParameterGroupFamily'
    -> Text -- ^ 'ccpgDescription'
    -> CreateCacheParameterGroup
createCacheParameterGroup pCacheParameterGroupName_ pCacheParameterGroupFamily_ pDescription_ =
    CreateCacheParameterGroup'
    { _ccpgCacheParameterGroupName = pCacheParameterGroupName_
    , _ccpgCacheParameterGroupFamily = pCacheParameterGroupFamily_
    , _ccpgDescription = pDescription_
    }

-- | A user-specified name for the cache parameter group.
ccpgCacheParameterGroupName :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupName = lens _ccpgCacheParameterGroupName (\ s a -> s{_ccpgCacheParameterGroupName = a});

-- | The name of the cache parameter group family the cache parameter group
-- can be used with.
--
-- Valid values are: 'memcached1.4' | 'redis2.6' | 'redis2.8'
ccpgCacheParameterGroupFamily :: Lens' CreateCacheParameterGroup Text
ccpgCacheParameterGroupFamily = lens _ccpgCacheParameterGroupFamily (\ s a -> s{_ccpgCacheParameterGroupFamily = a});

-- | A user-specified description for the cache parameter group.
ccpgDescription :: Lens' CreateCacheParameterGroup Text
ccpgDescription = lens _ccpgDescription (\ s a -> s{_ccpgDescription = a});

instance AWSRequest CreateCacheParameterGroup where
        type Rs CreateCacheParameterGroup =
             CreateCacheParameterGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "CreateCacheParameterGroupResult"
              (\ s h x ->
                 CreateCacheParameterGroupResponse' <$>
                   (x .@? "CacheParameterGroup") <*>
                     (pure (fromEnum s)))

instance Hashable CreateCacheParameterGroup

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
data CreateCacheParameterGroupResponse = CreateCacheParameterGroupResponse'
    { _ccpgrsCacheParameterGroup :: !(Maybe CacheParameterGroup)
    , _ccpgrsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateCacheParameterGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccpgrsCacheParameterGroup'
--
-- * 'ccpgrsResponseStatus'
createCacheParameterGroupResponse
    :: Int -- ^ 'ccpgrsResponseStatus'
    -> CreateCacheParameterGroupResponse
createCacheParameterGroupResponse pResponseStatus_ =
    CreateCacheParameterGroupResponse'
    { _ccpgrsCacheParameterGroup = Nothing
    , _ccpgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ccpgrsCacheParameterGroup :: Lens' CreateCacheParameterGroupResponse (Maybe CacheParameterGroup)
ccpgrsCacheParameterGroup = lens _ccpgrsCacheParameterGroup (\ s a -> s{_ccpgrsCacheParameterGroup = a});

-- | The response status code.
ccpgrsResponseStatus :: Lens' CreateCacheParameterGroupResponse Int
ccpgrsResponseStatus = lens _ccpgrsResponseStatus (\ s a -> s{_ccpgrsResponseStatus = a});
