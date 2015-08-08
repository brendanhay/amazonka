{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteCacheParameterGroup/ action deletes the specified cache
-- parameter group. You cannot delete a cache parameter group if it is
-- associated with any cache clusters.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheParameterGroup.html AWS API Reference> for DeleteCacheParameterGroup.
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
    (
    -- * Creating a Request
      DeleteCacheParameterGroup
    , deleteCacheParameterGroup
    -- * Request Lenses
    , dCacheParameterGroupName

    -- * Destructuring the Response
    , DeleteCacheParameterGroupResponse
    , deleteCacheParameterGroupResponse
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteCacheParameterGroup/ action.
--
-- /See:/ 'deleteCacheParameterGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dCacheParameterGroupName'
newtype DeleteCacheParameterGroup = DeleteCacheParameterGroup'
    { _dCacheParameterGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheParameterGroup' smart constructor.
deleteCacheParameterGroup :: Text -> DeleteCacheParameterGroup
deleteCacheParameterGroup pCacheParameterGroupName_ =
    DeleteCacheParameterGroup'
    { _dCacheParameterGroupName = pCacheParameterGroupName_
    }

-- | The name of the cache parameter group to delete.
--
-- The specified cache security group must not be associated with any cache
-- clusters.
dCacheParameterGroupName :: Lens' DeleteCacheParameterGroup Text
dCacheParameterGroupName = lens _dCacheParameterGroupName (\ s a -> s{_dCacheParameterGroupName = a});

instance AWSRequest DeleteCacheParameterGroup where
        type Sv DeleteCacheParameterGroup = ElastiCache
        type Rs DeleteCacheParameterGroup =
             DeleteCacheParameterGroupResponse
        request = postQuery
        response
          = receiveNull DeleteCacheParameterGroupResponse'

instance ToHeaders DeleteCacheParameterGroup where
        toHeaders = const mempty

instance ToPath DeleteCacheParameterGroup where
        toPath = const "/"

instance ToQuery DeleteCacheParameterGroup where
        toQuery DeleteCacheParameterGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCacheParameterGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _dCacheParameterGroupName]

-- | /See:/ 'deleteCacheParameterGroupResponse' smart constructor.
data DeleteCacheParameterGroupResponse =
    DeleteCacheParameterGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheParameterGroupResponse' smart constructor.
deleteCacheParameterGroupResponse :: DeleteCacheParameterGroupResponse
deleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'
