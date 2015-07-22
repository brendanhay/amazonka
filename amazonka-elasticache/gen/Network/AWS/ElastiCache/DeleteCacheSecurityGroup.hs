{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteCacheSecurityGroup/ action deletes a cache security group.
--
-- You cannot delete a cache security group if it is associated with any
-- cache clusters.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheSecurityGroup.html>
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    (
    -- * Request
      DeleteCacheSecurityGroup
    -- ** Request constructor
    , deleteCacheSecurityGroup
    -- ** Request lenses
    , dcsgrqCacheSecurityGroupName

    -- * Response
    , DeleteCacheSecurityGroupResponse
    -- ** Response constructor
    , deleteCacheSecurityGroupResponse
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DeleteCacheSecurityGroup/ action.
--
-- /See:/ 'deleteCacheSecurityGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcsgrqCacheSecurityGroupName'
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
    { _dcsgrqCacheSecurityGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheSecurityGroup' smart constructor.
deleteCacheSecurityGroup :: Text -> DeleteCacheSecurityGroup
deleteCacheSecurityGroup pCacheSecurityGroupName =
    DeleteCacheSecurityGroup'
    { _dcsgrqCacheSecurityGroupName = pCacheSecurityGroupName
    }

-- | The name of the cache security group to delete.
--
-- You cannot delete the default security group.
dcsgrqCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup Text
dcsgrqCacheSecurityGroupName = lens _dcsgrqCacheSecurityGroupName (\ s a -> s{_dcsgrqCacheSecurityGroupName = a});

instance AWSRequest DeleteCacheSecurityGroup where
        type Sv DeleteCacheSecurityGroup = ElastiCache
        type Rs DeleteCacheSecurityGroup =
             DeleteCacheSecurityGroupResponse
        request = post
        response
          = receiveNull DeleteCacheSecurityGroupResponse'

instance ToHeaders DeleteCacheSecurityGroup where
        toHeaders = const mempty

instance ToPath DeleteCacheSecurityGroup where
        toPath = const "/"

instance ToQuery DeleteCacheSecurityGroup where
        toQuery DeleteCacheSecurityGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCacheSecurityGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _dcsgrqCacheSecurityGroupName]

-- | /See:/ 'deleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse =
    DeleteCacheSecurityGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheSecurityGroupResponse' smart constructor.
deleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
