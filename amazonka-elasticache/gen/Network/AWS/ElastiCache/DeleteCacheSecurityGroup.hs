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
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The /DeleteCacheSecurityGroup/ action deletes a cache security group.
--
-- You cannot delete a cache security group if it is associated with any
-- cache clusters.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DeleteCacheSecurityGroup.html AWS API Reference> for DeleteCacheSecurityGroup.
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    (
    -- * Creating a Request
      DeleteCacheSecurityGroup
    , deleteCacheSecurityGroup
    -- * Request Lenses
    , dcsgCacheSecurityGroupName

    -- * Destructuring the Response
    , DeleteCacheSecurityGroupResponse
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
-- * 'dcsgCacheSecurityGroupName'
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
    { _dcsgCacheSecurityGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheSecurityGroup' smart constructor.
deleteCacheSecurityGroup :: Text -> DeleteCacheSecurityGroup
deleteCacheSecurityGroup pCacheSecurityGroupName_ =
    DeleteCacheSecurityGroup'
    { _dcsgCacheSecurityGroupName = pCacheSecurityGroupName_
    }

-- | The name of the cache security group to delete.
--
-- You cannot delete the default security group.
dcsgCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup Text
dcsgCacheSecurityGroupName = lens _dcsgCacheSecurityGroupName (\ s a -> s{_dcsgCacheSecurityGroupName = a});

instance AWSRequest DeleteCacheSecurityGroup where
        type Sv DeleteCacheSecurityGroup = ElastiCache
        type Rs DeleteCacheSecurityGroup =
             DeleteCacheSecurityGroupResponse
        request = postQuery
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
                 _dcsgCacheSecurityGroupName]

-- | /See:/ 'deleteCacheSecurityGroupResponse' smart constructor.
data DeleteCacheSecurityGroupResponse =
    DeleteCacheSecurityGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteCacheSecurityGroupResponse' smart constructor.
deleteCacheSecurityGroupResponse :: DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'
