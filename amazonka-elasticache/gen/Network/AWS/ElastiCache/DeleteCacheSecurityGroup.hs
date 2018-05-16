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
-- Module      : Network.AWS.ElastiCache.DeleteCacheSecurityGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache security group.
--
--
module Network.AWS.ElastiCache.DeleteCacheSecurityGroup
    (
    -- * Creating a Request
      deleteCacheSecurityGroup
    , DeleteCacheSecurityGroup
    -- * Request Lenses
    , dcsgCacheSecurityGroupName

    -- * Destructuring the Response
    , deleteCacheSecurityGroupResponse
    , DeleteCacheSecurityGroupResponse
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DeleteCacheSecurityGroup@ operation.
--
--
--
-- /See:/ 'deleteCacheSecurityGroup' smart constructor.
newtype DeleteCacheSecurityGroup = DeleteCacheSecurityGroup'
  { _dcsgCacheSecurityGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCacheSecurityGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgCacheSecurityGroupName' - The name of the cache security group to delete.
deleteCacheSecurityGroup
    :: Text -- ^ 'dcsgCacheSecurityGroupName'
    -> DeleteCacheSecurityGroup
deleteCacheSecurityGroup pCacheSecurityGroupName_ =
  DeleteCacheSecurityGroup'
    {_dcsgCacheSecurityGroupName = pCacheSecurityGroupName_}


-- | The name of the cache security group to delete.
dcsgCacheSecurityGroupName :: Lens' DeleteCacheSecurityGroup Text
dcsgCacheSecurityGroupName = lens _dcsgCacheSecurityGroupName (\ s a -> s{_dcsgCacheSecurityGroupName = a})

instance AWSRequest DeleteCacheSecurityGroup where
        type Rs DeleteCacheSecurityGroup =
             DeleteCacheSecurityGroupResponse
        request = postQuery elastiCache
        response
          = receiveNull DeleteCacheSecurityGroupResponse'

instance Hashable DeleteCacheSecurityGroup where

instance NFData DeleteCacheSecurityGroup where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCacheSecurityGroupResponse' with the minimum fields required to make a request.
--
deleteCacheSecurityGroupResponse
    :: DeleteCacheSecurityGroupResponse
deleteCacheSecurityGroupResponse = DeleteCacheSecurityGroupResponse'


instance NFData DeleteCacheSecurityGroupResponse
         where
