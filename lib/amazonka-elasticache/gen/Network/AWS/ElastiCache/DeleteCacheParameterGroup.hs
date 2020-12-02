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
-- Module      : Network.AWS.ElastiCache.DeleteCacheParameterGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified cache parameter group. You cannot delete a cache parameter group if it is associated with any cache clusters.
--
--
module Network.AWS.ElastiCache.DeleteCacheParameterGroup
    (
    -- * Creating a Request
      deleteCacheParameterGroup
    , DeleteCacheParameterGroup
    -- * Request Lenses
    , dCacheParameterGroupName

    -- * Destructuring the Response
    , deleteCacheParameterGroupResponse
    , DeleteCacheParameterGroupResponse
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DeleteCacheParameterGroup@ operation.
--
--
--
-- /See:/ 'deleteCacheParameterGroup' smart constructor.
newtype DeleteCacheParameterGroup = DeleteCacheParameterGroup'
  { _dCacheParameterGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCacheParameterGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCacheParameterGroupName' - The name of the cache parameter group to delete.
deleteCacheParameterGroup
    :: Text -- ^ 'dCacheParameterGroupName'
    -> DeleteCacheParameterGroup
deleteCacheParameterGroup pCacheParameterGroupName_ =
  DeleteCacheParameterGroup'
    {_dCacheParameterGroupName = pCacheParameterGroupName_}


-- | The name of the cache parameter group to delete.
dCacheParameterGroupName :: Lens' DeleteCacheParameterGroup Text
dCacheParameterGroupName = lens _dCacheParameterGroupName (\ s a -> s{_dCacheParameterGroupName = a})

instance AWSRequest DeleteCacheParameterGroup where
        type Rs DeleteCacheParameterGroup =
             DeleteCacheParameterGroupResponse
        request = postQuery elastiCache
        response
          = receiveNull DeleteCacheParameterGroupResponse'

instance Hashable DeleteCacheParameterGroup where

instance NFData DeleteCacheParameterGroup where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCacheParameterGroupResponse' with the minimum fields required to make a request.
--
deleteCacheParameterGroupResponse
    :: DeleteCacheParameterGroupResponse
deleteCacheParameterGroupResponse = DeleteCacheParameterGroupResponse'


instance NFData DeleteCacheParameterGroupResponse
         where
