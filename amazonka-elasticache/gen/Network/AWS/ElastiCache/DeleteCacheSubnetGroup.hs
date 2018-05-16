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
-- Module      : Network.AWS.ElastiCache.DeleteCacheSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a cache subnet group.
--
--
module Network.AWS.ElastiCache.DeleteCacheSubnetGroup
    (
    -- * Creating a Request
      deleteCacheSubnetGroup
    , DeleteCacheSubnetGroup
    -- * Request Lenses
    , dCacheSubnetGroupName

    -- * Destructuring the Response
    , deleteCacheSubnetGroupResponse
    , DeleteCacheSubnetGroupResponse
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DeleteCacheSubnetGroup@ operation.
--
--
--
-- /See:/ 'deleteCacheSubnetGroup' smart constructor.
newtype DeleteCacheSubnetGroup = DeleteCacheSubnetGroup'
  { _dCacheSubnetGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dCacheSubnetGroupName' - The name of the cache subnet group to delete. Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
deleteCacheSubnetGroup
    :: Text -- ^ 'dCacheSubnetGroupName'
    -> DeleteCacheSubnetGroup
deleteCacheSubnetGroup pCacheSubnetGroupName_ =
  DeleteCacheSubnetGroup' {_dCacheSubnetGroupName = pCacheSubnetGroupName_}


-- | The name of the cache subnet group to delete. Constraints: Must contain no more than 255 alphanumeric characters or hyphens.
dCacheSubnetGroupName :: Lens' DeleteCacheSubnetGroup Text
dCacheSubnetGroupName = lens _dCacheSubnetGroupName (\ s a -> s{_dCacheSubnetGroupName = a})

instance AWSRequest DeleteCacheSubnetGroup where
        type Rs DeleteCacheSubnetGroup =
             DeleteCacheSubnetGroupResponse
        request = postQuery elastiCache
        response
          = receiveNull DeleteCacheSubnetGroupResponse'

instance Hashable DeleteCacheSubnetGroup where

instance NFData DeleteCacheSubnetGroup where

instance ToHeaders DeleteCacheSubnetGroup where
        toHeaders = const mempty

instance ToPath DeleteCacheSubnetGroup where
        toPath = const "/"

instance ToQuery DeleteCacheSubnetGroup where
        toQuery DeleteCacheSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("DeleteCacheSubnetGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSubnetGroupName" =: _dCacheSubnetGroupName]

-- | /See:/ 'deleteCacheSubnetGroupResponse' smart constructor.
data DeleteCacheSubnetGroupResponse =
  DeleteCacheSubnetGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteCacheSubnetGroupResponse' with the minimum fields required to make a request.
--
deleteCacheSubnetGroupResponse
    :: DeleteCacheSubnetGroupResponse
deleteCacheSubnetGroupResponse = DeleteCacheSubnetGroupResponse'


instance NFData DeleteCacheSubnetGroupResponse where
