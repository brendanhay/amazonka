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
-- Module      : Network.AWS.ElastiCache.CreateCacheSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new cache subnet group.
--
--
-- Use this parameter only when you are creating a cluster in an Amazon Virtual Private Cloud (Amazon VPC).
--
module Network.AWS.ElastiCache.CreateCacheSubnetGroup
    (
    -- * Creating a Request
      createCacheSubnetGroup
    , CreateCacheSubnetGroup
    -- * Request Lenses
    , ccsgCacheSubnetGroupName
    , ccsgCacheSubnetGroupDescription
    , ccsgSubnetIds

    -- * Destructuring the Response
    , createCacheSubnetGroupResponse
    , CreateCacheSubnetGroupResponse
    -- * Response Lenses
    , crsCacheSubnetGroup
    , crsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @CreateCacheSubnetGroup@ operation.
--
--
--
-- /See:/ 'createCacheSubnetGroup' smart constructor.
data CreateCacheSubnetGroup = CreateCacheSubnetGroup'
  { _ccsgCacheSubnetGroupName        :: !Text
  , _ccsgCacheSubnetGroupDescription :: !Text
  , _ccsgSubnetIds                   :: ![Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ccsgCacheSubnetGroupName' - A name for the cache subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 alphanumeric characters or hyphens. Example: @mysubnetgroup@
--
-- * 'ccsgCacheSubnetGroupDescription' - A description for the cache subnet group.
--
-- * 'ccsgSubnetIds' - A list of VPC subnet IDs for the cache subnet group.
createCacheSubnetGroup
    :: Text -- ^ 'ccsgCacheSubnetGroupName'
    -> Text -- ^ 'ccsgCacheSubnetGroupDescription'
    -> CreateCacheSubnetGroup
createCacheSubnetGroup pCacheSubnetGroupName_ pCacheSubnetGroupDescription_ =
  CreateCacheSubnetGroup'
    { _ccsgCacheSubnetGroupName = pCacheSubnetGroupName_
    , _ccsgCacheSubnetGroupDescription = pCacheSubnetGroupDescription_
    , _ccsgSubnetIds = mempty
    }


-- | A name for the cache subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 alphanumeric characters or hyphens. Example: @mysubnetgroup@
ccsgCacheSubnetGroupName :: Lens' CreateCacheSubnetGroup Text
ccsgCacheSubnetGroupName = lens _ccsgCacheSubnetGroupName (\ s a -> s{_ccsgCacheSubnetGroupName = a})

-- | A description for the cache subnet group.
ccsgCacheSubnetGroupDescription :: Lens' CreateCacheSubnetGroup Text
ccsgCacheSubnetGroupDescription = lens _ccsgCacheSubnetGroupDescription (\ s a -> s{_ccsgCacheSubnetGroupDescription = a})

-- | A list of VPC subnet IDs for the cache subnet group.
ccsgSubnetIds :: Lens' CreateCacheSubnetGroup [Text]
ccsgSubnetIds = lens _ccsgSubnetIds (\ s a -> s{_ccsgSubnetIds = a}) . _Coerce

instance AWSRequest CreateCacheSubnetGroup where
        type Rs CreateCacheSubnetGroup =
             CreateCacheSubnetGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "CreateCacheSubnetGroupResult"
              (\ s h x ->
                 CreateCacheSubnetGroupResponse' <$>
                   (x .@? "CacheSubnetGroup") <*> (pure (fromEnum s)))

instance Hashable CreateCacheSubnetGroup where

instance NFData CreateCacheSubnetGroup where

instance ToHeaders CreateCacheSubnetGroup where
        toHeaders = const mempty

instance ToPath CreateCacheSubnetGroup where
        toPath = const "/"

instance ToQuery CreateCacheSubnetGroup where
        toQuery CreateCacheSubnetGroup'{..}
          = mconcat
              ["Action" =:
                 ("CreateCacheSubnetGroup" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSubnetGroupName" =: _ccsgCacheSubnetGroupName,
               "CacheSubnetGroupDescription" =:
                 _ccsgCacheSubnetGroupDescription,
               "SubnetIds" =:
                 toQueryList "SubnetIdentifier" _ccsgSubnetIds]

-- | /See:/ 'createCacheSubnetGroupResponse' smart constructor.
data CreateCacheSubnetGroupResponse = CreateCacheSubnetGroupResponse'
  { _crsCacheSubnetGroup :: !(Maybe CacheSubnetGroup)
  , _crsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateCacheSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsCacheSubnetGroup' - Undocumented member.
--
-- * 'crsResponseStatus' - -- | The response status code.
createCacheSubnetGroupResponse
    :: Int -- ^ 'crsResponseStatus'
    -> CreateCacheSubnetGroupResponse
createCacheSubnetGroupResponse pResponseStatus_ =
  CreateCacheSubnetGroupResponse'
    {_crsCacheSubnetGroup = Nothing, _crsResponseStatus = pResponseStatus_}


-- | Undocumented member.
crsCacheSubnetGroup :: Lens' CreateCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
crsCacheSubnetGroup = lens _crsCacheSubnetGroup (\ s a -> s{_crsCacheSubnetGroup = a})

-- | -- | The response status code.
crsResponseStatus :: Lens' CreateCacheSubnetGroupResponse Int
crsResponseStatus = lens _crsResponseStatus (\ s a -> s{_crsResponseStatus = a})

instance NFData CreateCacheSubnetGroupResponse where
