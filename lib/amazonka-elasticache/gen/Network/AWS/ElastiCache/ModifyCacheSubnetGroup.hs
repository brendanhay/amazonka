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
-- Module      : Network.AWS.ElastiCache.ModifyCacheSubnetGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies an existing cache subnet group.
--
--
module Network.AWS.ElastiCache.ModifyCacheSubnetGroup
    (
    -- * Creating a Request
      modifyCacheSubnetGroup
    , ModifyCacheSubnetGroup
    -- * Request Lenses
    , mcsgSubnetIds
    , mcsgCacheSubnetGroupDescription
    , mcsgCacheSubnetGroupName

    -- * Destructuring the Response
    , modifyCacheSubnetGroupResponse
    , ModifyCacheSubnetGroupResponse
    -- * Response Lenses
    , mcsgrsCacheSubnetGroup
    , mcsgrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @ModifyCacheSubnetGroup@ operation.
--
--
--
-- /See:/ 'modifyCacheSubnetGroup' smart constructor.
data ModifyCacheSubnetGroup = ModifyCacheSubnetGroup'
  { _mcsgSubnetIds                   :: !(Maybe [Text])
  , _mcsgCacheSubnetGroupDescription :: !(Maybe Text)
  , _mcsgCacheSubnetGroupName        :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyCacheSubnetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsgSubnetIds' - The EC2 subnet IDs for the cache subnet group.
--
-- * 'mcsgCacheSubnetGroupDescription' - A description of the cache subnet group.
--
-- * 'mcsgCacheSubnetGroupName' - The name for the cache subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 alphanumeric characters or hyphens. Example: @mysubnetgroup@
modifyCacheSubnetGroup
    :: Text -- ^ 'mcsgCacheSubnetGroupName'
    -> ModifyCacheSubnetGroup
modifyCacheSubnetGroup pCacheSubnetGroupName_ =
  ModifyCacheSubnetGroup'
    { _mcsgSubnetIds = Nothing
    , _mcsgCacheSubnetGroupDescription = Nothing
    , _mcsgCacheSubnetGroupName = pCacheSubnetGroupName_
    }


-- | The EC2 subnet IDs for the cache subnet group.
mcsgSubnetIds :: Lens' ModifyCacheSubnetGroup [Text]
mcsgSubnetIds = lens _mcsgSubnetIds (\ s a -> s{_mcsgSubnetIds = a}) . _Default . _Coerce

-- | A description of the cache subnet group.
mcsgCacheSubnetGroupDescription :: Lens' ModifyCacheSubnetGroup (Maybe Text)
mcsgCacheSubnetGroupDescription = lens _mcsgCacheSubnetGroupDescription (\ s a -> s{_mcsgCacheSubnetGroupDescription = a})

-- | The name for the cache subnet group. This value is stored as a lowercase string. Constraints: Must contain no more than 255 alphanumeric characters or hyphens. Example: @mysubnetgroup@
mcsgCacheSubnetGroupName :: Lens' ModifyCacheSubnetGroup Text
mcsgCacheSubnetGroupName = lens _mcsgCacheSubnetGroupName (\ s a -> s{_mcsgCacheSubnetGroupName = a})

instance AWSRequest ModifyCacheSubnetGroup where
        type Rs ModifyCacheSubnetGroup =
             ModifyCacheSubnetGroupResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "ModifyCacheSubnetGroupResult"
              (\ s h x ->
                 ModifyCacheSubnetGroupResponse' <$>
                   (x .@? "CacheSubnetGroup") <*> (pure (fromEnum s)))

instance Hashable ModifyCacheSubnetGroup where

instance NFData ModifyCacheSubnetGroup where

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
data ModifyCacheSubnetGroupResponse = ModifyCacheSubnetGroupResponse'
  { _mcsgrsCacheSubnetGroup :: !(Maybe CacheSubnetGroup)
  , _mcsgrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyCacheSubnetGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mcsgrsCacheSubnetGroup' - Undocumented member.
--
-- * 'mcsgrsResponseStatus' - -- | The response status code.
modifyCacheSubnetGroupResponse
    :: Int -- ^ 'mcsgrsResponseStatus'
    -> ModifyCacheSubnetGroupResponse
modifyCacheSubnetGroupResponse pResponseStatus_ =
  ModifyCacheSubnetGroupResponse'
    { _mcsgrsCacheSubnetGroup = Nothing
    , _mcsgrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
mcsgrsCacheSubnetGroup :: Lens' ModifyCacheSubnetGroupResponse (Maybe CacheSubnetGroup)
mcsgrsCacheSubnetGroup = lens _mcsgrsCacheSubnetGroup (\ s a -> s{_mcsgrsCacheSubnetGroup = a})

-- | -- | The response status code.
mcsgrsResponseStatus :: Lens' ModifyCacheSubnetGroupResponse Int
mcsgrsResponseStatus = lens _mcsgrsResponseStatus (\ s a -> s{_mcsgrsResponseStatus = a})

instance NFData ModifyCacheSubnetGroupResponse where
