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
-- Module      : Network.AWS.EMR.ModifyInstanceGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- ModifyInstanceGroups modifies the number of nodes and configuration settings of an instance group. The input parameters include the new target instance count for the group and the instance group ID. The call will either succeed or fail atomically.
--
--
module Network.AWS.EMR.ModifyInstanceGroups
    (
    -- * Creating a Request
      modifyInstanceGroups
    , ModifyInstanceGroups
    -- * Request Lenses
    , migClusterId
    , migInstanceGroups

    -- * Destructuring the Response
    , modifyInstanceGroupsResponse
    , ModifyInstanceGroupsResponse
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Change the size of some instance groups.
--
--
--
-- /See:/ 'modifyInstanceGroups' smart constructor.
data ModifyInstanceGroups = ModifyInstanceGroups'
  { _migClusterId      :: !(Maybe Text)
  , _migInstanceGroups :: !(Maybe [InstanceGroupModifyConfig])
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'migClusterId' - The ID of the cluster to which the instance group belongs.
--
-- * 'migInstanceGroups' - Instance groups to change.
modifyInstanceGroups
    :: ModifyInstanceGroups
modifyInstanceGroups =
  ModifyInstanceGroups' {_migClusterId = Nothing, _migInstanceGroups = Nothing}


-- | The ID of the cluster to which the instance group belongs.
migClusterId :: Lens' ModifyInstanceGroups (Maybe Text)
migClusterId = lens _migClusterId (\ s a -> s{_migClusterId = a})

-- | Instance groups to change.
migInstanceGroups :: Lens' ModifyInstanceGroups [InstanceGroupModifyConfig]
migInstanceGroups = lens _migInstanceGroups (\ s a -> s{_migInstanceGroups = a}) . _Default . _Coerce

instance AWSRequest ModifyInstanceGroups where
        type Rs ModifyInstanceGroups =
             ModifyInstanceGroupsResponse
        request = postJSON emr
        response = receiveNull ModifyInstanceGroupsResponse'

instance Hashable ModifyInstanceGroups where

instance NFData ModifyInstanceGroups where

instance ToHeaders ModifyInstanceGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ModifyInstanceGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ModifyInstanceGroups where
        toJSON ModifyInstanceGroups'{..}
          = object
              (catMaybes
                 [("ClusterId" .=) <$> _migClusterId,
                  ("InstanceGroups" .=) <$> _migInstanceGroups])

instance ToPath ModifyInstanceGroups where
        toPath = const "/"

instance ToQuery ModifyInstanceGroups where
        toQuery = const mempty

-- | /See:/ 'modifyInstanceGroupsResponse' smart constructor.
data ModifyInstanceGroupsResponse =
  ModifyInstanceGroupsResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyInstanceGroupsResponse' with the minimum fields required to make a request.
--
modifyInstanceGroupsResponse
    :: ModifyInstanceGroupsResponse
modifyInstanceGroupsResponse = ModifyInstanceGroupsResponse'


instance NFData ModifyInstanceGroupsResponse where
