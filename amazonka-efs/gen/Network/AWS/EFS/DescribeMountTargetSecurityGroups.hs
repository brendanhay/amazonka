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
-- Module      : Network.AWS.EFS.DescribeMountTargetSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the security groups currently in effect for a mount target. This
-- operation requires that the network interface of the mount target has
-- been created and the life cycle state of the mount target is not
-- \"deleted\".
--
-- This operation requires permissions for the following actions:
--
-- -   'elasticfilesystem:DescribeMountTargetSecurityGroups' action on the
--     mount target\'s file system.
-- -   'ec2:DescribeNetworkInterfaceAttribute' action on the mount
--     target\'s network interface.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeMountTargetSecurityGroups.html AWS API Reference> for DescribeMountTargetSecurityGroups.
module Network.AWS.EFS.DescribeMountTargetSecurityGroups
    (
    -- * Creating a Request
      describeMountTargetSecurityGroups
    , DescribeMountTargetSecurityGroups
    -- * Request Lenses
    , dmtsgMountTargetId

    -- * Destructuring the Response
    , describeMountTargetSecurityGroupsResponse
    , DescribeMountTargetSecurityGroupsResponse
    -- * Response Lenses
    , dmtsgrsStatus
    , dmtsgrsSecurityGroups
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.EFS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMountTargetSecurityGroups' smart constructor.
newtype DescribeMountTargetSecurityGroups = DescribeMountTargetSecurityGroups'
    { _dmtsgMountTargetId :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMountTargetSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmtsgMountTargetId'
describeMountTargetSecurityGroups
    :: Text -- ^ 'dmtsgMountTargetId'
    -> DescribeMountTargetSecurityGroups
describeMountTargetSecurityGroups pMountTargetId_ =
    DescribeMountTargetSecurityGroups'
    { _dmtsgMountTargetId = pMountTargetId_
    }

-- | The ID of the mount target whose security groups you want to retrieve.
dmtsgMountTargetId :: Lens' DescribeMountTargetSecurityGroups Text
dmtsgMountTargetId = lens _dmtsgMountTargetId (\ s a -> s{_dmtsgMountTargetId = a});

instance AWSRequest DescribeMountTargetSecurityGroups
         where
        type Rs DescribeMountTargetSecurityGroups =
             DescribeMountTargetSecurityGroupsResponse
        request = get eFS
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMountTargetSecurityGroupsResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .?> "SecurityGroups" .!@ mempty))

instance ToHeaders DescribeMountTargetSecurityGroups
         where
        toHeaders = const mempty

instance ToPath DescribeMountTargetSecurityGroups
         where
        toPath DescribeMountTargetSecurityGroups'{..}
          = mconcat
              ["/2015-02-01/mount-targets/",
               toBS _dmtsgMountTargetId, "/security-groups"]

instance ToQuery DescribeMountTargetSecurityGroups
         where
        toQuery = const mempty

-- | /See:/ 'describeMountTargetSecurityGroupsResponse' smart constructor.
data DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse'
    { _dmtsgrsStatus         :: !Int
    , _dmtsgrsSecurityGroups :: ![Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DescribeMountTargetSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmtsgrsStatus'
--
-- * 'dmtsgrsSecurityGroups'
describeMountTargetSecurityGroupsResponse
    :: Int -- ^ 'dmtsgrsStatus'
    -> DescribeMountTargetSecurityGroupsResponse
describeMountTargetSecurityGroupsResponse pStatus_ =
    DescribeMountTargetSecurityGroupsResponse'
    { _dmtsgrsStatus = pStatus_
    , _dmtsgrsSecurityGroups = mempty
    }

-- | The response status code.
dmtsgrsStatus :: Lens' DescribeMountTargetSecurityGroupsResponse Int
dmtsgrsStatus = lens _dmtsgrsStatus (\ s a -> s{_dmtsgrsStatus = a});

-- | An array of security groups.
dmtsgrsSecurityGroups :: Lens' DescribeMountTargetSecurityGroupsResponse [Text]
dmtsgrsSecurityGroups = lens _dmtsgrsSecurityGroups (\ s a -> s{_dmtsgrsSecurityGroups = a}) . _Coerce;
