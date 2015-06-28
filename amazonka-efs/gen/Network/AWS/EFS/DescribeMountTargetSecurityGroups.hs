{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EFS.DescribeMountTargetSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the security groups currently in effect for a mount target. This
-- operation requires that the network interface of the mount target has
-- been created and the life cycle state of the mount target is not
-- \"deleted\".
--
-- This operation requires permissions for the following actions:
--
-- -   @elasticfilesystem:DescribeMountTargetSecurityGroups@ action on the
--     mount target\'s file system.
-- -   @ec2:DescribeNetworkInterfaceAttribute@ action on the mount
--     target\'s network interface.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_DescribeMountTargetSecurityGroups.html>
module Network.AWS.EFS.DescribeMountTargetSecurityGroups
    (
    -- * Request
      DescribeMountTargetSecurityGroups
    -- ** Request constructor
    , describeMountTargetSecurityGroups
    -- ** Request lenses
    , dmtsgMountTargetId

    -- * Response
    , DescribeMountTargetSecurityGroupsResponse
    -- ** Response constructor
    , describeMountTargetSecurityGroupsResponse
    -- ** Response lenses
    , dmtsgrSecurityGroups
    , dmtsgrStatus
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeMountTargetSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmtsgMountTargetId'
newtype DescribeMountTargetSecurityGroups = DescribeMountTargetSecurityGroups'
    { _dmtsgMountTargetId :: Text
    } deriving (Eq,Read,Show)

-- | 'DescribeMountTargetSecurityGroups' smart constructor.
describeMountTargetSecurityGroups :: Text -> DescribeMountTargetSecurityGroups
describeMountTargetSecurityGroups pMountTargetId =
    DescribeMountTargetSecurityGroups'
    { _dmtsgMountTargetId = pMountTargetId
    }

-- | The ID of the mount target whose security groups you want to retrieve.
dmtsgMountTargetId :: Lens' DescribeMountTargetSecurityGroups Text
dmtsgMountTargetId = lens _dmtsgMountTargetId (\ s a -> s{_dmtsgMountTargetId = a});

instance AWSRequest DescribeMountTargetSecurityGroups
         where
        type Sv DescribeMountTargetSecurityGroups = EFS
        type Rs DescribeMountTargetSecurityGroups =
             DescribeMountTargetSecurityGroupsResponse
        request = get
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMountTargetSecurityGroupsResponse' <$>
                   (x .?> "SecurityGroups" .!@ mempty) <*> (pure s))

instance ToHeaders DescribeMountTargetSecurityGroups
         where
        toHeaders = const mempty

instance ToPath DescribeMountTargetSecurityGroups
         where
        toPath DescribeMountTargetSecurityGroups'{..}
          = mconcat
              ["/2015-02-01/mount-targets/",
               toText _dmtsgMountTargetId, "/security-groups"]

instance ToQuery DescribeMountTargetSecurityGroups
         where
        toQuery = const mempty

-- | /See:/ 'describeMountTargetSecurityGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmtsgrSecurityGroups'
--
-- * 'dmtsgrStatus'
data DescribeMountTargetSecurityGroupsResponse = DescribeMountTargetSecurityGroupsResponse'
    { _dmtsgrSecurityGroups :: ![Text]
    , _dmtsgrStatus         :: !Status
    } deriving (Eq,Read,Show)

-- | 'DescribeMountTargetSecurityGroupsResponse' smart constructor.
describeMountTargetSecurityGroupsResponse :: Status -> DescribeMountTargetSecurityGroupsResponse
describeMountTargetSecurityGroupsResponse pStatus =
    DescribeMountTargetSecurityGroupsResponse'
    { _dmtsgrSecurityGroups = mempty
    , _dmtsgrStatus = pStatus
    }

-- | An array of security groups.
dmtsgrSecurityGroups :: Lens' DescribeMountTargetSecurityGroupsResponse [Text]
dmtsgrSecurityGroups = lens _dmtsgrSecurityGroups (\ s a -> s{_dmtsgrSecurityGroups = a});

-- | FIXME: Undocumented member.
dmtsgrStatus :: Lens' DescribeMountTargetSecurityGroupsResponse Status
dmtsgrStatus = lens _dmtsgrStatus (\ s a -> s{_dmtsgrStatus = a});
