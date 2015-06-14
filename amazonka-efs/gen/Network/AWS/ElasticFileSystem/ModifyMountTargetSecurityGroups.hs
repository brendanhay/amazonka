{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.ElasticFileSystem.ModifyMountTargetSecurityGroups
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

-- | Modifies the set of security groups in effect for a mount target.
--
-- When you create a mount target, Amazon EFS also creates a new network
-- interface (see CreateMountTarget). This operation replaces the security
-- groups in effect for the network interface associated with a mount
-- target, with the @SecurityGroups@ provided in the request. This
-- operation requires that the network interface of the mount target has
-- been created and the life cycle state of the mount target is not
-- \"deleted\".
--
-- The operation requires permissions for the following actions:
--
-- -   @elasticfilesystem:ModifyMountTargetSecurityGroups@ action on the
--     mount target\'s file system.
-- -   @ec2:ModifyNetworkInterfaceAttribute@ action on the mount target\'s
--     network interface.
--
-- <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_ModifyMountTargetSecurityGroups.html>
module Network.AWS.ElasticFileSystem.ModifyMountTargetSecurityGroups
    (
    -- * Request
      ModifyMountTargetSecurityGroups
    -- ** Request constructor
    , modifyMountTargetSecurityGroups
    -- ** Request lenses
    , mmtsgSecurityGroups
    , mmtsgMountTargetId

    -- * Response
    , ModifyMountTargetSecurityGroupsResponse
    -- ** Response constructor
    , modifyMountTargetSecurityGroupsResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.ElasticFileSystem.Types

-- | /See:/ 'modifyMountTargetSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mmtsgSecurityGroups'
--
-- * 'mmtsgMountTargetId'
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'{_mmtsgSecurityGroups :: Maybe [Text], _mmtsgMountTargetId :: Text} deriving (Eq, Read, Show)

-- | 'ModifyMountTargetSecurityGroups' smart constructor.
modifyMountTargetSecurityGroups :: Text -> ModifyMountTargetSecurityGroups
modifyMountTargetSecurityGroups pMountTargetId = ModifyMountTargetSecurityGroups'{_mmtsgSecurityGroups = Nothing, _mmtsgMountTargetId = pMountTargetId};

-- | An array of up to five VPC security group IDs.
mmtsgSecurityGroups :: Lens' ModifyMountTargetSecurityGroups (Maybe [Text])
mmtsgSecurityGroups = lens _mmtsgSecurityGroups (\ s a -> s{_mmtsgSecurityGroups = a});

-- | The ID of the mount target whose security groups you want to modify.
mmtsgMountTargetId :: Lens' ModifyMountTargetSecurityGroups Text
mmtsgMountTargetId = lens _mmtsgMountTargetId (\ s a -> s{_mmtsgMountTargetId = a});

instance AWSRequest ModifyMountTargetSecurityGroups
         where
        type Sv ModifyMountTargetSecurityGroups =
             ElasticFileSystem
        type Rs ModifyMountTargetSecurityGroups =
             ModifyMountTargetSecurityGroupsResponse
        request = putJSON
        response
          = receiveNull
              ModifyMountTargetSecurityGroupsResponse'

instance ToHeaders ModifyMountTargetSecurityGroups
         where
        toHeaders = const mempty

instance ToJSON ModifyMountTargetSecurityGroups where
        toJSON ModifyMountTargetSecurityGroups'{..}
          = object ["SecurityGroups" .= _mmtsgSecurityGroups]

instance ToPath ModifyMountTargetSecurityGroups where
        toPath ModifyMountTargetSecurityGroups'{..}
          = mconcat
              ["/2015-02-01/mount-targets/",
               toText _mmtsgMountTargetId, "/security-groups"]

instance ToQuery ModifyMountTargetSecurityGroups
         where
        toQuery = const mempty

-- | /See:/ 'modifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse = ModifyMountTargetSecurityGroupsResponse' deriving (Eq, Read, Show)

-- | 'ModifyMountTargetSecurityGroupsResponse' smart constructor.
modifyMountTargetSecurityGroupsResponse :: ModifyMountTargetSecurityGroupsResponse
modifyMountTargetSecurityGroupsResponse = ModifyMountTargetSecurityGroupsResponse';
