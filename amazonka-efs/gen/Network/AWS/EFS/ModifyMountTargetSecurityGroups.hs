{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.ModifyMountTargetSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Modifies the set of security groups in effect for a mount target.
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
module Network.AWS.EFS.ModifyMountTargetSecurityGroups
    (
    -- * Request
      ModifyMountTargetSecurityGroups
    -- ** Request constructor
    , modifyMountTargetSecurityGroups
    -- ** Request lenses
    , mmtsgrqSecurityGroups
    , mmtsgrqMountTargetId

    -- * Response
    , ModifyMountTargetSecurityGroupsResponse
    -- ** Response constructor
    , modifyMountTargetSecurityGroupsResponse
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyMountTargetSecurityGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mmtsgrqSecurityGroups'
--
-- * 'mmtsgrqMountTargetId'
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'
    { _mmtsgrqSecurityGroups :: !(Maybe [Text])
    , _mmtsgrqMountTargetId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyMountTargetSecurityGroups' smart constructor.
modifyMountTargetSecurityGroups :: Text -> ModifyMountTargetSecurityGroups
modifyMountTargetSecurityGroups pMountTargetId_ =
    ModifyMountTargetSecurityGroups'
    { _mmtsgrqSecurityGroups = Nothing
    , _mmtsgrqMountTargetId = pMountTargetId_
    }

-- | An array of up to five VPC security group IDs.
mmtsgrqSecurityGroups :: Lens' ModifyMountTargetSecurityGroups [Text]
mmtsgrqSecurityGroups = lens _mmtsgrqSecurityGroups (\ s a -> s{_mmtsgrqSecurityGroups = a}) . _Default;

-- | The ID of the mount target whose security groups you want to modify.
mmtsgrqMountTargetId :: Lens' ModifyMountTargetSecurityGroups Text
mmtsgrqMountTargetId = lens _mmtsgrqMountTargetId (\ s a -> s{_mmtsgrqMountTargetId = a});

instance AWSRequest ModifyMountTargetSecurityGroups
         where
        type Sv ModifyMountTargetSecurityGroups = EFS
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
          = object ["SecurityGroups" .= _mmtsgrqSecurityGroups]

instance ToPath ModifyMountTargetSecurityGroups where
        toPath ModifyMountTargetSecurityGroups'{..}
          = mconcat
              ["/2015-02-01/mount-targets/",
               toText _mmtsgrqMountTargetId, "/security-groups"]

instance ToQuery ModifyMountTargetSecurityGroups
         where
        toQuery = const mempty

-- | /See:/ 'modifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse =
    ModifyMountTargetSecurityGroupsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ModifyMountTargetSecurityGroupsResponse' smart constructor.
modifyMountTargetSecurityGroupsResponse :: ModifyMountTargetSecurityGroupsResponse
modifyMountTargetSecurityGroupsResponse =
    ModifyMountTargetSecurityGroupsResponse'
