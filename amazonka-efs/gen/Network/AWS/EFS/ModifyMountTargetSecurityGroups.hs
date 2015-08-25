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
-- Module      : Network.AWS.EFS.ModifyMountTargetSecurityGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the set of security groups in effect for a mount target.
--
-- When you create a mount target, Amazon EFS also creates a new network
-- interface (see CreateMountTarget). This operation replaces the security
-- groups in effect for the network interface associated with a mount
-- target, with the 'SecurityGroups' provided in the request. This
-- operation requires that the network interface of the mount target has
-- been created and the life cycle state of the mount target is not
-- \"deleted\".
--
-- The operation requires permissions for the following actions:
--
-- -   'elasticfilesystem:ModifyMountTargetSecurityGroups' action on the
--     mount target\'s file system.
-- -   'ec2:ModifyNetworkInterfaceAttribute' action on the mount target\'s
--     network interface.
--
-- /See:/ <http://docs.aws.amazon.com/directoryservice/latest/devguide/API_ModifyMountTargetSecurityGroups.html AWS API Reference> for ModifyMountTargetSecurityGroups.
module Network.AWS.EFS.ModifyMountTargetSecurityGroups
    (
    -- * Creating a Request
      modifyMountTargetSecurityGroups
    , ModifyMountTargetSecurityGroups
    -- * Request Lenses
    , mmtsgSecurityGroups
    , mmtsgMountTargetId

    -- * Destructuring the Response
    , modifyMountTargetSecurityGroupsResponse
    , ModifyMountTargetSecurityGroupsResponse
    ) where

import           Network.AWS.EFS.Types
import           Network.AWS.EFS.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'modifyMountTargetSecurityGroups' smart constructor.
data ModifyMountTargetSecurityGroups = ModifyMountTargetSecurityGroups'
    { _mmtsgSecurityGroups :: !(Maybe [Text])
    , _mmtsgMountTargetId  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyMountTargetSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mmtsgSecurityGroups'
--
-- * 'mmtsgMountTargetId'
modifyMountTargetSecurityGroups
    :: Text -- ^ 'mmtsgMountTargetId'
    -> ModifyMountTargetSecurityGroups
modifyMountTargetSecurityGroups pMountTargetId_ =
    ModifyMountTargetSecurityGroups'
    { _mmtsgSecurityGroups = Nothing
    , _mmtsgMountTargetId = pMountTargetId_
    }

-- | An array of up to five VPC security group IDs.
mmtsgSecurityGroups :: Lens' ModifyMountTargetSecurityGroups [Text]
mmtsgSecurityGroups = lens _mmtsgSecurityGroups (\ s a -> s{_mmtsgSecurityGroups = a}) . _Default . _Coerce;

-- | The ID of the mount target whose security groups you want to modify.
mmtsgMountTargetId :: Lens' ModifyMountTargetSecurityGroups Text
mmtsgMountTargetId = lens _mmtsgMountTargetId (\ s a -> s{_mmtsgMountTargetId = a});

instance AWSRequest ModifyMountTargetSecurityGroups
         where
        type Rs ModifyMountTargetSecurityGroups =
             ModifyMountTargetSecurityGroupsResponse
        request = putJSON eFS
        response
          = receiveNull
              ModifyMountTargetSecurityGroupsResponse'

instance ToHeaders ModifyMountTargetSecurityGroups
         where
        toHeaders = const mempty

instance ToJSON ModifyMountTargetSecurityGroups where
        toJSON ModifyMountTargetSecurityGroups'{..}
          = object
              (catMaybes
                 [("SecurityGroups" .=) <$> _mmtsgSecurityGroups])

instance ToPath ModifyMountTargetSecurityGroups where
        toPath ModifyMountTargetSecurityGroups'{..}
          = mconcat
              ["/2015-02-01/mount-targets/",
               toBS _mmtsgMountTargetId, "/security-groups"]

instance ToQuery ModifyMountTargetSecurityGroups
         where
        toQuery = const mempty

-- | /See:/ 'modifyMountTargetSecurityGroupsResponse' smart constructor.
data ModifyMountTargetSecurityGroupsResponse =
    ModifyMountTargetSecurityGroupsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ModifyMountTargetSecurityGroupsResponse' with the minimum fields required to make a request.
--
modifyMountTargetSecurityGroupsResponse
    :: ModifyMountTargetSecurityGroupsResponse
modifyMountTargetSecurityGroupsResponse =
    ModifyMountTargetSecurityGroupsResponse'
