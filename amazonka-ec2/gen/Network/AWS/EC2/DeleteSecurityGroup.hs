{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes a security group.
--
-- If you attempt to delete a security group that is associated with an
-- instance, or is referenced by another security group, the operation fails
-- with 'InvalidGroup.InUse' in EC2-Classic or 'DependencyViolation' in EC2-VPC.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteSecurityGroup.html>
module Network.AWS.EC2.DeleteSecurityGroup
    (
    -- * Request
      DeleteSecurityGroup
    -- ** Request constructor
    , deleteSecurityGroup
    -- ** Request lenses
    , dsgDryRun
    , dsgGroupId
    , dsgGroupName

    -- * Response
    , DeleteSecurityGroupResponse
    -- ** Response constructor
    , deleteSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteSecurityGroup = DeleteSecurityGroup
    { _dsgDryRun    :: Maybe Bool
    , _dsgGroupId   :: Maybe Text
    , _dsgGroupName :: Maybe Text
    } deriving (Eq, Ord, Show)

-- | 'DeleteSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsgGroupId' @::@ 'Maybe' 'Text'
--
-- * 'dsgGroupName' @::@ 'Maybe' 'Text'
--
deleteSecurityGroup :: DeleteSecurityGroup
deleteSecurityGroup = DeleteSecurityGroup
    { _dsgDryRun    = Nothing
    , _dsgGroupName = Nothing
    , _dsgGroupId   = Nothing
    }

dsgDryRun :: Lens' DeleteSecurityGroup (Maybe Bool)
dsgDryRun = lens _dsgDryRun (\s a -> s { _dsgDryRun = a })

-- | The ID of the security group. Required for a nondefault VPC.
dsgGroupId :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupId = lens _dsgGroupId (\s a -> s { _dsgGroupId = a })

-- | [EC2-Classic, default VPC] The name of the security group. You can specify
-- either the security group name or the security group ID.
dsgGroupName :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupName = lens _dsgGroupName (\s a -> s { _dsgGroupName = a })

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteSecurityGroupResponse' constructor.
deleteSecurityGroupResponse :: DeleteSecurityGroupResponse
deleteSecurityGroupResponse = DeleteSecurityGroupResponse

instance ToPath DeleteSecurityGroup where
    toPath = const "/"

instance ToQuery DeleteSecurityGroup where
    toQuery DeleteSecurityGroup{..} = mconcat
        [ "dryRun"    =? _dsgDryRun
        , "GroupId"   =? _dsgGroupId
        , "GroupName" =? _dsgGroupName
        ]

instance ToHeaders DeleteSecurityGroup

instance AWSRequest DeleteSecurityGroup where
    type Sv DeleteSecurityGroup = EC2
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse

    request  = post "DeleteSecurityGroup"
    response = nullResponse DeleteSecurityGroupResponse
