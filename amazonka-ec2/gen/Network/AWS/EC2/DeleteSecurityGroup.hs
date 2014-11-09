{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DeleteSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a security group. If you attempt to delete a security group that is
-- associated with an instance, or is referenced by another security group,
-- the operation fails with InvalidGroup.InUse in EC2-Classic or
-- DependencyViolation in EC2-VPC.
module Network.AWS.EC2.DeleteSecurityGroup
    (
    -- * Request
      DeleteSecurityGroup
    -- ** Request constructor
    , deleteSecurityGroup
    -- ** Request lenses
    , dsg1DryRun
    , dsg1GroupId
    , dsg1GroupName

    -- * Response
    , DeleteSecurityGroupResponse
    -- ** Response constructor
    , deleteSecurityGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteSecurityGroup = DeleteSecurityGroup
    { _dsg1DryRun    :: Maybe Bool
    , _dsg1GroupId   :: Maybe Text
    , _dsg1GroupName :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteSecurityGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsg1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dsg1GroupId' @::@ 'Maybe' 'Text'
--
-- * 'dsg1GroupName' @::@ 'Maybe' 'Text'
--
deleteSecurityGroup :: DeleteSecurityGroup
deleteSecurityGroup = DeleteSecurityGroup
    { _dsg1DryRun    = Nothing
    , _dsg1GroupName = Nothing
    , _dsg1GroupId   = Nothing
    }

dsg1DryRun :: Lens' DeleteSecurityGroup (Maybe Bool)
dsg1DryRun = lens _dsg1DryRun (\s a -> s { _dsg1DryRun = a })

-- | The ID of the security group. Required for a nondefault VPC.
dsg1GroupId :: Lens' DeleteSecurityGroup (Maybe Text)
dsg1GroupId = lens _dsg1GroupId (\s a -> s { _dsg1GroupId = a })

-- | [EC2-Classic, default VPC] The name of the security group. You can
-- specify either the security group name or the security group ID.
dsg1GroupName :: Lens' DeleteSecurityGroup (Maybe Text)
dsg1GroupName = lens _dsg1GroupName (\s a -> s { _dsg1GroupName = a })

instance ToPath DeleteSecurityGroup where
    toPath = const "/"

instance ToQuery DeleteSecurityGroup

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse

-- | 'DeleteSecurityGroupResponse' constructor.
deleteSecurityGroupResponse :: DeleteSecurityGroupResponse
deleteSecurityGroupResponse = DeleteSecurityGroupResponse

instance AWSRequest DeleteSecurityGroup where
    type Sv DeleteSecurityGroup = EC2
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse

    request  = post "DeleteSecurityGroup"
    response = const (nullaryResponse DeleteSecurityGroupResponse)
