{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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

-- | Deletes a security group. If you attempt to delete a security group that is
-- associated with an instance, or is referenced by another security group,
-- the operation fails with InvalidGroup.InUse in EC2-Classic or
-- DependencyViolation in EC2-VPC. Example for EC2-Classic This example
-- deletes the specified security group for EC2-Classic.
-- https://ec2.amazonaws.com/?Action=DeleteSecurityGroup &amp;GroupName=websrv
-- &amp;AUTHPARAMS &lt;DeleteSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSecurityGroupResponse&gt;
-- Example for EC2-VPC his example deletes the specified security group for
-- EC2-VPC. https://ec2.amazonaws.com/?Action=DeleteSecurityGroup
-- &amp;GroupId=sg-1a2b3c4d &amp;AUTHPARAMS &lt;DeleteSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSecurityGroupResponse&gt;.
module Network.AWS.EC2
    (
    -- * Request
      DeleteSecurityGroup
    -- ** Request constructor
    , mkDeleteSecurityGroup
    -- ** Request lenses
    , dsgGroupName
    , dsgGroupId

    -- * Response
    , DeleteSecurityGroupResponse
    -- ** Response constructor
    , mkDeleteSecurityGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data DeleteSecurityGroup = DeleteSecurityGroup
    { _dsgGroupName :: !(Maybe Text)
    , _dsgGroupId :: !(Maybe Text)
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSecurityGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Maybe Text@
--
-- * @GroupId ::@ @Maybe Text@
--
mkDeleteSecurityGroup :: DeleteSecurityGroup
mkDeleteSecurityGroup = DeleteSecurityGroup
    { _dsgGroupName = Nothing
    , _dsgGroupId = Nothing
    }

-- | [EC2-Classic, default VPC] The name of the security group.
dsgGroupName :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupName = lens _dsgGroupName (\s a -> s { _dsgGroupName = a })

-- | The ID of the security group.
dsgGroupId :: Lens' DeleteSecurityGroup (Maybe Text)
dsgGroupId = lens _dsgGroupId (\s a -> s { _dsgGroupId = a })

instance ToQuery DeleteSecurityGroup where
    toQuery = genericQuery def

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteSecurityGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteSecurityGroupResponse :: DeleteSecurityGroupResponse
mkDeleteSecurityGroupResponse = DeleteSecurityGroupResponse

instance AWSRequest DeleteSecurityGroup where
    type Sv DeleteSecurityGroup = EC2
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse

    request = post "DeleteSecurityGroup"
    response _ = nullaryResponse DeleteSecurityGroupResponse
