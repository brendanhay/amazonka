{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteSecurityGroup
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
module Network.AWS.EC2.V2014_06_15.DeleteSecurityGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteSecurityGroup' request.
deleteSecurityGroup :: DeleteSecurityGroup
deleteSecurityGroup = DeleteSecurityGroup
    { _dsgrDryRun = Nothing
    , _dsgrGroupId = Nothing
    , _dsgrGroupName = Nothing
    }

data DeleteSecurityGroup = DeleteSecurityGroup
    { _dsgrDryRun :: Maybe Bool
      -- ^ 
    , _dsgrGroupId :: Maybe Text
      -- ^ The ID of the security group.
    , _dsgrGroupName :: Maybe Text
      -- ^ [EC2-Classic, default VPC] The name of the security group.
    } deriving (Show, Generic)

makeLenses ''DeleteSecurityGroup

instance ToQuery DeleteSecurityGroup where
    toQuery = genericQuery def

data DeleteSecurityGroupResponse = DeleteSecurityGroupResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteSecurityGroupResponse

instance AWSRequest DeleteSecurityGroup where
    type Sv DeleteSecurityGroup = EC2
    type Rs DeleteSecurityGroup = DeleteSecurityGroupResponse

    request = post "DeleteSecurityGroup"
    response _ = nullaryResponse DeleteSecurityGroupResponse
