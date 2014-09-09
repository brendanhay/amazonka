{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a security group. A security group is for use with instances either
-- in the EC2-Classic platform or in a specific VPC. For more information, see
-- Amazon EC2 Security Groups in the Amazon Elastic Compute Cloud User Guide
-- and Security Groups for Your VPC in the Amazon Virtual Private Cloud User
-- Guide. EC2-Classic: You can have up to 500 security groups. EC2-VPC: You
-- can create up to 100 security groups per VPC. When you create a security
-- group, you specify a friendly name of your choice. You can have a security
-- group for use in EC2-Classic with the same name as a security group for use
-- in a VPC. However, you can't have two security groups for use in
-- EC2-Classic with the same name or two security groups for use in a VPC with
-- the same name. You have a default security group for use in EC2-Classic and
-- a default security group for use in your VPC. If you don't specify a
-- security group when you launch an instance, the instance is launched into
-- the appropriate default security group. A default security group includes a
-- default rule that grants instances unrestricted network access to each
-- other. You can add or remove rules from your security groups using
-- AuthorizeSecurityGroupIngress, AuthorizeSecurityGroupEgress,
-- RevokeSecurityGroupIngress, and RevokeSecurityGroupEgress. Example for
-- EC2-Classic This example creates a security group named websrv for
-- EC2-Classic. https://ec2.amazonaws.com/?Action=CreateSecurityGroup
-- &amp;GroupName=websrv &amp;GroupDescription=Web Servers &amp;AUTHPARAMS
-- &lt;CreateSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;groupId&gt;sg-1a2b3c4d&lt;/groupId&gt;
-- &lt;/CreateSecurityGroupResponse&gt; Example for EC2-VPC This example
-- creates a security group named WebServerSG for the specified VPC.
-- https://ec2.amazonaws.com/?Action=CreateSecurityGroup
-- &amp;GroupName=WebServerSG &amp;GroupDescription=Web Servers
-- &amp;VpcId=vpc-3325caf2 &amp;AUTHPARAMS &lt;CreateSecurityGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;groupId&gt;sg-0a42d66a&lt;/groupId&gt;
-- &lt;/CreateSecurityGroupResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreateSecurityGroup
    (
    -- * Request
      CreateSecurityGroup
    -- ** Request constructor
    , mkCreateSecurityGroup
    -- ** Request lenses
    , csgGroupName
    , csgDescription
    , csgVpcId

    -- * Response
    , CreateSecurityGroupResponse
    -- ** Response constructor
    , mkCreateSecurityGroupResponse
    -- ** Response lenses
    , csgrGroupId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data CreateSecurityGroup = CreateSecurityGroup
    { _csgGroupName :: Text
    , _csgDescription :: Text
    , _csgVpcId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSecurityGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @Description ::@ @Text@
--
-- * @VpcId ::@ @Maybe Text@
--
mkCreateSecurityGroup :: Text -- ^ 'csgGroupName'
                      -> Text -- ^ 'csgDescription'
                      -> CreateSecurityGroup
mkCreateSecurityGroup p1 p2 = CreateSecurityGroup
    { _csgGroupName = p1
    , _csgDescription = p2
    , _csgVpcId = Nothing
    }

-- | The name of the security group. Constraints: Up to 255 characters in length
-- Constraints for EC2-Classic: ASCII characters Constraints for EC2-VPC: a-z,
-- A-Z, 0-9, spaces, and ._-:/()#,@[]+=&amp;;{}!$*.
csgGroupName :: Lens' CreateSecurityGroup Text
csgGroupName = lens _csgGroupName (\s a -> s { _csgGroupName = a })

-- | A description for the security group. This is informational only.
csgDescription :: Lens' CreateSecurityGroup Text
csgDescription = lens _csgDescription (\s a -> s { _csgDescription = a })

-- | [EC2-VPC] The ID of the VPC.
csgVpcId :: Lens' CreateSecurityGroup (Maybe Text)
csgVpcId = lens _csgVpcId (\s a -> s { _csgVpcId = a })

instance ToQuery CreateSecurityGroup where
    toQuery = genericQuery def

newtype CreateSecurityGroupResponse = CreateSecurityGroupResponse
    { _csgrGroupId :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateSecurityGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupId ::@ @Maybe Text@
--
mkCreateSecurityGroupResponse :: CreateSecurityGroupResponse
mkCreateSecurityGroupResponse = CreateSecurityGroupResponse
    { _csgrGroupId = Nothing
    }

-- | The ID of the security group.
csgrGroupId :: Lens' CreateSecurityGroupResponse (Maybe Text)
csgrGroupId = lens _csgrGroupId (\s a -> s { _csgrGroupId = a })

instance FromXML CreateSecurityGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateSecurityGroup where
    type Sv CreateSecurityGroup = EC2
    type Rs CreateSecurityGroup = CreateSecurityGroupResponse

    request = post "CreateSecurityGroup"
    response _ = xmlResponse
