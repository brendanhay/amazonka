{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB security group. DB security groups control access to a DB
-- instance. https://rds.amazonaws.com/ ?Action=CreateDBSecurityGroup
-- &DBSecurityGroupName=mydbsecuritygroup
-- &DBSecurityGroupDescription=My%20new%20DBSecurityGroup
-- &EC2VpcId=vpc-1a2b3c4d &Version=2013-05-15 &SignatureVersion=2
-- &SignatureMethod=HmacSHA256 &Timestamp=2011-02-15T18%3A14%3A49.482Z
-- &AWSAccessKeyId= &Signature= My new DBSecurityGroup 565419523791
-- mydbsecuritygroup vpc-1a2b3c4d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.CreateDBSecurityGroup
    (
    -- * Request
      CreateDBSecurityGroup
    -- ** Request constructor
    , createDBSecurityGroup
    -- ** Request lenses
    , cdbsgDBSecurityGroupName
    , cdbsgDBSecurityGroupDescription
    , cdbsgTags

    -- * Response
    , CreateDBSecurityGroupResponse
    -- ** Response constructor
    , createDBSecurityGroupResponse
    -- ** Response lenses
    , cdbsgrDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CreateDBSecurityGroup = CreateDBSecurityGroup
    { _cdbsgDBSecurityGroupName :: Text
    , _cdbsgDBSecurityGroupDescription :: Text
    , _cdbsgTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSecurityGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroupName ::@ @Text@
--
-- * @DBSecurityGroupDescription ::@ @Text@
--
-- * @Tags ::@ @[Tag]@
--
createDBSecurityGroup :: Text -- ^ 'cdbsgDBSecurityGroupName'
                        -> Text -- ^ 'cdbsgDBSecurityGroupDescription'
                        -> CreateDBSecurityGroup
createDBSecurityGroup p1 p2 = CreateDBSecurityGroup
    { _cdbsgDBSecurityGroupName = p1
    , _cdbsgDBSecurityGroupDescription = p2
    , _cdbsgTags = mempty
    }

-- | The name for the DB security group. This value is stored as a lowercase
-- string. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Must not be "Default" May not contain spaces Example:
-- mysecuritygroup.
cdbsgDBSecurityGroupName :: Lens' CreateDBSecurityGroup Text
cdbsgDBSecurityGroupName =
    lens _cdbsgDBSecurityGroupName
         (\s a -> s { _cdbsgDBSecurityGroupName = a })

-- | The description for the DB security group.
cdbsgDBSecurityGroupDescription :: Lens' CreateDBSecurityGroup Text
cdbsgDBSecurityGroupDescription =
    lens _cdbsgDBSecurityGroupDescription
         (\s a -> s { _cdbsgDBSecurityGroupDescription = a })

-- | A list of tags.
cdbsgTags :: Lens' CreateDBSecurityGroup [Tag]
cdbsgTags = lens _cdbsgTags (\s a -> s { _cdbsgTags = a })

instance ToQuery CreateDBSecurityGroup where
    toQuery = genericQuery def

newtype CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse
    { _cdbsgrDBSecurityGroup :: Maybe DBSecurityGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSecurityGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSecurityGroup ::@ @Maybe DBSecurityGroup@
--
createDBSecurityGroupResponse :: CreateDBSecurityGroupResponse
createDBSecurityGroupResponse = CreateDBSecurityGroupResponse
    { _cdbsgrDBSecurityGroup = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
cdbsgrDBSecurityGroup :: Lens' CreateDBSecurityGroupResponse (Maybe DBSecurityGroup)
cdbsgrDBSecurityGroup =
    lens _cdbsgrDBSecurityGroup (\s a -> s { _cdbsgrDBSecurityGroup = a })

instance FromXML CreateDBSecurityGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBSecurityGroup where
    type Sv CreateDBSecurityGroup = RDS
    type Rs CreateDBSecurityGroup = CreateDBSecurityGroupResponse

    request = post "CreateDBSecurityGroup"
    response _ = xmlResponse
