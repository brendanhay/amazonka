{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CreateDBSecurityGroup
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
module Network.AWS.RDS.V2013_09_09.CreateDBSecurityGroup
    (
    -- * Request
      CreateDBSecurityGroup
    -- ** Request constructor
    , mkCreateDBSecurityGroupMessage
    -- ** Request lenses
    , cdbsgmDBSecurityGroupName
    , cdbsgmDBSecurityGroupDescription
    , cdbsgmTags

    -- * Response
    , CreateDBSecurityGroupResponse
    -- ** Response lenses
    , dbsgxDBSecurityGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSecurityGroup' request.
mkCreateDBSecurityGroupMessage :: Text -- ^ 'cdbsgmDBSecurityGroupName'
                               -> Text -- ^ 'cdbsgmDBSecurityGroupDescription'
                               -> CreateDBSecurityGroup
mkCreateDBSecurityGroupMessage p1 p2 = CreateDBSecurityGroup
    { _cdbsgmDBSecurityGroupName = p1
    , _cdbsgmDBSecurityGroupDescription = p2
    , _cdbsgmTags = mempty
    }
{-# INLINE mkCreateDBSecurityGroupMessage #-}

data CreateDBSecurityGroup = CreateDBSecurityGroup
    { _cdbsgmDBSecurityGroupName :: Text
      -- ^ The name for the DB security group. This value is stored as a
      -- lowercase string. Constraints: Must be 1 to 255 alphanumeric
      -- characters First character must be a letter Cannot end with a
      -- hyphen or contain two consecutive hyphens Must not be "Default"
      -- May not contain spaces Example: mysecuritygroup.
    , _cdbsgmDBSecurityGroupDescription :: Text
      -- ^ The description for the DB security group.
    , _cdbsgmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

-- | The name for the DB security group. This value is stored as a lowercase
-- string. Constraints: Must be 1 to 255 alphanumeric characters First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Must not be "Default" May not contain spaces Example:
-- mysecuritygroup.
cdbsgmDBSecurityGroupName :: Lens' CreateDBSecurityGroup (Text)
cdbsgmDBSecurityGroupName = lens _cdbsgmDBSecurityGroupName (\s a -> s { _cdbsgmDBSecurityGroupName = a })
{-# INLINE cdbsgmDBSecurityGroupName #-}

-- | The description for the DB security group.
cdbsgmDBSecurityGroupDescription :: Lens' CreateDBSecurityGroup (Text)
cdbsgmDBSecurityGroupDescription = lens _cdbsgmDBSecurityGroupDescription (\s a -> s { _cdbsgmDBSecurityGroupDescription = a })
{-# INLINE cdbsgmDBSecurityGroupDescription #-}

-- | A list of tags.
cdbsgmTags :: Lens' CreateDBSecurityGroup ([Tag])
cdbsgmTags = lens _cdbsgmTags (\s a -> s { _cdbsgmTags = a })
{-# INLINE cdbsgmTags #-}

instance ToQuery CreateDBSecurityGroup where
    toQuery = genericQuery def

newtype CreateDBSecurityGroupResponse = CreateDBSecurityGroupResponse
    { _dbsgxDBSecurityGroup :: Maybe DBSecurityGroup
      -- ^ Contains the result of a successful invocation of the following
      -- actions: DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
      -- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type
      -- is used as a response element in the DescribeDBSecurityGroups
      -- action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- DescribeDBSecurityGroups AuthorizeDBSecurityGroupIngress
-- CreateDBSecurityGroup RevokeDBSecurityGroupIngress This data type is used
-- as a response element in the DescribeDBSecurityGroups action.
dbsgxDBSecurityGroup :: Lens' CreateDBSecurityGroupResponse (Maybe DBSecurityGroup)
dbsgxDBSecurityGroup = lens _dbsgxDBSecurityGroup (\s a -> s { _dbsgxDBSecurityGroup = a })
{-# INLINE dbsgxDBSecurityGroup #-}

instance FromXML CreateDBSecurityGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBSecurityGroup where
    type Sv CreateDBSecurityGroup = RDS
    type Rs CreateDBSecurityGroup = CreateDBSecurityGroupResponse

    request = post "CreateDBSecurityGroup"
    response _ = xmlResponse
