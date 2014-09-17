{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.CreateDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new DB subnet group. DB subnet groups must contain at least one
-- subnet in at least two AZs in the region. https://rds.amazonaws.com/
-- ?Action=CreateDBSubnetGroup &DBSubnetGroupName=mydbsubnetgroup
-- &DBSubnetGroupDescription=My%20new%20DBSubnetGroup &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 Complete My new DBSubnetGroup mydbsubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.CreateDBSubnetGroup
    (
    -- * Request
      CreateDBSubnetGroup
    -- ** Request constructor
    , mkCreateDBSubnetGroup
    -- ** Request lenses
    , cdbsg1DBSubnetGroupName
    , cdbsg1DBSubnetGroupDescription
    , cdbsg1SubnetIds
    , cdbsg1Tags

    -- * Response
    , CreateDBSubnetGroupResponse
    -- ** Response constructor
    , mkCreateDBSubnetGroupResponse
    -- ** Response lenses
    , cdbsgrrDBSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CreateDBSubnetGroup = CreateDBSubnetGroup
    { _cdbsg1DBSubnetGroupName :: Text
    , _cdbsg1DBSubnetGroupDescription :: Text
    , _cdbsg1SubnetIds :: [Text]
    , _cdbsg1Tags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSubnetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSubnetGroupName ::@ @Text@
--
-- * @DBSubnetGroupDescription ::@ @Text@
--
-- * @SubnetIds ::@ @[Text]@
--
-- * @Tags ::@ @[Tag]@
--
mkCreateDBSubnetGroup :: Text -- ^ 'cdbsg1DBSubnetGroupName'
                      -> Text -- ^ 'cdbsg1DBSubnetGroupDescription'
                      -> [Text] -- ^ 'cdbsg1SubnetIds'
                      -> CreateDBSubnetGroup
mkCreateDBSubnetGroup p1 p2 p3 = CreateDBSubnetGroup
    { _cdbsg1DBSubnetGroupName = p1
    , _cdbsg1DBSubnetGroupDescription = p2
    , _cdbsg1SubnetIds = p3
    , _cdbsg1Tags = mempty
    }

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters
-- or hyphens. Must not be "Default". Example: mySubnetgroup.
cdbsg1DBSubnetGroupName :: Lens' CreateDBSubnetGroup Text
cdbsg1DBSubnetGroupName =
    lens _cdbsg1DBSubnetGroupName
         (\s a -> s { _cdbsg1DBSubnetGroupName = a })

-- | The description for the DB subnet group.
cdbsg1DBSubnetGroupDescription :: Lens' CreateDBSubnetGroup Text
cdbsg1DBSubnetGroupDescription =
    lens _cdbsg1DBSubnetGroupDescription
         (\s a -> s { _cdbsg1DBSubnetGroupDescription = a })

-- | The EC2 Subnet IDs for the DB subnet group.
cdbsg1SubnetIds :: Lens' CreateDBSubnetGroup [Text]
cdbsg1SubnetIds = lens _cdbsg1SubnetIds (\s a -> s { _cdbsg1SubnetIds = a })

-- | A list of tags.
cdbsg1Tags :: Lens' CreateDBSubnetGroup [Tag]
cdbsg1Tags = lens _cdbsg1Tags (\s a -> s { _cdbsg1Tags = a })

instance ToQuery CreateDBSubnetGroup where
    toQuery = genericQuery def

newtype CreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse
    { _cdbsgrrDBSubnetGroup :: Maybe DBSubnetGroup
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSubnetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSubnetGroup ::@ @Maybe DBSubnetGroup@
--
mkCreateDBSubnetGroupResponse :: CreateDBSubnetGroupResponse
mkCreateDBSubnetGroupResponse = CreateDBSubnetGroupResponse
    { _cdbsgrrDBSubnetGroup = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSubnetGroup ModifyDBSubnetGroup DescribeDBSubnetGroups
-- DeleteDBSubnetGroup This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
cdbsgrrDBSubnetGroup :: Lens' CreateDBSubnetGroupResponse (Maybe DBSubnetGroup)
cdbsgrrDBSubnetGroup =
    lens _cdbsgrrDBSubnetGroup (\s a -> s { _cdbsgrrDBSubnetGroup = a })

instance FromXML CreateDBSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBSubnetGroup where
    type Sv CreateDBSubnetGroup = RDS
    type Rs CreateDBSubnetGroup = CreateDBSubnetGroupResponse

    request = post "CreateDBSubnetGroup"
    response _ = xmlResponse
