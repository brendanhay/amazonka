{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.ModifyDBSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies an existing DB subnet group. DB subnet groups must contain at
-- least one subnet in at least two AZs in the region.
-- https://rds.amazonaws.com/ ?Action=ModifyDBSubnetGroup
-- &DBSubnetGroupName=mydbsubnetgroup
-- &DBSubnetGroupDescription=My%20modified%20DBSubnetGroup &Version=2013-05-15
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256
-- &Timestamp=2011-02-15T18%3A14%3A49.482Z &AWSAccessKeyId= &Signature=
-- 990524496922 Complete My modified DBSubnetGroup mydbsubnetgroup Active
-- subnet-7c5b4115 us-east-1c Active subnet-7b5b4112 us-east-1b Active
-- subnet-3ea6bd57 us-east-1d ed662948-a57b-11df-9e38-7ffab86c801f.
module Network.AWS.RDS.V2013_09_09.ModifyDBSubnetGroup
    (
    -- * Request
      ModifyDBSubnetGroup
    -- ** Request constructor
    , mkModifyDBSubnetGroup
    -- ** Request lenses
    , mdbsgDBSubnetGroupName
    , mdbsgDBSubnetGroupDescription
    , mdbsgSubnetIds

    -- * Response
    , ModifyDBSubnetGroupResponse
    -- ** Response lenses
    , mdbsgrsDBSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data ModifyDBSubnetGroup = ModifyDBSubnetGroup
    { _mdbsgDBSubnetGroupName :: Text
    , _mdbsgDBSubnetGroupDescription :: Maybe Text
    , _mdbsgSubnetIds :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyDBSubnetGroup' request.
mkModifyDBSubnetGroup :: Text -- ^ 'mdbsgDBSubnetGroupName'
                      -> [Text] -- ^ 'mdbsgSubnetIds'
                      -> ModifyDBSubnetGroup
mkModifyDBSubnetGroup p1 p3 = ModifyDBSubnetGroup
    { _mdbsgDBSubnetGroupName = p1
    , _mdbsgDBSubnetGroupDescription = Nothing
    , _mdbsgSubnetIds = p3
    }

-- | The name for the DB subnet group. This value is stored as a lowercase
-- string. Constraints: Must contain no more than 255 alphanumeric characters
-- or hyphens. Must not be "Default". Example: mySubnetgroup.
mdbsgDBSubnetGroupName :: Lens' ModifyDBSubnetGroup Text
mdbsgDBSubnetGroupName =
    lens _mdbsgDBSubnetGroupName (\s a -> s { _mdbsgDBSubnetGroupName = a })

-- | The description for the DB subnet group.
mdbsgDBSubnetGroupDescription :: Lens' ModifyDBSubnetGroup (Maybe Text)
mdbsgDBSubnetGroupDescription =
    lens _mdbsgDBSubnetGroupDescription
         (\s a -> s { _mdbsgDBSubnetGroupDescription = a })

-- | The EC2 subnet IDs for the DB subnet group.
mdbsgSubnetIds :: Lens' ModifyDBSubnetGroup [Text]
mdbsgSubnetIds = lens _mdbsgSubnetIds (\s a -> s { _mdbsgSubnetIds = a })

instance ToQuery ModifyDBSubnetGroup where
    toQuery = genericQuery def

newtype ModifyDBSubnetGroupResponse = ModifyDBSubnetGroupResponse
    { _mdbsgrsDBSubnetGroup :: Maybe DBSubnetGroup
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSubnetGroup ModifyDBSubnetGroup DescribeDBSubnetGroups
-- DeleteDBSubnetGroup This data type is used as a response element in the
-- DescribeDBSubnetGroups action.
mdbsgrsDBSubnetGroup :: Lens' ModifyDBSubnetGroupResponse (Maybe DBSubnetGroup)
mdbsgrsDBSubnetGroup =
    lens _mdbsgrsDBSubnetGroup (\s a -> s { _mdbsgrsDBSubnetGroup = a })

instance FromXML ModifyDBSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyDBSubnetGroup where
    type Sv ModifyDBSubnetGroup = RDS
    type Rs ModifyDBSubnetGroup = ModifyDBSubnetGroupResponse

    request = post "ModifyDBSubnetGroup"
    response _ = xmlResponse
