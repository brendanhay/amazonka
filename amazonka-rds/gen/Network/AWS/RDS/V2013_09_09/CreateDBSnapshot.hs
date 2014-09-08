{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CreateDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a DBSnapshot. The source DBInstance must be in "available" state.
-- https://rds.amazonaws.com/ ?Action=CreateDBSnapshot
-- &DBInstanceIdentifier=simcoprod01 &DBSnapshotIdentifier=mydbsnapshot
-- &Version=2013-05-15 &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-05-23T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- mysql creating us-east-1a general-public-license 2011-05-23T06:06:43.110Z
-- 10 simcoprod01 5.1.50 mydbsnapshot manual master
-- c4181d1d-8505-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.CreateDBSnapshot
    (
    -- * Request
      CreateDBSnapshot
    -- ** Request constructor
    , mkCreateDBSnapshot
    -- ** Request lenses
    , cdbs1DBSnapshotIdentifier
    , cdbs1DBInstanceIdentifier
    , cdbs1Tags

    -- * Response
    , CreateDBSnapshotResponse
    -- ** Response constructor
    , mkCreateDBSnapshotResponse
    -- ** Response lenses
    , cdbsrrDBSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | 
data CreateDBSnapshot = CreateDBSnapshot
    { _cdbs1DBSnapshotIdentifier :: Text
    , _cdbs1DBInstanceIdentifier :: Text
    , _cdbs1Tags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSnapshot' request.
mkCreateDBSnapshot :: Text -- ^ 'cdbs1DBSnapshotIdentifier'
                   -> Text -- ^ 'cdbs1DBInstanceIdentifier'
                   -> CreateDBSnapshot
mkCreateDBSnapshot p1 p2 = CreateDBSnapshot
    { _cdbs1DBSnapshotIdentifier = p1
    , _cdbs1DBInstanceIdentifier = p2
    , _cdbs1Tags = mempty
    }

-- | The identifier for the DB snapshot. Constraints: Cannot be null, empty, or
-- blank Must contain from 1 to 255 alphanumeric characters or hyphens First
-- character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: my-snapshot-id.
cdbs1DBSnapshotIdentifier :: Lens' CreateDBSnapshot Text
cdbs1DBSnapshotIdentifier =
    lens _cdbs1DBSnapshotIdentifier
         (\s a -> s { _cdbs1DBSnapshotIdentifier = a })

-- | The DB instance identifier. This is the unique key that identifies a DB
-- instance. This parameter isn't case sensitive. Constraints: Must contain
-- from 1 to 63 alphanumeric characters or hyphens First character must be a
-- letter Cannot end with a hyphen or contain two consecutive hyphens.
cdbs1DBInstanceIdentifier :: Lens' CreateDBSnapshot Text
cdbs1DBInstanceIdentifier =
    lens _cdbs1DBInstanceIdentifier
         (\s a -> s { _cdbs1DBInstanceIdentifier = a })

-- | A list of tags.
cdbs1Tags :: Lens' CreateDBSnapshot [Tag]
cdbs1Tags = lens _cdbs1Tags (\s a -> s { _cdbs1Tags = a })

instance ToQuery CreateDBSnapshot where
    toQuery = genericQuery def

newtype CreateDBSnapshotResponse = CreateDBSnapshotResponse
    { _cdbsrrDBSnapshot :: Maybe DBSnapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreateDBSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreateDBSnapshotResponse :: CreateDBSnapshotResponse
mkCreateDBSnapshotResponse = CreateDBSnapshotResponse
    { _cdbsrrDBSnapshot = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
cdbsrrDBSnapshot :: Lens' CreateDBSnapshotResponse (Maybe DBSnapshot)
cdbsrrDBSnapshot =
    lens _cdbsrrDBSnapshot (\s a -> s { _cdbsrrDBSnapshot = a })

instance FromXML CreateDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBSnapshot where
    type Sv CreateDBSnapshot = RDS
    type Rs CreateDBSnapshot = CreateDBSnapshotResponse

    request = post "CreateDBSnapshot"
    response _ = xmlResponse
