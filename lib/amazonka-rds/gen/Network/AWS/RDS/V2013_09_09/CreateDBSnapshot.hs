{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.RDS.V2013_09_09.CreateDBSnapshot where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CreateDBSnapshot' request.
createDBSnapshot :: Text -- ^ '_cdbsnDBSnapshotIdentifier'
                 -> Text -- ^ '_cdbsnDBInstanceIdentifier'
                 -> CreateDBSnapshot
createDBSnapshot p1 p2 = CreateDBSnapshot
    { _cdbsnDBSnapshotIdentifier = p1
    , _cdbsnDBInstanceIdentifier = p2
    , _cdbsnTags = mempty
    }

data CreateDBSnapshot = CreateDBSnapshot
    { _cdbsnDBSnapshotIdentifier :: Text
      -- ^ The identifier for the DB snapshot. Constraints: Cannot be null,
      -- empty, or blank Must contain from 1 to 255 alphanumeric
      -- characters or hyphens First character must be a letter Cannot end
      -- with a hyphen or contain two consecutive hyphens Example:
      -- my-snapshot-id.
    , _cdbsnDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier. This is the unique key that
      -- identifies a DB instance. This parameter isn't case sensitive.
      -- Constraints: Must contain from 1 to 63 alphanumeric characters or
      -- hyphens First character must be a letter Cannot end with a hyphen
      -- or contain two consecutive hyphens.
    , _cdbsnTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

makeLenses ''CreateDBSnapshot

instance ToQuery CreateDBSnapshot where
    toQuery = genericQuery def

data CreateDBSnapshotResponse = CreateDBSnapshotResponse
    { _dbsxDBSnapshot :: Maybe DBSnapshot
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBSnapshot DeleteDBSnapshot This data type is used
      -- as a response element in the DescribeDBSnapshots action.
    } deriving (Show, Generic)

makeLenses ''CreateDBSnapshotResponse

instance FromXML CreateDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CreateDBSnapshot where
    type Sv CreateDBSnapshot = RDS
    type Rs CreateDBSnapshot = CreateDBSnapshotResponse

    request = post "CreateDBSnapshot"
    response _ = xmlResponse
