{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.V2013_09_09.DeleteDBInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DeleteDBInstance action deletes a previously provisioned DB instance. A
-- successful response from the web service indicates the request was received
-- correctly. When you delete a DB instance, all automated backups for that
-- instance are deleted and cannot be recovered. Manual DB snapshots of the DB
-- instance to be deleted are not deleted. If a final DB snapshot is requested
-- the status of the RDS instance will be "deleting" until the DB snapshot is
-- created. The API action DescribeDBInstance is used to monitor the status of
-- this operation. The action cannot be canceled or reverted once submitted.
-- https://rds.amazonaws.com/ ?Action=DeleteDBInstance
-- &DBInstanceIdentifier=myrestoreddbinstance &SkipFinalSnapshot=true
-- &Version=2013-05-15 &Timestamp=2011-05-23T07%3A19%3A35.947Z
-- &SignatureVersion=2 &SignatureMethod=HmacSHA256 &AWSAccessKeyId=
-- &Signature= 2011-05-23T07:15:00Z mysql 1 false general-public-license
-- deleting 5.1.50 3306
-- myrestoreddbinstance.cu7u2t4uz396.us-east.rds.amazonaws.com
-- myrestoreddbinstance in-sync default.mysql5.1 active default 00:00-00:30
-- true sat:07:30-sat:08:00 us-east-1d 2011-05-23T06:52:48.255Z 10 db.m1.large
-- master 03ea4ae8-850d-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.DeleteDBInstance where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteDBInstance' request.
deleteDBInstance :: Text -- ^ '_ddbimDBInstanceIdentifier'
                 -> DeleteDBInstance
deleteDBInstance p1 = DeleteDBInstance
    { _ddbimDBInstanceIdentifier = p1
    , _ddbimSkipFinalSnapshot = Nothing
    , _ddbimFinalDBSnapshotIdentifier = Nothing
    }

data DeleteDBInstance = DeleteDBInstance
    { _ddbimDBInstanceIdentifier :: Text
      -- ^ The DB instance identifier for the DB instance to be deleted.
      -- This parameter isn't case sensitive. Constraints: Must contain
      -- from 1 to 63 alphanumeric characters or hyphens First character
      -- must be a letter Cannot end with a hyphen or contain two
      -- consecutive hyphens.
    , _ddbimSkipFinalSnapshot :: Maybe Bool
      -- ^ Determines whether a final DB snapshot is created before the DB
      -- instance is deleted. If true is specified, no DBSnapshot is
      -- created. If false is specified, a DB snapshot is created before
      -- the DB instance is deleted. Specify true when deleting a read
      -- replica. The FinalDBSnapshotIdentifier parameter must be
      -- specified if SkipFinalSnapshot is false. Default: false.
    , _ddbimFinalDBSnapshotIdentifier :: Maybe Text
      -- ^ The DBSnapshotIdentifier of the new DBSnapshot created when
      -- SkipFinalSnapshot is set to false. Specifying this parameter and
      -- also setting the SkipFinalShapshot parameter to true results in
      -- an error. Constraints: Must be 1 to 255 alphanumeric characters
      -- First character must be a letter Cannot end with a hyphen or
      -- contain two consecutive hyphens Cannot be specified when deleting
      -- a read replica.
    } deriving (Show, Generic)

makeLenses ''DeleteDBInstance

instance ToQuery DeleteDBInstance where
    toQuery = genericToQuery def

data DeleteDBInstanceResponse = DeleteDBInstanceResponse
    { _dbidtDBInstance :: Maybe DBInstance
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBInstance DeleteDBInstance ModifyDBInstance This
      -- data type is used as a response element in the
      -- DescribeDBInstances action.
    } deriving (Show, Generic)

makeLenses ''DeleteDBInstanceResponse

instance AWSRequest DeleteDBInstance where
    type Sv DeleteDBInstance = RDS
    type Rs DeleteDBInstance = DeleteDBInstanceResponse

    request = post "DeleteDBInstance"
    response _ = cursorResponse $ \hs xml ->
        pure DeleteDBInstanceResponse
            <*> xml %|? "DBInstance"
