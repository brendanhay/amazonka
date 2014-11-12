{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.RDS.DeleteDBInstance
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
module Network.AWS.RDS.DeleteDBInstance
    (
    -- * Request
      DeleteDBInstanceMessage
    -- ** Request constructor
    , deleteDBInstance
    -- ** Request lenses
    , ddbim1DBInstanceIdentifier
    , ddbim1FinalDBSnapshotIdentifier
    , ddbim1SkipFinalSnapshot

    -- * Response
    , DeleteDBInstanceResult
    -- ** Response constructor
    , deleteDBInstanceResponse
    -- ** Response lenses
    , ddbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data DeleteDBInstanceMessage = DeleteDBInstanceMessage
    { _ddbim1DBInstanceIdentifier      :: Text
    , _ddbim1FinalDBSnapshotIdentifier :: Maybe Text
    , _ddbim1SkipFinalSnapshot         :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDBInstanceMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbim1DBInstanceIdentifier' @::@ 'Text'
--
-- * 'ddbim1FinalDBSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbim1SkipFinalSnapshot' @::@ 'Maybe' 'Bool'
--
deleteDBInstance :: Text -- ^ 'ddbim1DBInstanceIdentifier'
                 -> DeleteDBInstanceMessage
deleteDBInstance p1 = DeleteDBInstanceMessage
    { _ddbim1DBInstanceIdentifier      = p1
    , _ddbim1SkipFinalSnapshot         = Nothing
    , _ddbim1FinalDBSnapshotIdentifier = Nothing
    }

-- | The DB instance identifier for the DB instance to be deleted. This
-- parameter isn't case sensitive. Constraints: Must contain from 1 to 63
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
ddbim1DBInstanceIdentifier :: Lens' DeleteDBInstanceMessage Text
ddbim1DBInstanceIdentifier =
    lens _ddbim1DBInstanceIdentifier
        (\s a -> s { _ddbim1DBInstanceIdentifier = a })

-- | The DBSnapshotIdentifier of the new DBSnapshot created when
-- SkipFinalSnapshot is set to false. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with
-- a hyphen or contain two consecutive hyphens Cannot be specified when
-- deleting a read replica.
ddbim1FinalDBSnapshotIdentifier :: Lens' DeleteDBInstanceMessage (Maybe Text)
ddbim1FinalDBSnapshotIdentifier =
    lens _ddbim1FinalDBSnapshotIdentifier
        (\s a -> s { _ddbim1FinalDBSnapshotIdentifier = a })

-- | Determines whether a final DB snapshot is created before the DB instance
-- is deleted. If true is specified, no DBSnapshot is created. If false is
-- specified, a DB snapshot is created before the DB instance is deleted.
-- Specify true when deleting a read replica. Default: false.
ddbim1SkipFinalSnapshot :: Lens' DeleteDBInstanceMessage (Maybe Bool)
ddbim1SkipFinalSnapshot =
    lens _ddbim1SkipFinalSnapshot (\s a -> s { _ddbim1SkipFinalSnapshot = a })

instance ToQuery DeleteDBInstanceMessage

instance ToPath DeleteDBInstanceMessage where
    toPath = const "/"

newtype DeleteDBInstanceResult = DeleteDBInstanceResult
    { _ddbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Show, Generic)

-- | 'DeleteDBInstanceResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
deleteDBInstanceResponse :: DeleteDBInstanceResult
deleteDBInstanceResponse = DeleteDBInstanceResult
    { _ddbirDBInstance = Nothing
    }

ddbirDBInstance :: Lens' DeleteDBInstanceResult (Maybe DBInstance)
ddbirDBInstance = lens _ddbirDBInstance (\s a -> s { _ddbirDBInstance = a })

instance FromXML DeleteDBInstanceResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteDBInstanceResult"

instance AWSRequest DeleteDBInstanceMessage where
    type Sv DeleteDBInstanceMessage = RDS
    type Rs DeleteDBInstanceMessage = DeleteDBInstanceResult

    request  = post "DeleteDBInstance"
    response = xmlResponse $ \h x -> DeleteDBInstanceResult
        <$> x %| "DBInstance"
