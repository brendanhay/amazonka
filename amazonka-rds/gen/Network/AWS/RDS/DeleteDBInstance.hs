{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBInstance.html>
module Network.AWS.RDS.DeleteDBInstance
    (
    -- * Request
      DeleteDBInstance
    -- ** Request constructor
    , deleteDBInstance
    -- ** Request lenses
    , ddbiDBInstanceIdentifier
    , ddbiFinalDBSnapshotIdentifier
    , ddbiSkipFinalSnapshot

    -- * Response
    , DeleteDBInstanceResponse
    -- ** Response constructor
    , deleteDBInstanceResponse
    -- ** Response lenses
    , ddbirDBInstance
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data DeleteDBInstance = DeleteDBInstance
    { _ddbiDBInstanceIdentifier      :: Text
    , _ddbiFinalDBSnapshotIdentifier :: Maybe Text
    , _ddbiSkipFinalSnapshot         :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'DeleteDBInstance' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbiDBInstanceIdentifier' @::@ 'Text'
--
-- * 'ddbiFinalDBSnapshotIdentifier' @::@ 'Maybe' 'Text'
--
-- * 'ddbiSkipFinalSnapshot' @::@ 'Maybe' 'Bool'
--
deleteDBInstance :: Text -- ^ 'ddbiDBInstanceIdentifier'
                 -> DeleteDBInstance
deleteDBInstance p1 = DeleteDBInstance
    { _ddbiDBInstanceIdentifier      = p1
    , _ddbiSkipFinalSnapshot         = Nothing
    , _ddbiFinalDBSnapshotIdentifier = Nothing
    }

-- | The DB instance identifier for the DB instance to be deleted. This
-- parameter isn't case sensitive. Constraints: Must contain from 1 to 63
-- alphanumeric characters or hyphens First character must be a letter
-- Cannot end with a hyphen or contain two consecutive hyphens.
ddbiDBInstanceIdentifier :: Lens' DeleteDBInstance Text
ddbiDBInstanceIdentifier =
    lens _ddbiDBInstanceIdentifier
        (\s a -> s { _ddbiDBInstanceIdentifier = a })

-- | The DBSnapshotIdentifier of the new DBSnapshot created when
-- SkipFinalSnapshot is set to false. Constraints: Must be 1 to 255
-- alphanumeric characters First character must be a letter Cannot end with
-- a hyphen or contain two consecutive hyphens Cannot be specified when
-- deleting a read replica.
ddbiFinalDBSnapshotIdentifier :: Lens' DeleteDBInstance (Maybe Text)
ddbiFinalDBSnapshotIdentifier =
    lens _ddbiFinalDBSnapshotIdentifier
        (\s a -> s { _ddbiFinalDBSnapshotIdentifier = a })

-- | Determines whether a final DB snapshot is created before the DB instance
-- is deleted. If true is specified, no DBSnapshot is created. If false is
-- specified, a DB snapshot is created before the DB instance is deleted.
-- Specify true when deleting a read replica. Default: false.
ddbiSkipFinalSnapshot :: Lens' DeleteDBInstance (Maybe Bool)
ddbiSkipFinalSnapshot =
    lens _ddbiSkipFinalSnapshot (\s a -> s { _ddbiSkipFinalSnapshot = a })

newtype DeleteDBInstanceResponse = DeleteDBInstanceResponse
    { _ddbirDBInstance :: Maybe DBInstance
    } deriving (Eq, Show)

-- | 'DeleteDBInstanceResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbirDBInstance' @::@ 'Maybe' 'DBInstance'
--
deleteDBInstanceResponse :: DeleteDBInstanceResponse
deleteDBInstanceResponse = DeleteDBInstanceResponse
    { _ddbirDBInstance = Nothing
    }

ddbirDBInstance :: Lens' DeleteDBInstanceResponse (Maybe DBInstance)
ddbirDBInstance = lens _ddbirDBInstance (\s a -> s { _ddbirDBInstance = a })

instance ToPath DeleteDBInstance where
    toPath = const "/"

instance ToQuery DeleteDBInstance where
    toQuery DeleteDBInstance{..} = mconcat
        [ "DBInstanceIdentifier"      =? _ddbiDBInstanceIdentifier
        , "FinalDBSnapshotIdentifier" =? _ddbiFinalDBSnapshotIdentifier
        , "SkipFinalSnapshot"         =? _ddbiSkipFinalSnapshot
        ]

instance ToHeaders DeleteDBInstance

query

instance AWSRequest DeleteDBInstance where
    type Sv DeleteDBInstance = RDS
    type Rs DeleteDBInstance = DeleteDBInstanceResponse

    request  = post "DeleteDBInstance"
    response = xmlResponse

instance FromXML DeleteDBInstanceResponse where
    parseXML = withElement "DeleteDBInstanceResult" $ \x -> DeleteDBInstanceResponse
        <$> x .@? "DBInstance"
