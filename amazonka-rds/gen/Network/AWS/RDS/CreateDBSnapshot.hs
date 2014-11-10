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

-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a DBSnapshot. The source DBInstance must be in "available" state.
module Network.AWS.RDS.CreateDBSnapshot
    (
    -- * Request
      CreateDBSnapshotMessage
    -- ** Request constructor
    , createDBSnapshot
    -- ** Request lenses
    , cdbsm1DBInstanceIdentifier
    , cdbsm1DBSnapshotIdentifier
    , cdbsm1Tags

    -- * Response
    , CreateDBSnapshotResult
    -- ** Response constructor
    , createDBSnapshotResponse
    -- ** Response lenses
    , cdbsrDBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CreateDBSnapshotMessage = CreateDBSnapshotMessage
    { _cdbsm1DBInstanceIdentifier :: Text
    , _cdbsm1DBSnapshotIdentifier :: Text
    , _cdbsm1Tags                 :: [Tag]
    } deriving (Eq, Show, Generic)

-- | 'CreateDBSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsm1DBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbsm1DBSnapshotIdentifier' @::@ 'Text'
--
-- * 'cdbsm1Tags' @::@ ['Tag']
--
createDBSnapshot :: Text -- ^ 'cdbsm1DBSnapshotIdentifier'
                 -> Text -- ^ 'cdbsm1DBInstanceIdentifier'
                 -> CreateDBSnapshotMessage
createDBSnapshot p1 p2 = CreateDBSnapshotMessage
    { _cdbsm1DBSnapshotIdentifier = p1
    , _cdbsm1DBInstanceIdentifier = p2
    , _cdbsm1Tags                 = mempty
    }

-- | The DB instance identifier. This is the unique key that identifies a DB
-- instance. Constraints: Must contain from 1 to 63 alphanumeric characters
-- or hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens.
cdbsm1DBInstanceIdentifier :: Lens' CreateDBSnapshotMessage Text
cdbsm1DBInstanceIdentifier =
    lens _cdbsm1DBInstanceIdentifier
        (\s a -> s { _cdbsm1DBInstanceIdentifier = a })

-- | The identifier for the DB snapshot. Constraints: Cannot be null, empty,
-- or blank Must contain from 1 to 255 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: my-snapshot-id.
cdbsm1DBSnapshotIdentifier :: Lens' CreateDBSnapshotMessage Text
cdbsm1DBSnapshotIdentifier =
    lens _cdbsm1DBSnapshotIdentifier
        (\s a -> s { _cdbsm1DBSnapshotIdentifier = a })

cdbsm1Tags :: Lens' CreateDBSnapshotMessage [Tag]
cdbsm1Tags = lens _cdbsm1Tags (\s a -> s { _cdbsm1Tags = a })

instance ToPath CreateDBSnapshotMessage where
    toPath = const "/"

instance ToQuery CreateDBSnapshotMessage

newtype CreateDBSnapshotResult = CreateDBSnapshotResult
    { _cdbsrDBSnapshot :: Maybe DBSnapshot
    } deriving (Eq, Show, Generic)

-- | 'CreateDBSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsrDBSnapshot' @::@ 'Maybe' 'DBSnapshot'
--
createDBSnapshotResponse :: CreateDBSnapshotResult
createDBSnapshotResponse = CreateDBSnapshotResult
    { _cdbsrDBSnapshot = Nothing
    }

cdbsrDBSnapshot :: Lens' CreateDBSnapshotResult (Maybe DBSnapshot)
cdbsrDBSnapshot = lens _cdbsrDBSnapshot (\s a -> s { _cdbsrDBSnapshot = a })

instance AWSRequest CreateDBSnapshotMessage where
    type Sv CreateDBSnapshotMessage = RDS
    type Rs CreateDBSnapshotMessage = CreateDBSnapshotResult

    request  = post "CreateDBSnapshot"
    response = xmlResponse $ \h x -> CreateDBSnapshotResult
        <$> x %| "DBSnapshot"
