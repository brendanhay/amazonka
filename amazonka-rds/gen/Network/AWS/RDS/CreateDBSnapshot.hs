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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSnapshot.html>
module Network.AWS.RDS.CreateDBSnapshot
    (
    -- * Request
      CreateDBSnapshot
    -- ** Request constructor
    , createDBSnapshot
    -- ** Request lenses
    , cdbs1DBInstanceIdentifier
    , cdbs1DBSnapshotIdentifier
    , cdbs1Tags

    -- * Response
    , CreateDBSnapshotResponse
    -- ** Response constructor
    , createDBSnapshotResponse
    -- ** Response lenses
    , cdbsr1DBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CreateDBSnapshot = CreateDBSnapshot
    { _cdbs1DBInstanceIdentifier :: Text
    , _cdbs1DBSnapshotIdentifier :: Text
    , _cdbs1Tags                 :: List "Tag" Tag
    } deriving (Eq, Show)

-- | 'CreateDBSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbs1DBInstanceIdentifier' @::@ 'Text'
--
-- * 'cdbs1DBSnapshotIdentifier' @::@ 'Text'
--
-- * 'cdbs1Tags' @::@ ['Tag']
--
createDBSnapshot :: Text -- ^ 'cdbs1DBSnapshotIdentifier'
                 -> Text -- ^ 'cdbs1DBInstanceIdentifier'
                 -> CreateDBSnapshot
createDBSnapshot p1 p2 = CreateDBSnapshot
    { _cdbs1DBSnapshotIdentifier = p1
    , _cdbs1DBInstanceIdentifier = p2
    , _cdbs1Tags                 = mempty
    }

-- | The DB instance identifier. This is the unique key that identifies a DB
-- instance. Constraints: Must contain from 1 to 63 alphanumeric characters
-- or hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens.
cdbs1DBInstanceIdentifier :: Lens' CreateDBSnapshot Text
cdbs1DBInstanceIdentifier =
    lens _cdbs1DBInstanceIdentifier
        (\s a -> s { _cdbs1DBInstanceIdentifier = a })

-- | The identifier for the DB snapshot. Constraints: Cannot be null, empty,
-- or blank Must contain from 1 to 255 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: my-snapshot-id.
cdbs1DBSnapshotIdentifier :: Lens' CreateDBSnapshot Text
cdbs1DBSnapshotIdentifier =
    lens _cdbs1DBSnapshotIdentifier
        (\s a -> s { _cdbs1DBSnapshotIdentifier = a })

cdbs1Tags :: Lens' CreateDBSnapshot [Tag]
cdbs1Tags = lens _cdbs1Tags (\s a -> s { _cdbs1Tags = a }) . _List

newtype CreateDBSnapshotResponse = CreateDBSnapshotResponse
    { _cdbsr1DBSnapshot :: Maybe DBSnapshot
    } deriving (Eq, Show)

-- | 'CreateDBSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsr1DBSnapshot' @::@ 'Maybe' 'DBSnapshot'
--
createDBSnapshotResponse :: CreateDBSnapshotResponse
createDBSnapshotResponse = CreateDBSnapshotResponse
    { _cdbsr1DBSnapshot = Nothing
    }

cdbsr1DBSnapshot :: Lens' CreateDBSnapshotResponse (Maybe DBSnapshot)
cdbsr1DBSnapshot = lens _cdbsr1DBSnapshot (\s a -> s { _cdbsr1DBSnapshot = a })

instance ToPath CreateDBSnapshot where
    toPath = const "/"

instance ToQuery CreateDBSnapshot where
    toQuery CreateDBSnapshot{..} = mconcat
        [ "DBInstanceIdentifier" =? _cdbs1DBInstanceIdentifier
        , "DBSnapshotIdentifier" =? _cdbs1DBSnapshotIdentifier
        , "Tags"                 =? _cdbs1Tags
        ]

instance ToHeaders CreateDBSnapshot

instance AWSRequest CreateDBSnapshot where
    type Sv CreateDBSnapshot = RDS
    type Rs CreateDBSnapshot = CreateDBSnapshotResponse

    request  = post "CreateDBSnapshot"
    response = xmlResponse

instance FromXML CreateDBSnapshotResponse where
    parseXML = withElement "CreateDBSnapshotResult" $ \x -> CreateDBSnapshotResponse
        <$> x .@? "DBSnapshot"


Some kind of operator / class to check the types whether to continue?
