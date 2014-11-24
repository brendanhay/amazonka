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

-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a DBSnapshot. If the snapshot is being copied, the copy operation
-- is terminated.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_DeleteDBSnapshot.html>
module Network.AWS.RDS.DeleteDBSnapshot
    (
    -- * Request
      DeleteDBSnapshot
    -- ** Request constructor
    , deleteDBSnapshot
    -- ** Request lenses
    , ddbs1DBSnapshotIdentifier

    -- * Response
    , DeleteDBSnapshotResponse
    -- ** Response constructor
    , deleteDBSnapshotResponse
    -- ** Response lenses
    , ddbsrDBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

newtype DeleteDBSnapshot = DeleteDBSnapshot
    { _ddbs1DBSnapshotIdentifier :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

-- | 'DeleteDBSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbs1DBSnapshotIdentifier' @::@ 'Text'
--
deleteDBSnapshot :: Text -- ^ 'ddbs1DBSnapshotIdentifier'
                 -> DeleteDBSnapshot
deleteDBSnapshot p1 = DeleteDBSnapshot
    { _ddbs1DBSnapshotIdentifier = p1
    }

-- | The DBSnapshot identifier. Constraints: Must be the name of an existing
-- DB snapshot in the 'available' state.
ddbs1DBSnapshotIdentifier :: Lens' DeleteDBSnapshot Text
ddbs1DBSnapshotIdentifier =
    lens _ddbs1DBSnapshotIdentifier
        (\s a -> s { _ddbs1DBSnapshotIdentifier = a })

newtype DeleteDBSnapshotResponse = DeleteDBSnapshotResponse
    { _ddbsrDBSnapshot :: Maybe DBSnapshot
    } deriving (Eq, Show)

-- | 'DeleteDBSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsrDBSnapshot' @::@ 'Maybe' 'DBSnapshot'
--
deleteDBSnapshotResponse :: DeleteDBSnapshotResponse
deleteDBSnapshotResponse = DeleteDBSnapshotResponse
    { _ddbsrDBSnapshot = Nothing
    }

ddbsrDBSnapshot :: Lens' DeleteDBSnapshotResponse (Maybe DBSnapshot)
ddbsrDBSnapshot = lens _ddbsrDBSnapshot (\s a -> s { _ddbsrDBSnapshot = a })

instance ToPath DeleteDBSnapshot where
    toPath = const "/"

instance ToQuery DeleteDBSnapshot where
    toQuery DeleteDBSnapshot{..} = mconcat
        [ "DBSnapshotIdentifier" =? _ddbs1DBSnapshotIdentifier
        ]

instance ToHeaders DeleteDBSnapshot

instance AWSRequest DeleteDBSnapshot where
    type Sv DeleteDBSnapshot = RDS
    type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse

    request  = post "DeleteDBSnapshot"
    response = xmlResponse

instance FromXML DeleteDBSnapshotResponse where
    parseXML = withElement "DeleteDBSnapshotResult" $ \x -> DeleteDBSnapshotResponse
        <$> x .@? "DBSnapshot"
