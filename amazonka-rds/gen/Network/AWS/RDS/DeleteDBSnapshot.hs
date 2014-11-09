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
module Network.AWS.RDS.DeleteDBSnapshot
    (
    -- * Request
      DeleteDBSnapshotMessage
    -- ** Request constructor
    , deleteDBSnapshotMessage
    -- ** Request lenses
    , ddbsm1DBSnapshotIdentifier

    -- * Response
    , DeleteDBSnapshotResult
    -- ** Response constructor
    , deleteDBSnapshotResult
    -- ** Response lenses
    , ddbsrDBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

newtype DeleteDBSnapshotMessage = DeleteDBSnapshotMessage
    { _ddbsm1DBSnapshotIdentifier :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteDBSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsm1DBSnapshotIdentifier' @::@ 'Text'
--
deleteDBSnapshotMessage :: Text -- ^ 'ddbsm1DBSnapshotIdentifier'
                        -> DeleteDBSnapshotMessage
deleteDBSnapshotMessage p1 = DeleteDBSnapshotMessage
    { _ddbsm1DBSnapshotIdentifier = p1
    }

-- | The DBSnapshot identifier. Constraints: Must be the name of an existing
-- DB snapshot in the available state.
ddbsm1DBSnapshotIdentifier :: Lens' DeleteDBSnapshotMessage Text
ddbsm1DBSnapshotIdentifier =
    lens _ddbsm1DBSnapshotIdentifier
        (\s a -> s { _ddbsm1DBSnapshotIdentifier = a })

instance ToPath DeleteDBSnapshotMessage where
    toPath = const "/"

instance ToQuery DeleteDBSnapshotMessage

newtype DeleteDBSnapshotResult = DeleteDBSnapshotResult
    { _ddbsrDBSnapshot :: Maybe DBSnapshot
    } deriving (Eq, Show, Generic)

-- | 'DeleteDBSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddbsrDBSnapshot' @::@ 'Maybe' 'DBSnapshot'
--
deleteDBSnapshotResult :: DeleteDBSnapshotResult
deleteDBSnapshotResult = DeleteDBSnapshotResult
    { _ddbsrDBSnapshot = Nothing
    }

ddbsrDBSnapshot :: Lens' DeleteDBSnapshotResult (Maybe DBSnapshot)
ddbsrDBSnapshot = lens _ddbsrDBSnapshot (\s a -> s { _ddbsrDBSnapshot = a })

instance AWSRequest DeleteDBSnapshotMessage where
    type Sv DeleteDBSnapshotMessage = RDS
    type Rs DeleteDBSnapshotMessage = DeleteDBSnapshotResult

    request  = post "DeleteDBSnapshot"
    response = const . xmlResponse $ \h x -> DeleteDBSnapshotResult
newtype
