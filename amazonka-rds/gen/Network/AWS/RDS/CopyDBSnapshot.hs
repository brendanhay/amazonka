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

-- Module      : Network.AWS.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies the specified DBSnapshot. The source DBSnapshot must be in the
-- "available" state.
module Network.AWS.RDS.CopyDBSnapshot
    (
    -- * Request
      CopyDBSnapshotMessage
    -- ** Request constructor
    , copyDBSnapshotMessage
    -- ** Request lenses
    , cdbsmSourceDBSnapshotIdentifier
    , cdbsmTags
    , cdbsmTargetDBSnapshotIdentifier

    -- * Response
    , CopyDBSnapshotResult
    -- ** Response constructor
    , copyDBSnapshotResult
    -- ** Response lenses
    , cdbsr1DBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

data CopyDBSnapshotMessage = CopyDBSnapshotMessage
    { _cdbsmSourceDBSnapshotIdentifier :: Text
    , _cdbsmTags                       :: [Tag]
    , _cdbsmTargetDBSnapshotIdentifier :: Text
    } deriving (Eq, Show, Generic)

-- | 'CopyDBSnapshotMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsmSourceDBSnapshotIdentifier' @::@ 'Text'
--
-- * 'cdbsmTags' @::@ ['Tag']
--
-- * 'cdbsmTargetDBSnapshotIdentifier' @::@ 'Text'
--
copyDBSnapshotMessage :: Text -- ^ 'cdbsmSourceDBSnapshotIdentifier'
                      -> Text -- ^ 'cdbsmTargetDBSnapshotIdentifier'
                      -> CopyDBSnapshotMessage
copyDBSnapshotMessage p1 p2 = CopyDBSnapshotMessage
    { _cdbsmSourceDBSnapshotIdentifier = p1
    , _cdbsmTargetDBSnapshotIdentifier = p2
    , _cdbsmTags                       = mempty
    }

-- | The identifier for the source DB snapshot. Constraints: Must specify a
-- valid system snapshot in the "available" state. If the source snapshot is
-- in the same region as the copy, specify a valid DB snapshot identifier.
-- If the source snapshot is in a different region than the copy, specify a
-- valid DB snapshot ARN. For more information, go to Copying a DB Snapshot.
-- Example: rds:mydb-2012-04-02-00-01 Example:
-- arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805.
-- 
cdbsmSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshotMessage Text
cdbsmSourceDBSnapshotIdentifier =
    lens _cdbsmSourceDBSnapshotIdentifier
        (\s a -> s { _cdbsmSourceDBSnapshotIdentifier = a })

cdbsmTags :: Lens' CopyDBSnapshotMessage [Tag]
cdbsmTags = lens _cdbsmTags (\s a -> s { _cdbsmTags = a })

-- | The identifier for the copied snapshot. Constraints: Cannot be null,
-- empty, or blank Must contain from 1 to 255 alphanumeric characters or
-- hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens Example: my-db-snapshot.
cdbsmTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshotMessage Text
cdbsmTargetDBSnapshotIdentifier =
    lens _cdbsmTargetDBSnapshotIdentifier
        (\s a -> s { _cdbsmTargetDBSnapshotIdentifier = a })
instance ToQuery CopyDBSnapshotMessage

instance ToPath CopyDBSnapshotMessage where
    toPath = const "/"

newtype CopyDBSnapshotResult = CopyDBSnapshotResult
    { _cdbsr1DBSnapshot :: Maybe DBSnapshot
    } deriving (Eq, Show, Generic)

-- | 'CopyDBSnapshotResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsr1DBSnapshot' @::@ 'Maybe' 'DBSnapshot'
--
copyDBSnapshotResult :: CopyDBSnapshotResult
copyDBSnapshotResult = CopyDBSnapshotResult
    { _cdbsr1DBSnapshot = Nothing
    }

cdbsr1DBSnapshot :: Lens' CopyDBSnapshotResult (Maybe DBSnapshot)
cdbsr1DBSnapshot = lens _cdbsr1DBSnapshot (\s a -> s { _cdbsr1DBSnapshot = a })
instance FromXML CopyDBSnapshotResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CopyDBSnapshotResult"

instance AWSRequest CopyDBSnapshotMessage where
    type Sv CopyDBSnapshotMessage = RDS
    type Rs CopyDBSnapshotMessage = CopyDBSnapshotResult

    request  = post "CopyDBSnapshot"
    response = xmlResponse $ \h x -> CopyDBSnapshotResult
        <$> x %| "DBSnapshot"
