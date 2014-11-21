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
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyDBSnapshot.html>
module Network.AWS.RDS.CopyDBSnapshot
    (
    -- * Request
      CopyDBSnapshot
    -- ** Request constructor
    , copyDBSnapshot
    -- ** Request lenses
    , cdbsSourceDBSnapshotIdentifier
    , cdbsTags
    , cdbsTargetDBSnapshotIdentifier

    -- * Response
    , CopyDBSnapshotResponse
    -- ** Response constructor
    , copyDBSnapshotResponse
    -- ** Response lenses
    , cdbsrDBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data CopyDBSnapshot = CopyDBSnapshot
    { _cdbsSourceDBSnapshotIdentifier :: Text
    , _cdbsTags                       :: List "Tag" Tag
    , _cdbsTargetDBSnapshotIdentifier :: Text
    } deriving (Eq, Show)

-- | 'CopyDBSnapshot' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsSourceDBSnapshotIdentifier' @::@ 'Text'
--
-- * 'cdbsTags' @::@ ['Tag']
--
-- * 'cdbsTargetDBSnapshotIdentifier' @::@ 'Text'
--
copyDBSnapshot :: Text -- ^ 'cdbsSourceDBSnapshotIdentifier'
               -> Text -- ^ 'cdbsTargetDBSnapshotIdentifier'
               -> CopyDBSnapshot
copyDBSnapshot p1 p2 = CopyDBSnapshot
    { _cdbsSourceDBSnapshotIdentifier = p1
    , _cdbsTargetDBSnapshotIdentifier = p2
    , _cdbsTags                       = mempty
    }

-- | The identifier for the source DB snapshot. Constraints: Must specify a
-- valid system snapshot in the "available" state. If the source snapshot is
-- in the same region as the copy, specify a valid DB snapshot identifier.
-- If the source snapshot is in a different region than the copy, specify a
-- valid DB snapshot ARN. For more information, go to Copying a DB Snapshot.
-- Example: rds:mydb-2012-04-02-00-01 Example:
-- arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805.
-- 
cdbsSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdbsSourceDBSnapshotIdentifier =
    lens _cdbsSourceDBSnapshotIdentifier
        (\s a -> s { _cdbsSourceDBSnapshotIdentifier = a })

cdbsTags :: Lens' CopyDBSnapshot [Tag]
cdbsTags = lens _cdbsTags (\s a -> s { _cdbsTags = a }) . _List

-- | The identifier for the copied snapshot. Constraints: Cannot be null,
-- empty, or blank Must contain from 1 to 255 alphanumeric characters or
-- hyphens First character must be a letter Cannot end with a hyphen or
-- contain two consecutive hyphens Example: my-db-snapshot.
cdbsTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdbsTargetDBSnapshotIdentifier =
    lens _cdbsTargetDBSnapshotIdentifier
        (\s a -> s { _cdbsTargetDBSnapshotIdentifier = a })

newtype CopyDBSnapshotResponse = CopyDBSnapshotResponse
    { _cdbsrDBSnapshot :: Maybe DBSnapshot
    } deriving (Eq, Show)

-- | 'CopyDBSnapshotResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsrDBSnapshot' @::@ 'Maybe' 'DBSnapshot'
--
copyDBSnapshotResponse :: CopyDBSnapshotResponse
copyDBSnapshotResponse = CopyDBSnapshotResponse
    { _cdbsrDBSnapshot = Nothing
    }

cdbsrDBSnapshot :: Lens' CopyDBSnapshotResponse (Maybe DBSnapshot)
cdbsrDBSnapshot = lens _cdbsrDBSnapshot (\s a -> s { _cdbsrDBSnapshot = a })

instance ToPath CopyDBSnapshot where
    toPath = const "/"

instance ToQuery CopyDBSnapshot where
    toQuery CopyDBSnapshot{..} = mconcat
        [ "SourceDBSnapshotIdentifier" =? _cdbsSourceDBSnapshotIdentifier
        , "Tags"                       =? _cdbsTags
        , "TargetDBSnapshotIdentifier" =? _cdbsTargetDBSnapshotIdentifier
        ]

instance ToHeaders CopyDBSnapshot

instance AWSRequest CopyDBSnapshot where
    type Sv CopyDBSnapshot = RDS
    type Rs CopyDBSnapshot = CopyDBSnapshotResponse

    request  = post "CopyDBSnapshot"
    response = xmlResponse

instance FromXML CopyDBSnapshotResponse where
    parseXML = withElement "CopyDBSnapshotResult" $ \x -> CopyDBSnapshotResponse
        <$> x .@? "DBSnapshot"
