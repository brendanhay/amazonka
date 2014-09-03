{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.CopyDBSnapshot
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Copies the specified DBSnapshot. The source DBSnapshot must be in the
-- "available" state. https://rds.amazonaws.com/ ?Action=CopyDBSnapshot
-- &SourceDBSnapshotIdentifier=rds:simcoprod01-2012-04-02-00-01
-- &TargetDBSnapshotIdentifier=mydbsnapshot &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-12-12T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- mysql available us-east-1a general-public-license 2011-05-23T06:06:43.110Z
-- 10 simcoprod01 5.1.50 mydbsnapshot manual master
-- c4181d1d-8505-11e0-90aa-eb648410240d.
module Network.AWS.RDS.V2013_09_09.CopyDBSnapshot
    (
    -- * Request
      CopyDBSnapshot
    -- ** Request constructor
    , copyDBSnapshot
    -- ** Request lenses
    , cdbsmSourceDBSnapshotIdentifier
    , cdbsmTargetDBSnapshotIdentifier
    , cdbsmTags

    -- * Response
    , CopyDBSnapshotResponse
    -- ** Response lenses
    , dbswDBSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CopyDBSnapshot' request.
copyDBSnapshot :: Text -- ^ 'cdbsmSourceDBSnapshotIdentifier'
               -> Text -- ^ 'cdbsmTargetDBSnapshotIdentifier'
               -> CopyDBSnapshot
copyDBSnapshot p1 p2 = CopyDBSnapshot
    { _cdbsmSourceDBSnapshotIdentifier = p1
    , _cdbsmTargetDBSnapshotIdentifier = p2
    , _cdbsmTags = mempty
    }

data CopyDBSnapshot = CopyDBSnapshot
    { _cdbsmSourceDBSnapshotIdentifier :: Text
      -- ^ The identifier for the source DB snapshot. Constraints: Must
      -- specify a valid system snapshot in the "available" state. If the
      -- source snapshot is in the same region as the copy, specify a
      -- valid DB snapshot identifier. If the source snapshot is in a
      -- different region than the copy, specify valid DB snapshot ARN.
      -- For more information, go to Copying a DB Snapshot. Example:
      -- rds:mydb-2012-04-02-00-01 Example:
      -- arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805.
      -- 
    , _cdbsmTargetDBSnapshotIdentifier :: Text
      -- ^ The identifier for the copied snapshot. Constraints: Cannot be
      -- null, empty, or blank Must contain from 1 to 255 alphanumeric
      -- characters or hyphens First character must be a letter Cannot end
      -- with a hyphen or contain two consecutive hyphens Example:
      -- my-db-snapshot.
    , _cdbsmTags :: [Tag]
      -- ^ A list of tags.
    } deriving (Show, Generic)

-- | The identifier for the source DB snapshot. Constraints: Must specify a
-- valid system snapshot in the "available" state. If the source snapshot is
-- in the same region as the copy, specify a valid DB snapshot identifier. If
-- the source snapshot is in a different region than the copy, specify valid
-- DB snapshot ARN. For more information, go to Copying a DB Snapshot.
-- Example: rds:mydb-2012-04-02-00-01 Example:
-- arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805.
-- 
cdbsmSourceDBSnapshotIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> CopyDBSnapshot
    -> f CopyDBSnapshot
cdbsmSourceDBSnapshotIdentifier f x =
    (\y -> x { _cdbsmSourceDBSnapshotIdentifier = y })
       <$> f (_cdbsmSourceDBSnapshotIdentifier x)
{-# INLINE cdbsmSourceDBSnapshotIdentifier #-}

-- | The identifier for the copied snapshot. Constraints: Cannot be null, empty,
-- or blank Must contain from 1 to 255 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: my-db-snapshot.
cdbsmTargetDBSnapshotIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> CopyDBSnapshot
    -> f CopyDBSnapshot
cdbsmTargetDBSnapshotIdentifier f x =
    (\y -> x { _cdbsmTargetDBSnapshotIdentifier = y })
       <$> f (_cdbsmTargetDBSnapshotIdentifier x)
{-# INLINE cdbsmTargetDBSnapshotIdentifier #-}

-- | A list of tags.
cdbsmTags
    :: Functor f
    => ([Tag]
    -> f ([Tag]))
    -> CopyDBSnapshot
    -> f CopyDBSnapshot
cdbsmTags f x =
    (\y -> x { _cdbsmTags = y })
       <$> f (_cdbsmTags x)
{-# INLINE cdbsmTags #-}

instance ToQuery CopyDBSnapshot where
    toQuery = genericQuery def

data CopyDBSnapshotResponse = CopyDBSnapshotResponse
    { _dbswDBSnapshot :: Maybe DBSnapshot
      -- ^ Contains the result of a successful invocation of the following
      -- actions: CreateDBSnapshot DeleteDBSnapshot This data type is used
      -- as a response element in the DescribeDBSnapshots action.
    } deriving (Show, Generic)

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
dbswDBSnapshot
    :: Functor f
    => (Maybe DBSnapshot
    -> f (Maybe DBSnapshot))
    -> CopyDBSnapshotResponse
    -> f CopyDBSnapshotResponse
dbswDBSnapshot f x =
    (\y -> x { _dbswDBSnapshot = y })
       <$> f (_dbswDBSnapshot x)
{-# INLINE dbswDBSnapshot #-}

instance FromXML CopyDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopyDBSnapshot where
    type Sv CopyDBSnapshot = RDS
    type Rs CopyDBSnapshot = CopyDBSnapshotResponse

    request = post "CopyDBSnapshot"
    response _ = xmlResponse
