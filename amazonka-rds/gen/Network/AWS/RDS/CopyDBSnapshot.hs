{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
-- "available" state. https://rds.amazonaws.com/ ?Action=CopyDBSnapshot
-- &SourceDBSnapshotIdentifier=rds:simcoprod01-2012-04-02-00-01
-- &TargetDBSnapshotIdentifier=mydbsnapshot &Version=2013-05-15
-- &SignatureVersion=2&SignatureMethod=HmacSHA256
-- &Timestamp=2011-12-12T06%3A27%3A42.551Z &AWSAccessKeyId= &Signature= 3306
-- mysql available us-east-1a general-public-license 2011-05-23T06:06:43.110Z
-- 10 simcoprod01 5.1.50 mydbsnapshot manual master
-- c4181d1d-8505-11e0-90aa-eb648410240d.
module Network.AWS.RDS.CopyDBSnapshot
    (
    -- * Request
      CopyDBSnapshot
    -- ** Request constructor
    , mkCopyDBSnapshot
    -- ** Request lenses
    , cdbsSourceDBSnapshotIdentifier
    , cdbsTargetDBSnapshotIdentifier
    , cdbsTags

    -- * Response
    , CopyDBSnapshotResponse
    -- ** Response constructor
    , mkCopyDBSnapshotResponse
    -- ** Response lenses
    , cdbsrDBSnapshot
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import Network.AWS.Prelude

-- | 
data CopyDBSnapshot = CopyDBSnapshot
    { _cdbsSourceDBSnapshotIdentifier :: !Text
    , _cdbsTargetDBSnapshotIdentifier :: !Text
    , _cdbsTags :: [Tag]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyDBSnapshot' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @SourceDBSnapshotIdentifier ::@ @Text@
--
-- * @TargetDBSnapshotIdentifier ::@ @Text@
--
-- * @Tags ::@ @[Tag]@
--
mkCopyDBSnapshot :: Text -- ^ 'cdbsSourceDBSnapshotIdentifier'
                 -> Text -- ^ 'cdbsTargetDBSnapshotIdentifier'
                 -> CopyDBSnapshot
mkCopyDBSnapshot p1 p2 = CopyDBSnapshot
    { _cdbsSourceDBSnapshotIdentifier = p1
    , _cdbsTargetDBSnapshotIdentifier = p2
    , _cdbsTags = mempty
    }

-- | The identifier for the source DB snapshot. Constraints: Must specify a
-- valid system snapshot in the "available" state. If the source snapshot is
-- in the same region as the copy, specify a valid DB snapshot identifier. If
-- the source snapshot is in a different region than the copy, specify valid
-- DB snapshot ARN. For more information, go to Copying a DB Snapshot.
-- Example: rds:mydb-2012-04-02-00-01 Example:
-- arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805.
-- 
cdbsSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdbsSourceDBSnapshotIdentifier =
    lens _cdbsSourceDBSnapshotIdentifier
         (\s a -> s { _cdbsSourceDBSnapshotIdentifier = a })

-- | The identifier for the copied snapshot. Constraints: Cannot be null, empty,
-- or blank Must contain from 1 to 255 alphanumeric characters or hyphens
-- First character must be a letter Cannot end with a hyphen or contain two
-- consecutive hyphens Example: my-db-snapshot.
cdbsTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdbsTargetDBSnapshotIdentifier =
    lens _cdbsTargetDBSnapshotIdentifier
         (\s a -> s { _cdbsTargetDBSnapshotIdentifier = a })

-- | A list of tags.
cdbsTags :: Lens' CopyDBSnapshot [Tag]
cdbsTags = lens _cdbsTags (\s a -> s { _cdbsTags = a })

instance ToQuery CopyDBSnapshot where
    toQuery = genericQuery def

newtype CopyDBSnapshotResponse = CopyDBSnapshotResponse
    { _cdbsrDBSnapshot :: Maybe DBSnapshot
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CopyDBSnapshotResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DBSnapshot ::@ @Maybe DBSnapshot@
--
mkCopyDBSnapshotResponse :: CopyDBSnapshotResponse
mkCopyDBSnapshotResponse = CopyDBSnapshotResponse
    { _cdbsrDBSnapshot = Nothing
    }

-- | Contains the result of a successful invocation of the following actions:
-- CreateDBSnapshot DeleteDBSnapshot This data type is used as a response
-- element in the DescribeDBSnapshots action.
cdbsrDBSnapshot :: Lens' CopyDBSnapshotResponse (Maybe DBSnapshot)
cdbsrDBSnapshot = lens _cdbsrDBSnapshot (\s a -> s { _cdbsrDBSnapshot = a })

instance FromXML CopyDBSnapshotResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest CopyDBSnapshot where
    type Sv CopyDBSnapshot = RDS
    type Rs CopyDBSnapshot = CopyDBSnapshotResponse

    request = post "CopyDBSnapshot"
    response _ = xmlResponse
