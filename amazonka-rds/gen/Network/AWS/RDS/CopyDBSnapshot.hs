{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Copies the specified DBSnapshot. The source DBSnapshot must be in the
-- \"available\" state.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CopyDBSnapshot.html>
module Network.AWS.RDS.CopyDBSnapshot
    (
    -- * Request
      CopyDBSnapshot
    -- ** Request constructor
    , copyDBSnapshot
    -- ** Request lenses
    , cdsTags
    , cdsSourceDBSnapshotIdentifier
    , cdsTargetDBSnapshotIdentifier

    -- * Response
    , CopyDBSnapshotResponse
    -- ** Response constructor
    , copyDBSnapshotResponse
    -- ** Response lenses
    , cdsrDBSnapshot
    , cdsrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'copyDBSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsTags'
--
-- * 'cdsSourceDBSnapshotIdentifier'
--
-- * 'cdsTargetDBSnapshotIdentifier'
data CopyDBSnapshot = CopyDBSnapshot'
    { _cdsTags                       :: !(Maybe [Tag])
    , _cdsSourceDBSnapshotIdentifier :: !Text
    , _cdsTargetDBSnapshotIdentifier :: !Text
    } deriving (Eq,Read,Show)

-- | 'CopyDBSnapshot' smart constructor.
copyDBSnapshot :: Text -> Text -> CopyDBSnapshot
copyDBSnapshot pSourceDBSnapshotIdentifier pTargetDBSnapshotIdentifier =
    CopyDBSnapshot'
    { _cdsTags = Nothing
    , _cdsSourceDBSnapshotIdentifier = pSourceDBSnapshotIdentifier
    , _cdsTargetDBSnapshotIdentifier = pTargetDBSnapshotIdentifier
    }

-- | FIXME: Undocumented member.
cdsTags :: Lens' CopyDBSnapshot [Tag]
cdsTags = lens _cdsTags (\ s a -> s{_cdsTags = a}) . _Default;

-- | The identifier for the source DB snapshot.
--
-- Constraints:
--
-- -   Must specify a valid system snapshot in the \"available\" state.
-- -   If the source snapshot is in the same region as the copy, specify a
--     valid DB snapshot identifier.
-- -   If the source snapshot is in a different region than the copy,
--     specify a valid DB snapshot ARN. For more information, go to
--     <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_CopySnapshot.html Copying a DB Snapshot>.
--
-- Example: @rds:mydb-2012-04-02-00-01@
--
-- Example:
-- @arn:aws:rds:rr-regn-1:123456789012:snapshot:mysql-instance1-snapshot-20130805@
cdsSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsSourceDBSnapshotIdentifier = lens _cdsSourceDBSnapshotIdentifier (\ s a -> s{_cdsSourceDBSnapshotIdentifier = a});

-- | The identifier for the copied snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-db-snapshot@
cdsTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsTargetDBSnapshotIdentifier = lens _cdsTargetDBSnapshotIdentifier (\ s a -> s{_cdsTargetDBSnapshotIdentifier = a});

instance AWSRequest CopyDBSnapshot where
        type Sv CopyDBSnapshot = RDS
        type Rs CopyDBSnapshot = CopyDBSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CopyDBSnapshotResult"
              (\ s h x ->
                 CopyDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure s))

instance ToHeaders CopyDBSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBSnapshot where
        toPath = const "/"

instance ToQuery CopyDBSnapshot where
        toQuery CopyDBSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdsTags),
               "SourceDBSnapshotIdentifier" =:
                 _cdsSourceDBSnapshotIdentifier,
               "TargetDBSnapshotIdentifier" =:
                 _cdsTargetDBSnapshotIdentifier]

-- | /See:/ 'copyDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsrDBSnapshot'
--
-- * 'cdsrStatus'
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
    { _cdsrDBSnapshot :: !(Maybe DBSnapshot)
    , _cdsrStatus     :: !Status
    } deriving (Eq,Read,Show)

-- | 'CopyDBSnapshotResponse' smart constructor.
copyDBSnapshotResponse :: Status -> CopyDBSnapshotResponse
copyDBSnapshotResponse pStatus =
    CopyDBSnapshotResponse'
    { _cdsrDBSnapshot = Nothing
    , _cdsrStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdsrDBSnapshot :: Lens' CopyDBSnapshotResponse (Maybe DBSnapshot)
cdsrDBSnapshot = lens _cdsrDBSnapshot (\ s a -> s{_cdsrDBSnapshot = a});

-- | FIXME: Undocumented member.
cdsrStatus :: Lens' CopyDBSnapshotResponse Status
cdsrStatus = lens _cdsrStatus (\ s a -> s{_cdsrStatus = a});
