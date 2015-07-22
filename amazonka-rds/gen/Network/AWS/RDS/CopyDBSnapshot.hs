{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CopyDBSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Copies the specified DBSnapshot. The source DBSnapshot must be in the
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
    , cdsrqTags
    , cdsrqSourceDBSnapshotIdentifier
    , cdsrqTargetDBSnapshotIdentifier

    -- * Response
    , CopyDBSnapshotResponse
    -- ** Response constructor
    , copyDBSnapshotResponse
    -- ** Response lenses
    , cdsrsDBSnapshot
    , cdsrsStatus
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
-- * 'cdsrqTags'
--
-- * 'cdsrqSourceDBSnapshotIdentifier'
--
-- * 'cdsrqTargetDBSnapshotIdentifier'
data CopyDBSnapshot = CopyDBSnapshot'
    { _cdsrqTags                       :: !(Maybe [Tag])
    , _cdsrqSourceDBSnapshotIdentifier :: !Text
    , _cdsrqTargetDBSnapshotIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBSnapshot' smart constructor.
copyDBSnapshot :: Text -> Text -> CopyDBSnapshot
copyDBSnapshot pSourceDBSnapshotIdentifier pTargetDBSnapshotIdentifier =
    CopyDBSnapshot'
    { _cdsrqTags = Nothing
    , _cdsrqSourceDBSnapshotIdentifier = pSourceDBSnapshotIdentifier
    , _cdsrqTargetDBSnapshotIdentifier = pTargetDBSnapshotIdentifier
    }

-- | FIXME: Undocumented member.
cdsrqTags :: Lens' CopyDBSnapshot [Tag]
cdsrqTags = lens _cdsrqTags (\ s a -> s{_cdsrqTags = a}) . _Default;

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
cdsrqSourceDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsrqSourceDBSnapshotIdentifier = lens _cdsrqSourceDBSnapshotIdentifier (\ s a -> s{_cdsrqSourceDBSnapshotIdentifier = a});

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
cdsrqTargetDBSnapshotIdentifier :: Lens' CopyDBSnapshot Text
cdsrqTargetDBSnapshotIdentifier = lens _cdsrqTargetDBSnapshotIdentifier (\ s a -> s{_cdsrqTargetDBSnapshotIdentifier = a});

instance AWSRequest CopyDBSnapshot where
        type Sv CopyDBSnapshot = RDS
        type Rs CopyDBSnapshot = CopyDBSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CopyDBSnapshotResult"
              (\ s h x ->
                 CopyDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders CopyDBSnapshot where
        toHeaders = const mempty

instance ToPath CopyDBSnapshot where
        toPath = const "/"

instance ToQuery CopyDBSnapshot where
        toQuery CopyDBSnapshot'{..}
          = mconcat
              ["Action" =: ("CopyDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdsrqTags),
               "SourceDBSnapshotIdentifier" =:
                 _cdsrqSourceDBSnapshotIdentifier,
               "TargetDBSnapshotIdentifier" =:
                 _cdsrqTargetDBSnapshotIdentifier]

-- | /See:/ 'copyDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdsrsDBSnapshot'
--
-- * 'cdsrsStatus'
data CopyDBSnapshotResponse = CopyDBSnapshotResponse'
    { _cdsrsDBSnapshot :: !(Maybe DBSnapshot)
    , _cdsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CopyDBSnapshotResponse' smart constructor.
copyDBSnapshotResponse :: Int -> CopyDBSnapshotResponse
copyDBSnapshotResponse pStatus =
    CopyDBSnapshotResponse'
    { _cdsrsDBSnapshot = Nothing
    , _cdsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdsrsDBSnapshot :: Lens' CopyDBSnapshotResponse (Maybe DBSnapshot)
cdsrsDBSnapshot = lens _cdsrsDBSnapshot (\ s a -> s{_cdsrsDBSnapshot = a});

-- | FIXME: Undocumented member.
cdsrsStatus :: Lens' CopyDBSnapshotResponse Int
cdsrsStatus = lens _cdsrsStatus (\ s a -> s{_cdsrsStatus = a});
