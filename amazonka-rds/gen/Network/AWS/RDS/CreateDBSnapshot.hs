{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a DBSnapshot. The source DBInstance must be in \"available\"
-- state.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSnapshot.html>
module Network.AWS.RDS.CreateDBSnapshot
    (
    -- * Request
      CreateDBSnapshot
    -- ** Request constructor
    , createDBSnapshot
    -- ** Request lenses
    , cdbsrqTags
    , cdbsrqDBSnapshotIdentifier
    , cdbsrqDBInstanceIdentifier

    -- * Response
    , CreateDBSnapshotResponse
    -- ** Response constructor
    , createDBSnapshotResponse
    -- ** Response lenses
    , cdbsrsDBSnapshot
    , cdbsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsrqTags'
--
-- * 'cdbsrqDBSnapshotIdentifier'
--
-- * 'cdbsrqDBInstanceIdentifier'
data CreateDBSnapshot = CreateDBSnapshot'
    { _cdbsrqTags                 :: !(Maybe [Tag])
    , _cdbsrqDBSnapshotIdentifier :: !Text
    , _cdbsrqDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSnapshot' smart constructor.
createDBSnapshot :: Text -> Text -> CreateDBSnapshot
createDBSnapshot pDBSnapshotIdentifier pDBInstanceIdentifier =
    CreateDBSnapshot'
    { _cdbsrqTags = Nothing
    , _cdbsrqDBSnapshotIdentifier = pDBSnapshotIdentifier
    , _cdbsrqDBInstanceIdentifier = pDBInstanceIdentifier
    }

-- | FIXME: Undocumented member.
cdbsrqTags :: Lens' CreateDBSnapshot [Tag]
cdbsrqTags = lens _cdbsrqTags (\ s a -> s{_cdbsrqTags = a}) . _Default;

-- | The identifier for the DB snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: @my-snapshot-id@
cdbsrqDBSnapshotIdentifier :: Lens' CreateDBSnapshot Text
cdbsrqDBSnapshotIdentifier = lens _cdbsrqDBSnapshotIdentifier (\ s a -> s{_cdbsrqDBSnapshotIdentifier = a});

-- | The DB instance identifier. This is the unique key that identifies a DB
-- instance.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
cdbsrqDBInstanceIdentifier :: Lens' CreateDBSnapshot Text
cdbsrqDBInstanceIdentifier = lens _cdbsrqDBInstanceIdentifier (\ s a -> s{_cdbsrqDBInstanceIdentifier = a});

instance AWSRequest CreateDBSnapshot where
        type Sv CreateDBSnapshot = RDS
        type Rs CreateDBSnapshot = CreateDBSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBSnapshotResult"
              (\ s h x ->
                 CreateDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance ToHeaders CreateDBSnapshot where
        toHeaders = const mempty

instance ToPath CreateDBSnapshot where
        toPath = const "/"

instance ToQuery CreateDBSnapshot where
        toQuery CreateDBSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =:
                 toQuery (toQueryList "Tag" <$> _cdbsrqTags),
               "DBSnapshotIdentifier" =:
                 _cdbsrqDBSnapshotIdentifier,
               "DBInstanceIdentifier" =:
                 _cdbsrqDBInstanceIdentifier]

-- | /See:/ 'createDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cdbsrsDBSnapshot'
--
-- * 'cdbsrsStatus'
data CreateDBSnapshotResponse = CreateDBSnapshotResponse'
    { _cdbsrsDBSnapshot :: !(Maybe DBSnapshot)
    , _cdbsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateDBSnapshotResponse' smart constructor.
createDBSnapshotResponse :: Int -> CreateDBSnapshotResponse
createDBSnapshotResponse pStatus =
    CreateDBSnapshotResponse'
    { _cdbsrsDBSnapshot = Nothing
    , _cdbsrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
cdbsrsDBSnapshot :: Lens' CreateDBSnapshotResponse (Maybe DBSnapshot)
cdbsrsDBSnapshot = lens _cdbsrsDBSnapshot (\ s a -> s{_cdbsrsDBSnapshot = a});

-- | FIXME: Undocumented member.
cdbsrsStatus :: Lens' CreateDBSnapshotResponse Int
cdbsrsStatus = lens _cdbsrsStatus (\ s a -> s{_cdbsrsStatus = a});
