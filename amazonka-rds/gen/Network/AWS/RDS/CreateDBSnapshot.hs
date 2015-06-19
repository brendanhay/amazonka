{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.RDS.CreateDBSnapshot
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

-- | Creates a DBSnapshot. The source DBInstance must be in \"available\"
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
    , creTags
    , creDBSnapshotIdentifier
    , creDBInstanceIdentifier

    -- * Response
    , CreateDBSnapshotResponse
    -- ** Response constructor
    , createDBSnapshotResponse
    -- ** Response lenses
    , creDBSnapshot
    ) where

import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDBSnapshot' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creTags'
--
-- * 'creDBSnapshotIdentifier'
--
-- * 'creDBInstanceIdentifier'
data CreateDBSnapshot = CreateDBSnapshot'{_creTags :: Maybe [Tag], _creDBSnapshotIdentifier :: Text, _creDBInstanceIdentifier :: Text} deriving (Eq, Read, Show)

-- | 'CreateDBSnapshot' smart constructor.
createDBSnapshot :: Text -> Text -> CreateDBSnapshot
createDBSnapshot pDBSnapshotIdentifier pDBInstanceIdentifier = CreateDBSnapshot'{_creTags = Nothing, _creDBSnapshotIdentifier = pDBSnapshotIdentifier, _creDBInstanceIdentifier = pDBInstanceIdentifier};

-- | FIXME: Undocumented member.
creTags :: Lens' CreateDBSnapshot [Tag]
creTags = lens _creTags (\ s a -> s{_creTags = a}) . _Default;

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
creDBSnapshotIdentifier :: Lens' CreateDBSnapshot Text
creDBSnapshotIdentifier = lens _creDBSnapshotIdentifier (\ s a -> s{_creDBSnapshotIdentifier = a});

-- | The DB instance identifier. This is the unique key that identifies a DB
-- instance.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
creDBInstanceIdentifier :: Lens' CreateDBSnapshot Text
creDBInstanceIdentifier = lens _creDBInstanceIdentifier (\ s a -> s{_creDBInstanceIdentifier = a});

instance AWSRequest CreateDBSnapshot where
        type Sv CreateDBSnapshot = RDS
        type Rs CreateDBSnapshot = CreateDBSnapshotResponse
        request = post
        response
          = receiveXMLWrapper "CreateDBSnapshotResult"
              (\ s h x ->
                 CreateDBSnapshotResponse' <$> (x .@? "DBSnapshot"))

instance ToHeaders CreateDBSnapshot where
        toHeaders = const mempty

instance ToPath CreateDBSnapshot where
        toPath = const "/"

instance ToQuery CreateDBSnapshot where
        toQuery CreateDBSnapshot'{..}
          = mconcat
              ["Action" =: ("CreateDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "Tags" =: toQuery (toQueryList "Tag" <$> _creTags),
               "DBSnapshotIdentifier" =: _creDBSnapshotIdentifier,
               "DBInstanceIdentifier" =: _creDBInstanceIdentifier]

-- | /See:/ 'createDBSnapshotResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'creDBSnapshot'
newtype CreateDBSnapshotResponse = CreateDBSnapshotResponse'{_creDBSnapshot :: Maybe DBSnapshot} deriving (Eq, Read, Show)

-- | 'CreateDBSnapshotResponse' smart constructor.
createDBSnapshotResponse :: CreateDBSnapshotResponse
createDBSnapshotResponse = CreateDBSnapshotResponse'{_creDBSnapshot = Nothing};

-- | FIXME: Undocumented member.
creDBSnapshot :: Lens' CreateDBSnapshotResponse (Maybe DBSnapshot)
creDBSnapshot = lens _creDBSnapshot (\ s a -> s{_creDBSnapshot = a});
