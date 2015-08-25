{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateDBSnapshot
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a DBSnapshot. The source DBInstance must be in \"available\"
-- state.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_CreateDBSnapshot.html AWS API Reference> for CreateDBSnapshot.
module Network.AWS.RDS.CreateDBSnapshot
    (
    -- * Creating a Request
      createDBSnapshot
    , CreateDBSnapshot
    -- * Request Lenses
    , cdbsTags
    , cdbsDBSnapshotIdentifier
    , cdbsDBInstanceIdentifier

    -- * Destructuring the Response
    , createDBSnapshotResponse
    , CreateDBSnapshotResponse
    -- * Response Lenses
    , cdbsrsDBSnapshot
    , cdbsrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'createDBSnapshot' smart constructor.
data CreateDBSnapshot = CreateDBSnapshot'
    { _cdbsTags                 :: !(Maybe [Tag])
    , _cdbsDBSnapshotIdentifier :: !Text
    , _cdbsDBInstanceIdentifier :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbsTags'
--
-- * 'cdbsDBSnapshotIdentifier'
--
-- * 'cdbsDBInstanceIdentifier'
createDBSnapshot
    :: Text -- ^ 'cdbsDBSnapshotIdentifier'
    -> Text -- ^ 'cdbsDBInstanceIdentifier'
    -> CreateDBSnapshot
createDBSnapshot pDBSnapshotIdentifier_ pDBInstanceIdentifier_ =
    CreateDBSnapshot'
    { _cdbsTags = Nothing
    , _cdbsDBSnapshotIdentifier = pDBSnapshotIdentifier_
    , _cdbsDBInstanceIdentifier = pDBInstanceIdentifier_
    }

-- | Undocumented member.
cdbsTags :: Lens' CreateDBSnapshot [Tag]
cdbsTags = lens _cdbsTags (\ s a -> s{_cdbsTags = a}) . _Default . _Coerce;

-- | The identifier for the DB snapshot.
--
-- Constraints:
--
-- -   Cannot be null, empty, or blank
-- -   Must contain from 1 to 255 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
--
-- Example: 'my-snapshot-id'
cdbsDBSnapshotIdentifier :: Lens' CreateDBSnapshot Text
cdbsDBSnapshotIdentifier = lens _cdbsDBSnapshotIdentifier (\ s a -> s{_cdbsDBSnapshotIdentifier = a});

-- | The DB instance identifier. This is the unique key that identifies a DB
-- instance.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
cdbsDBInstanceIdentifier :: Lens' CreateDBSnapshot Text
cdbsDBInstanceIdentifier = lens _cdbsDBInstanceIdentifier (\ s a -> s{_cdbsDBInstanceIdentifier = a});

instance AWSRequest CreateDBSnapshot where
        type Rs CreateDBSnapshot = CreateDBSnapshotResponse
        request = postQuery rDS
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
               "Tags" =: toQuery (toQueryList "Tag" <$> _cdbsTags),
               "DBSnapshotIdentifier" =: _cdbsDBSnapshotIdentifier,
               "DBInstanceIdentifier" =: _cdbsDBInstanceIdentifier]

-- | /See:/ 'createDBSnapshotResponse' smart constructor.
data CreateDBSnapshotResponse = CreateDBSnapshotResponse'
    { _cdbsrsDBSnapshot :: !(Maybe DBSnapshot)
    , _cdbsrsStatus     :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbsrsDBSnapshot'
--
-- * 'cdbsrsStatus'
createDBSnapshotResponse
    :: Int -- ^ 'cdbsrsStatus'
    -> CreateDBSnapshotResponse
createDBSnapshotResponse pStatus_ =
    CreateDBSnapshotResponse'
    { _cdbsrsDBSnapshot = Nothing
    , _cdbsrsStatus = pStatus_
    }

-- | Undocumented member.
cdbsrsDBSnapshot :: Lens' CreateDBSnapshotResponse (Maybe DBSnapshot)
cdbsrsDBSnapshot = lens _cdbsrsDBSnapshot (\ s a -> s{_cdbsrsDBSnapshot = a});

-- | The response status code.
cdbsrsStatus :: Lens' CreateDBSnapshotResponse Int
cdbsrsStatus = lens _cdbsrsStatus (\ s a -> s{_cdbsrsStatus = a});
