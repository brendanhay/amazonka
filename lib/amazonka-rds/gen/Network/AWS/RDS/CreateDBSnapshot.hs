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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a DBSnapshot. The source DBInstance must be in "available" state.
--
--
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
    , cdbsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'createDBSnapshot' smart constructor.
data CreateDBSnapshot = CreateDBSnapshot'
  { _cdbsTags                 :: !(Maybe [Tag])
  , _cdbsDBSnapshotIdentifier :: !Text
  , _cdbsDBInstanceIdentifier :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbsTags' - Undocumented member.
--
-- * 'cdbsDBSnapshotIdentifier' - The identifier for the DB snapshot. Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
--
-- * 'cdbsDBInstanceIdentifier' - The identifier of the DB instance that you want to create the snapshot of. Constraints:     * Must match the identifier of an existing DBInstance.
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
cdbsTags = lens _cdbsTags (\ s a -> s{_cdbsTags = a}) . _Default . _Coerce

-- | The identifier for the DB snapshot. Constraints:     * Cannot be null, empty, or blank     * Must contain from 1 to 255 letters, numbers, or hyphens     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens Example: @my-snapshot-id@
cdbsDBSnapshotIdentifier :: Lens' CreateDBSnapshot Text
cdbsDBSnapshotIdentifier = lens _cdbsDBSnapshotIdentifier (\ s a -> s{_cdbsDBSnapshotIdentifier = a})

-- | The identifier of the DB instance that you want to create the snapshot of. Constraints:     * Must match the identifier of an existing DBInstance.
cdbsDBInstanceIdentifier :: Lens' CreateDBSnapshot Text
cdbsDBInstanceIdentifier = lens _cdbsDBInstanceIdentifier (\ s a -> s{_cdbsDBInstanceIdentifier = a})

instance AWSRequest CreateDBSnapshot where
        type Rs CreateDBSnapshot = CreateDBSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "CreateDBSnapshotResult"
              (\ s h x ->
                 CreateDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance Hashable CreateDBSnapshot where

instance NFData CreateDBSnapshot where

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
  { _cdbsrsDBSnapshot     :: !(Maybe DBSnapshot)
  , _cdbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdbsrsDBSnapshot' - Undocumented member.
--
-- * 'cdbsrsResponseStatus' - -- | The response status code.
createDBSnapshotResponse
    :: Int -- ^ 'cdbsrsResponseStatus'
    -> CreateDBSnapshotResponse
createDBSnapshotResponse pResponseStatus_ =
  CreateDBSnapshotResponse'
    {_cdbsrsDBSnapshot = Nothing, _cdbsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
cdbsrsDBSnapshot :: Lens' CreateDBSnapshotResponse (Maybe DBSnapshot)
cdbsrsDBSnapshot = lens _cdbsrsDBSnapshot (\ s a -> s{_cdbsrsDBSnapshot = a})

-- | -- | The response status code.
cdbsrsResponseStatus :: Lens' CreateDBSnapshotResponse Int
cdbsrsResponseStatus = lens _cdbsrsResponseStatus (\ s a -> s{_cdbsrsResponseStatus = a})

instance NFData CreateDBSnapshotResponse where
