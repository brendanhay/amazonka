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
-- Module      : Network.AWS.RDS.DeleteDBSnapshot
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a DBSnapshot. If the snapshot is being copied, the copy operation is terminated.
--
--
module Network.AWS.RDS.DeleteDBSnapshot
    (
    -- * Creating a Request
      deleteDBSnapshot
    , DeleteDBSnapshot
    -- * Request Lenses
    , ddbsDBSnapshotIdentifier

    -- * Destructuring the Response
    , deleteDBSnapshotResponse
    , DeleteDBSnapshotResponse
    -- * Response Lenses
    , ddbsrsDBSnapshot
    , ddbsrsResponseStatus
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
-- /See:/ 'deleteDBSnapshot' smart constructor.
newtype DeleteDBSnapshot = DeleteDBSnapshot'
  { _ddbsDBSnapshotIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbsDBSnapshotIdentifier' - The DBSnapshot identifier. Constraints: Must be the name of an existing DB snapshot in the @available@ state.
deleteDBSnapshot
    :: Text -- ^ 'ddbsDBSnapshotIdentifier'
    -> DeleteDBSnapshot
deleteDBSnapshot pDBSnapshotIdentifier_ =
  DeleteDBSnapshot' {_ddbsDBSnapshotIdentifier = pDBSnapshotIdentifier_}


-- | The DBSnapshot identifier. Constraints: Must be the name of an existing DB snapshot in the @available@ state.
ddbsDBSnapshotIdentifier :: Lens' DeleteDBSnapshot Text
ddbsDBSnapshotIdentifier = lens _ddbsDBSnapshotIdentifier (\ s a -> s{_ddbsDBSnapshotIdentifier = a})

instance AWSRequest DeleteDBSnapshot where
        type Rs DeleteDBSnapshot = DeleteDBSnapshotResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "DeleteDBSnapshotResult"
              (\ s h x ->
                 DeleteDBSnapshotResponse' <$>
                   (x .@? "DBSnapshot") <*> (pure (fromEnum s)))

instance Hashable DeleteDBSnapshot where

instance NFData DeleteDBSnapshot where

instance ToHeaders DeleteDBSnapshot where
        toHeaders = const mempty

instance ToPath DeleteDBSnapshot where
        toPath = const "/"

instance ToQuery DeleteDBSnapshot where
        toQuery DeleteDBSnapshot'{..}
          = mconcat
              ["Action" =: ("DeleteDBSnapshot" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBSnapshotIdentifier" =: _ddbsDBSnapshotIdentifier]

-- | /See:/ 'deleteDBSnapshotResponse' smart constructor.
data DeleteDBSnapshotResponse = DeleteDBSnapshotResponse'
  { _ddbsrsDBSnapshot     :: !(Maybe DBSnapshot)
  , _ddbsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteDBSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddbsrsDBSnapshot' - Undocumented member.
--
-- * 'ddbsrsResponseStatus' - -- | The response status code.
deleteDBSnapshotResponse
    :: Int -- ^ 'ddbsrsResponseStatus'
    -> DeleteDBSnapshotResponse
deleteDBSnapshotResponse pResponseStatus_ =
  DeleteDBSnapshotResponse'
    {_ddbsrsDBSnapshot = Nothing, _ddbsrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ddbsrsDBSnapshot :: Lens' DeleteDBSnapshotResponse (Maybe DBSnapshot)
ddbsrsDBSnapshot = lens _ddbsrsDBSnapshot (\ s a -> s{_ddbsrsDBSnapshot = a})

-- | -- | The response status code.
ddbsrsResponseStatus :: Lens' DeleteDBSnapshotResponse Int
ddbsrsResponseStatus = lens _ddbsrsResponseStatus (\ s a -> s{_ddbsrsResponseStatus = a})

instance NFData DeleteDBSnapshotResponse where
