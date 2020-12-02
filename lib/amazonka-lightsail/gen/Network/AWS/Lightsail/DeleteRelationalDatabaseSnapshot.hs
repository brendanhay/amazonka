{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a database snapshot in Amazon Lightsail.
--
--
-- The @delete relational database snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by relationalDatabaseName. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteRelationalDatabaseSnapshot
  ( -- * Creating a Request
    deleteRelationalDatabaseSnapshot,
    DeleteRelationalDatabaseSnapshot,

    -- * Request Lenses
    drdsRelationalDatabaseSnapshotName,

    -- * Destructuring the Response
    deleteRelationalDatabaseSnapshotResponse,
    DeleteRelationalDatabaseSnapshotResponse,

    -- * Response Lenses
    drdsrsOperations,
    drdsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteRelationalDatabaseSnapshot' smart constructor.
newtype DeleteRelationalDatabaseSnapshot = DeleteRelationalDatabaseSnapshot'
  { _drdsRelationalDatabaseSnapshotName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRelationalDatabaseSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsRelationalDatabaseSnapshotName' - The name of the database snapshot that you are deleting.
deleteRelationalDatabaseSnapshot ::
  -- | 'drdsRelationalDatabaseSnapshotName'
  Text ->
  DeleteRelationalDatabaseSnapshot
deleteRelationalDatabaseSnapshot pRelationalDatabaseSnapshotName_ =
  DeleteRelationalDatabaseSnapshot'
    { _drdsRelationalDatabaseSnapshotName =
        pRelationalDatabaseSnapshotName_
    }

-- | The name of the database snapshot that you are deleting.
drdsRelationalDatabaseSnapshotName :: Lens' DeleteRelationalDatabaseSnapshot Text
drdsRelationalDatabaseSnapshotName = lens _drdsRelationalDatabaseSnapshotName (\s a -> s {_drdsRelationalDatabaseSnapshotName = a})

instance AWSRequest DeleteRelationalDatabaseSnapshot where
  type
    Rs DeleteRelationalDatabaseSnapshot =
      DeleteRelationalDatabaseSnapshotResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteRelationalDatabaseSnapshotResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteRelationalDatabaseSnapshot

instance NFData DeleteRelationalDatabaseSnapshot

instance ToHeaders DeleteRelationalDatabaseSnapshot where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "Lightsail_20161128.DeleteRelationalDatabaseSnapshot" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteRelationalDatabaseSnapshot where
  toJSON DeleteRelationalDatabaseSnapshot' {..} =
    object
      ( catMaybes
          [ Just
              ( "relationalDatabaseSnapshotName"
                  .= _drdsRelationalDatabaseSnapshotName
              )
          ]
      )

instance ToPath DeleteRelationalDatabaseSnapshot where
  toPath = const "/"

instance ToQuery DeleteRelationalDatabaseSnapshot where
  toQuery = const mempty

-- | /See:/ 'deleteRelationalDatabaseSnapshotResponse' smart constructor.
data DeleteRelationalDatabaseSnapshotResponse = DeleteRelationalDatabaseSnapshotResponse'
  { _drdsrsOperations ::
      !( Maybe
           [Operation]
       ),
    _drdsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteRelationalDatabaseSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drdsrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'drdsrsResponseStatus' - -- | The response status code.
deleteRelationalDatabaseSnapshotResponse ::
  -- | 'drdsrsResponseStatus'
  Int ->
  DeleteRelationalDatabaseSnapshotResponse
deleteRelationalDatabaseSnapshotResponse pResponseStatus_ =
  DeleteRelationalDatabaseSnapshotResponse'
    { _drdsrsOperations =
        Nothing,
      _drdsrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
drdsrsOperations :: Lens' DeleteRelationalDatabaseSnapshotResponse [Operation]
drdsrsOperations = lens _drdsrsOperations (\s a -> s {_drdsrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
drdsrsResponseStatus :: Lens' DeleteRelationalDatabaseSnapshotResponse Int
drdsrsResponseStatus = lens _drdsrsResponseStatus (\s a -> s {_drdsrsResponseStatus = a})

instance NFData DeleteRelationalDatabaseSnapshotResponse
