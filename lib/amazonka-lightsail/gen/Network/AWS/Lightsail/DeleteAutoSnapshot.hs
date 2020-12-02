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
-- Module      : Network.AWS.Lightsail.DeleteAutoSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an automatic snapshot of an instance or disk. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-configuring-automatic-snapshots Lightsail Dev Guide> .
module Network.AWS.Lightsail.DeleteAutoSnapshot
  ( -- * Creating a Request
    deleteAutoSnapshot,
    DeleteAutoSnapshot,

    -- * Request Lenses
    dasResourceName,
    dasDate,

    -- * Destructuring the Response
    deleteAutoSnapshotResponse,
    DeleteAutoSnapshotResponse,

    -- * Response Lenses
    dasrsOperations,
    dasrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAutoSnapshot' smart constructor.
data DeleteAutoSnapshot = DeleteAutoSnapshot'
  { _dasResourceName ::
      !Text,
    _dasDate :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAutoSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasResourceName' - The name of the source instance or disk from which to delete the automatic snapshot.
--
-- * 'dasDate' - The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
deleteAutoSnapshot ::
  -- | 'dasResourceName'
  Text ->
  -- | 'dasDate'
  Text ->
  DeleteAutoSnapshot
deleteAutoSnapshot pResourceName_ pDate_ =
  DeleteAutoSnapshot'
    { _dasResourceName = pResourceName_,
      _dasDate = pDate_
    }

-- | The name of the source instance or disk from which to delete the automatic snapshot.
dasResourceName :: Lens' DeleteAutoSnapshot Text
dasResourceName = lens _dasResourceName (\s a -> s {_dasResourceName = a})

-- | The date of the automatic snapshot to delete in @YYYY-MM-DD@ format. Use the @get auto snapshots@ operation to get the available automatic snapshots for a resource.
dasDate :: Lens' DeleteAutoSnapshot Text
dasDate = lens _dasDate (\s a -> s {_dasDate = a})

instance AWSRequest DeleteAutoSnapshot where
  type Rs DeleteAutoSnapshot = DeleteAutoSnapshotResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteAutoSnapshotResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteAutoSnapshot

instance NFData DeleteAutoSnapshot

instance ToHeaders DeleteAutoSnapshot where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteAutoSnapshot" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAutoSnapshot where
  toJSON DeleteAutoSnapshot' {..} =
    object
      ( catMaybes
          [ Just ("resourceName" .= _dasResourceName),
            Just ("date" .= _dasDate)
          ]
      )

instance ToPath DeleteAutoSnapshot where
  toPath = const "/"

instance ToQuery DeleteAutoSnapshot where
  toQuery = const mempty

-- | /See:/ 'deleteAutoSnapshotResponse' smart constructor.
data DeleteAutoSnapshotResponse = DeleteAutoSnapshotResponse'
  { _dasrsOperations ::
      !(Maybe [Operation]),
    _dasrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAutoSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dasrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'dasrsResponseStatus' - -- | The response status code.
deleteAutoSnapshotResponse ::
  -- | 'dasrsResponseStatus'
  Int ->
  DeleteAutoSnapshotResponse
deleteAutoSnapshotResponse pResponseStatus_ =
  DeleteAutoSnapshotResponse'
    { _dasrsOperations = Nothing,
      _dasrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
dasrsOperations :: Lens' DeleteAutoSnapshotResponse [Operation]
dasrsOperations = lens _dasrsOperations (\s a -> s {_dasrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
dasrsResponseStatus :: Lens' DeleteAutoSnapshotResponse Int
dasrsResponseStatus = lens _dasrsResponseStatus (\s a -> s {_dasrsResponseStatus = a})

instance NFData DeleteAutoSnapshotResponse
