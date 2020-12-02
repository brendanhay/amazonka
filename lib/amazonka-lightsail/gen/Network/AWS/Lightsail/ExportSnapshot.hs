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
-- Module      : Network.AWS.Lightsail.ExportSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Lightsail instance or block storage disk snapshot to Amazon Elastic Compute Cloud (Amazon EC2). This operation results in an export snapshot record that can be used with the @create cloud formation stack@ operation to create new Amazon EC2 instances.
--
--
-- Exported instance snapshots appear in Amazon EC2 as Amazon Machine Images (AMIs), and the instance system disk appears as an Amazon Elastic Block Store (Amazon EBS) volume. Exported disk snapshots appear in Amazon EC2 as Amazon EBS volumes. Snapshots are exported to the same Amazon Web Services Region in Amazon EC2 as the source Lightsail snapshot.
--
--
--
-- The @export snapshot@ operation supports tag-based access control via resource tags applied to the resource identified by @source snapshot name@ . For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide> .
module Network.AWS.Lightsail.ExportSnapshot
  ( -- * Creating a Request
    exportSnapshot,
    ExportSnapshot,

    -- * Request Lenses
    esSourceSnapshotName,

    -- * Destructuring the Response
    exportSnapshotResponse,
    ExportSnapshotResponse,

    -- * Response Lenses
    esrsOperations,
    esrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportSnapshot' smart constructor.
newtype ExportSnapshot = ExportSnapshot'
  { _esSourceSnapshotName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportSnapshot' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esSourceSnapshotName' - The name of the instance or disk snapshot to be exported to Amazon EC2.
exportSnapshot ::
  -- | 'esSourceSnapshotName'
  Text ->
  ExportSnapshot
exportSnapshot pSourceSnapshotName_ =
  ExportSnapshot' {_esSourceSnapshotName = pSourceSnapshotName_}

-- | The name of the instance or disk snapshot to be exported to Amazon EC2.
esSourceSnapshotName :: Lens' ExportSnapshot Text
esSourceSnapshotName = lens _esSourceSnapshotName (\s a -> s {_esSourceSnapshotName = a})

instance AWSRequest ExportSnapshot where
  type Rs ExportSnapshot = ExportSnapshotResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          ExportSnapshotResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable ExportSnapshot

instance NFData ExportSnapshot

instance ToHeaders ExportSnapshot where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.ExportSnapshot" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON ExportSnapshot where
  toJSON ExportSnapshot' {..} =
    object
      (catMaybes [Just ("sourceSnapshotName" .= _esSourceSnapshotName)])

instance ToPath ExportSnapshot where
  toPath = const "/"

instance ToQuery ExportSnapshot where
  toQuery = const mempty

-- | /See:/ 'exportSnapshotResponse' smart constructor.
data ExportSnapshotResponse = ExportSnapshotResponse'
  { _esrsOperations ::
      !(Maybe [Operation]),
    _esrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportSnapshotResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'esrsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'esrsResponseStatus' - -- | The response status code.
exportSnapshotResponse ::
  -- | 'esrsResponseStatus'
  Int ->
  ExportSnapshotResponse
exportSnapshotResponse pResponseStatus_ =
  ExportSnapshotResponse'
    { _esrsOperations = Nothing,
      _esrsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
esrsOperations :: Lens' ExportSnapshotResponse [Operation]
esrsOperations = lens _esrsOperations (\s a -> s {_esrsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
esrsResponseStatus :: Lens' ExportSnapshotResponse Int
esrsResponseStatus = lens _esrsResponseStatus (\s a -> s {_esrsResponseStatus = a})

instance NFData ExportSnapshotResponse
