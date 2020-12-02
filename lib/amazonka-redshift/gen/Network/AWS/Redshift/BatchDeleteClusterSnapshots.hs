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
-- Module      : Network.AWS.Redshift.BatchDeleteClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of cluster snapshots.
module Network.AWS.Redshift.BatchDeleteClusterSnapshots
  ( -- * Creating a Request
    batchDeleteClusterSnapshots,
    BatchDeleteClusterSnapshots,

    -- * Request Lenses
    bdcsIdentifiers,

    -- * Destructuring the Response
    batchDeleteClusterSnapshotsResponse,
    BatchDeleteClusterSnapshotsResponse,

    -- * Response Lenses
    bdcsrsResources,
    bdcsrsErrors,
    bdcsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'batchDeleteClusterSnapshots' smart constructor.
newtype BatchDeleteClusterSnapshots = BatchDeleteClusterSnapshots'
  { _bdcsIdentifiers ::
      [DeleteClusterSnapshotMessage]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteClusterSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdcsIdentifiers' - A list of identifiers for the snapshots that you want to delete.
batchDeleteClusterSnapshots ::
  BatchDeleteClusterSnapshots
batchDeleteClusterSnapshots =
  BatchDeleteClusterSnapshots' {_bdcsIdentifiers = mempty}

-- | A list of identifiers for the snapshots that you want to delete.
bdcsIdentifiers :: Lens' BatchDeleteClusterSnapshots [DeleteClusterSnapshotMessage]
bdcsIdentifiers = lens _bdcsIdentifiers (\s a -> s {_bdcsIdentifiers = a}) . _Coerce

instance AWSRequest BatchDeleteClusterSnapshots where
  type
    Rs BatchDeleteClusterSnapshots =
      BatchDeleteClusterSnapshotsResponse
  request = postQuery redshift
  response =
    receiveXMLWrapper
      "BatchDeleteClusterSnapshotsResult"
      ( \s h x ->
          BatchDeleteClusterSnapshotsResponse'
            <$> (x .@? "Resources" .!@ mempty >>= may (parseXMLList "String"))
            <*> ( x .@? "Errors" .!@ mempty
                    >>= may (parseXMLList "SnapshotErrorMessage")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable BatchDeleteClusterSnapshots

instance NFData BatchDeleteClusterSnapshots

instance ToHeaders BatchDeleteClusterSnapshots where
  toHeaders = const mempty

instance ToPath BatchDeleteClusterSnapshots where
  toPath = const "/"

instance ToQuery BatchDeleteClusterSnapshots where
  toQuery BatchDeleteClusterSnapshots' {..} =
    mconcat
      [ "Action" =: ("BatchDeleteClusterSnapshots" :: ByteString),
        "Version" =: ("2012-12-01" :: ByteString),
        "Identifiers"
          =: toQueryList "DeleteClusterSnapshotMessage" _bdcsIdentifiers
      ]

-- | /See:/ 'batchDeleteClusterSnapshotsResponse' smart constructor.
data BatchDeleteClusterSnapshotsResponse = BatchDeleteClusterSnapshotsResponse'
  { _bdcsrsResources ::
      !(Maybe [Text]),
    _bdcsrsErrors ::
      !( Maybe
           [SnapshotErrorMessage]
       ),
    _bdcsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'BatchDeleteClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdcsrsResources' - A list of the snapshot identifiers that were deleted.
--
-- * 'bdcsrsErrors' - A list of any errors returned.
--
-- * 'bdcsrsResponseStatus' - -- | The response status code.
batchDeleteClusterSnapshotsResponse ::
  -- | 'bdcsrsResponseStatus'
  Int ->
  BatchDeleteClusterSnapshotsResponse
batchDeleteClusterSnapshotsResponse pResponseStatus_ =
  BatchDeleteClusterSnapshotsResponse'
    { _bdcsrsResources = Nothing,
      _bdcsrsErrors = Nothing,
      _bdcsrsResponseStatus = pResponseStatus_
    }

-- | A list of the snapshot identifiers that were deleted.
bdcsrsResources :: Lens' BatchDeleteClusterSnapshotsResponse [Text]
bdcsrsResources = lens _bdcsrsResources (\s a -> s {_bdcsrsResources = a}) . _Default . _Coerce

-- | A list of any errors returned.
bdcsrsErrors :: Lens' BatchDeleteClusterSnapshotsResponse [SnapshotErrorMessage]
bdcsrsErrors = lens _bdcsrsErrors (\s a -> s {_bdcsrsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
bdcsrsResponseStatus :: Lens' BatchDeleteClusterSnapshotsResponse Int
bdcsrsResponseStatus = lens _bdcsrsResponseStatus (\s a -> s {_bdcsrsResponseStatus = a})

instance NFData BatchDeleteClusterSnapshotsResponse
