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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshots for the specified WorkSpace.
module Network.AWS.WorkSpaces.DescribeWorkspaceSnapshots
  ( -- * Creating a Request
    describeWorkspaceSnapshots,
    DescribeWorkspaceSnapshots,

    -- * Request Lenses
    dwsWorkspaceId,

    -- * Destructuring the Response
    describeWorkspaceSnapshotsResponse,
    DescribeWorkspaceSnapshotsResponse,

    -- * Response Lenses
    dwsrsRestoreSnapshots,
    dwsrsRebuildSnapshots,
    dwsrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types

-- | /See:/ 'describeWorkspaceSnapshots' smart constructor.
newtype DescribeWorkspaceSnapshots = DescribeWorkspaceSnapshots'
  { _dwsWorkspaceId ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkspaceSnapshots' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwsWorkspaceId' - The identifier of the WorkSpace.
describeWorkspaceSnapshots ::
  -- | 'dwsWorkspaceId'
  Text ->
  DescribeWorkspaceSnapshots
describeWorkspaceSnapshots pWorkspaceId_ =
  DescribeWorkspaceSnapshots' {_dwsWorkspaceId = pWorkspaceId_}

-- | The identifier of the WorkSpace.
dwsWorkspaceId :: Lens' DescribeWorkspaceSnapshots Text
dwsWorkspaceId = lens _dwsWorkspaceId (\s a -> s {_dwsWorkspaceId = a})

instance AWSRequest DescribeWorkspaceSnapshots where
  type
    Rs DescribeWorkspaceSnapshots =
      DescribeWorkspaceSnapshotsResponse
  request = postJSON workSpaces
  response =
    receiveJSON
      ( \s h x ->
          DescribeWorkspaceSnapshotsResponse'
            <$> (x .?> "RestoreSnapshots" .!@ mempty)
            <*> (x .?> "RebuildSnapshots" .!@ mempty)
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeWorkspaceSnapshots

instance NFData DescribeWorkspaceSnapshots

instance ToHeaders DescribeWorkspaceSnapshots where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("WorkspacesService.DescribeWorkspaceSnapshots" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DescribeWorkspaceSnapshots where
  toJSON DescribeWorkspaceSnapshots' {..} =
    object (catMaybes [Just ("WorkspaceId" .= _dwsWorkspaceId)])

instance ToPath DescribeWorkspaceSnapshots where
  toPath = const "/"

instance ToQuery DescribeWorkspaceSnapshots where
  toQuery = const mempty

-- | /See:/ 'describeWorkspaceSnapshotsResponse' smart constructor.
data DescribeWorkspaceSnapshotsResponse = DescribeWorkspaceSnapshotsResponse'
  { _dwsrsRestoreSnapshots ::
      !(Maybe [Snapshot]),
    _dwsrsRebuildSnapshots ::
      !(Maybe [Snapshot]),
    _dwsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeWorkspaceSnapshotsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwsrsRestoreSnapshots' - Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
--
-- * 'dwsrsRebuildSnapshots' - Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
--
-- * 'dwsrsResponseStatus' - -- | The response status code.
describeWorkspaceSnapshotsResponse ::
  -- | 'dwsrsResponseStatus'
  Int ->
  DescribeWorkspaceSnapshotsResponse
describeWorkspaceSnapshotsResponse pResponseStatus_ =
  DescribeWorkspaceSnapshotsResponse'
    { _dwsrsRestoreSnapshots =
        Nothing,
      _dwsrsRebuildSnapshots = Nothing,
      _dwsrsResponseStatus = pResponseStatus_
    }

-- | Information about the snapshots that can be used to restore a WorkSpace. These snapshots include both the root volume and the user volume.
dwsrsRestoreSnapshots :: Lens' DescribeWorkspaceSnapshotsResponse [Snapshot]
dwsrsRestoreSnapshots = lens _dwsrsRestoreSnapshots (\s a -> s {_dwsrsRestoreSnapshots = a}) . _Default . _Coerce

-- | Information about the snapshots that can be used to rebuild a WorkSpace. These snapshots include the user volume.
dwsrsRebuildSnapshots :: Lens' DescribeWorkspaceSnapshotsResponse [Snapshot]
dwsrsRebuildSnapshots = lens _dwsrsRebuildSnapshots (\s a -> s {_dwsrsRebuildSnapshots = a}) . _Default . _Coerce

-- | -- | The response status code.
dwsrsResponseStatus :: Lens' DescribeWorkspaceSnapshotsResponse Int
dwsrsResponseStatus = lens _dwsrsResponseStatus (\s a -> s {_dwsrsResponseStatus = a})

instance NFData DescribeWorkspaceSnapshotsResponse
