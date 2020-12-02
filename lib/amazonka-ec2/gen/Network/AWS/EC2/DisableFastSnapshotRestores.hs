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
-- Module      : Network.AWS.EC2.DisableFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables fast snapshot restores for the specified snapshots in the specified Availability Zones.
module Network.AWS.EC2.DisableFastSnapshotRestores
  ( -- * Creating a Request
    disableFastSnapshotRestores,
    DisableFastSnapshotRestores,

    -- * Request Lenses
    dfsrsDryRun,
    dfsrsAvailabilityZones,
    dfsrsSourceSnapshotIds,

    -- * Destructuring the Response
    disableFastSnapshotRestoresResponse,
    DisableFastSnapshotRestoresResponse,

    -- * Response Lenses
    dfsrrsUnsuccessful,
    dfsrrsSuccessful,
    dfsrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disableFastSnapshotRestores' smart constructor.
data DisableFastSnapshotRestores = DisableFastSnapshotRestores'
  { _dfsrsDryRun ::
      !(Maybe Bool),
    _dfsrsAvailabilityZones :: ![Text],
    _dfsrsSourceSnapshotIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableFastSnapshotRestores' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfsrsAvailabilityZones' - One or more Availability Zones. For example, @us-east-2a@ .
--
-- * 'dfsrsSourceSnapshotIds' - The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
disableFastSnapshotRestores ::
  DisableFastSnapshotRestores
disableFastSnapshotRestores =
  DisableFastSnapshotRestores'
    { _dfsrsDryRun = Nothing,
      _dfsrsAvailabilityZones = mempty,
      _dfsrsSourceSnapshotIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfsrsDryRun :: Lens' DisableFastSnapshotRestores (Maybe Bool)
dfsrsDryRun = lens _dfsrsDryRun (\s a -> s {_dfsrsDryRun = a})

-- | One or more Availability Zones. For example, @us-east-2a@ .
dfsrsAvailabilityZones :: Lens' DisableFastSnapshotRestores [Text]
dfsrsAvailabilityZones = lens _dfsrsAvailabilityZones (\s a -> s {_dfsrsAvailabilityZones = a}) . _Coerce

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ .
dfsrsSourceSnapshotIds :: Lens' DisableFastSnapshotRestores [Text]
dfsrsSourceSnapshotIds = lens _dfsrsSourceSnapshotIds (\s a -> s {_dfsrsSourceSnapshotIds = a}) . _Coerce

instance AWSRequest DisableFastSnapshotRestores where
  type
    Rs DisableFastSnapshotRestores =
      DisableFastSnapshotRestoresResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DisableFastSnapshotRestoresResponse'
            <$> (x .@? "unsuccessful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "successful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DisableFastSnapshotRestores

instance NFData DisableFastSnapshotRestores

instance ToHeaders DisableFastSnapshotRestores where
  toHeaders = const mempty

instance ToPath DisableFastSnapshotRestores where
  toPath = const "/"

instance ToQuery DisableFastSnapshotRestores where
  toQuery DisableFastSnapshotRestores' {..} =
    mconcat
      [ "Action" =: ("DisableFastSnapshotRestores" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _dfsrsDryRun,
        toQueryList "AvailabilityZone" _dfsrsAvailabilityZones,
        toQueryList "SourceSnapshotId" _dfsrsSourceSnapshotIds
      ]

-- | /See:/ 'disableFastSnapshotRestoresResponse' smart constructor.
data DisableFastSnapshotRestoresResponse = DisableFastSnapshotRestoresResponse'
  { _dfsrrsUnsuccessful ::
      !( Maybe
           [DisableFastSnapshotRestoreErrorItem]
       ),
    _dfsrrsSuccessful ::
      !( Maybe
           [DisableFastSnapshotRestoreSuccessItem]
       ),
    _dfsrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisableFastSnapshotRestoresResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrrsUnsuccessful' - Information about the snapshots for which fast snapshot restores could not be disabled.
--
-- * 'dfsrrsSuccessful' - Information about the snapshots for which fast snapshot restores were successfully disabled.
--
-- * 'dfsrrsResponseStatus' - -- | The response status code.
disableFastSnapshotRestoresResponse ::
  -- | 'dfsrrsResponseStatus'
  Int ->
  DisableFastSnapshotRestoresResponse
disableFastSnapshotRestoresResponse pResponseStatus_ =
  DisableFastSnapshotRestoresResponse'
    { _dfsrrsUnsuccessful =
        Nothing,
      _dfsrrsSuccessful = Nothing,
      _dfsrrsResponseStatus = pResponseStatus_
    }

-- | Information about the snapshots for which fast snapshot restores could not be disabled.
dfsrrsUnsuccessful :: Lens' DisableFastSnapshotRestoresResponse [DisableFastSnapshotRestoreErrorItem]
dfsrrsUnsuccessful = lens _dfsrrsUnsuccessful (\s a -> s {_dfsrrsUnsuccessful = a}) . _Default . _Coerce

-- | Information about the snapshots for which fast snapshot restores were successfully disabled.
dfsrrsSuccessful :: Lens' DisableFastSnapshotRestoresResponse [DisableFastSnapshotRestoreSuccessItem]
dfsrrsSuccessful = lens _dfsrrsSuccessful (\s a -> s {_dfsrrsSuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
dfsrrsResponseStatus :: Lens' DisableFastSnapshotRestoresResponse Int
dfsrrsResponseStatus = lens _dfsrrsResponseStatus (\s a -> s {_dfsrrsResponseStatus = a})

instance NFData DisableFastSnapshotRestoresResponse
