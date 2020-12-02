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
-- Module      : Network.AWS.EC2.EnableFastSnapshotRestores
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables fast snapshot restores for the specified snapshots in the specified Availability Zones.
--
--
-- You get the full benefit of fast snapshot restores after they enter the @enabled@ state. To get the current state of fast snapshot restores, use 'DescribeFastSnapshotRestores' . To disable fast snapshot restores, use 'DisableFastSnapshotRestores' .
--
-- For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-fast-snapshot-restore.html Amazon EBS fast snapshot restore> in the /Amazon Elastic Compute Cloud User Guide/ .
module Network.AWS.EC2.EnableFastSnapshotRestores
  ( -- * Creating a Request
    enableFastSnapshotRestores,
    EnableFastSnapshotRestores,

    -- * Request Lenses
    efsrDryRun,
    efsrAvailabilityZones,
    efsrSourceSnapshotIds,

    -- * Destructuring the Response
    enableFastSnapshotRestoresResponse,
    EnableFastSnapshotRestoresResponse,

    -- * Response Lenses
    efsrrsUnsuccessful,
    efsrrsSuccessful,
    efsrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'enableFastSnapshotRestores' smart constructor.
data EnableFastSnapshotRestores = EnableFastSnapshotRestores'
  { _efsrDryRun ::
      !(Maybe Bool),
    _efsrAvailabilityZones :: ![Text],
    _efsrSourceSnapshotIds :: ![Text]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableFastSnapshotRestores' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'efsrAvailabilityZones' - One or more Availability Zones. For example, @us-east-2a@ .
--
-- * 'efsrSourceSnapshotIds' - The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ . You can specify a snapshot that was shared with you from another AWS account.
enableFastSnapshotRestores ::
  EnableFastSnapshotRestores
enableFastSnapshotRestores =
  EnableFastSnapshotRestores'
    { _efsrDryRun = Nothing,
      _efsrAvailabilityZones = mempty,
      _efsrSourceSnapshotIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
efsrDryRun :: Lens' EnableFastSnapshotRestores (Maybe Bool)
efsrDryRun = lens _efsrDryRun (\s a -> s {_efsrDryRun = a})

-- | One or more Availability Zones. For example, @us-east-2a@ .
efsrAvailabilityZones :: Lens' EnableFastSnapshotRestores [Text]
efsrAvailabilityZones = lens _efsrAvailabilityZones (\s a -> s {_efsrAvailabilityZones = a}) . _Coerce

-- | The IDs of one or more snapshots. For example, @snap-1234567890abcdef0@ . You can specify a snapshot that was shared with you from another AWS account.
efsrSourceSnapshotIds :: Lens' EnableFastSnapshotRestores [Text]
efsrSourceSnapshotIds = lens _efsrSourceSnapshotIds (\s a -> s {_efsrSourceSnapshotIds = a}) . _Coerce

instance AWSRequest EnableFastSnapshotRestores where
  type
    Rs EnableFastSnapshotRestores =
      EnableFastSnapshotRestoresResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          EnableFastSnapshotRestoresResponse'
            <$> (x .@? "unsuccessful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "successful" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable EnableFastSnapshotRestores

instance NFData EnableFastSnapshotRestores

instance ToHeaders EnableFastSnapshotRestores where
  toHeaders = const mempty

instance ToPath EnableFastSnapshotRestores where
  toPath = const "/"

instance ToQuery EnableFastSnapshotRestores where
  toQuery EnableFastSnapshotRestores' {..} =
    mconcat
      [ "Action" =: ("EnableFastSnapshotRestores" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "DryRun" =: _efsrDryRun,
        toQueryList "AvailabilityZone" _efsrAvailabilityZones,
        toQueryList "SourceSnapshotId" _efsrSourceSnapshotIds
      ]

-- | /See:/ 'enableFastSnapshotRestoresResponse' smart constructor.
data EnableFastSnapshotRestoresResponse = EnableFastSnapshotRestoresResponse'
  { _efsrrsUnsuccessful ::
      !( Maybe
           [EnableFastSnapshotRestoreErrorItem]
       ),
    _efsrrsSuccessful ::
      !( Maybe
           [EnableFastSnapshotRestoreSuccessItem]
       ),
    _efsrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EnableFastSnapshotRestoresResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'efsrrsUnsuccessful' - Information about the snapshots for which fast snapshot restores could not be enabled.
--
-- * 'efsrrsSuccessful' - Information about the snapshots for which fast snapshot restores were successfully enabled.
--
-- * 'efsrrsResponseStatus' - -- | The response status code.
enableFastSnapshotRestoresResponse ::
  -- | 'efsrrsResponseStatus'
  Int ->
  EnableFastSnapshotRestoresResponse
enableFastSnapshotRestoresResponse pResponseStatus_ =
  EnableFastSnapshotRestoresResponse'
    { _efsrrsUnsuccessful =
        Nothing,
      _efsrrsSuccessful = Nothing,
      _efsrrsResponseStatus = pResponseStatus_
    }

-- | Information about the snapshots for which fast snapshot restores could not be enabled.
efsrrsUnsuccessful :: Lens' EnableFastSnapshotRestoresResponse [EnableFastSnapshotRestoreErrorItem]
efsrrsUnsuccessful = lens _efsrrsUnsuccessful (\s a -> s {_efsrrsUnsuccessful = a}) . _Default . _Coerce

-- | Information about the snapshots for which fast snapshot restores were successfully enabled.
efsrrsSuccessful :: Lens' EnableFastSnapshotRestoresResponse [EnableFastSnapshotRestoreSuccessItem]
efsrrsSuccessful = lens _efsrrsSuccessful (\s a -> s {_efsrrsSuccessful = a}) . _Default . _Coerce

-- | -- | The response status code.
efsrrsResponseStatus :: Lens' EnableFastSnapshotRestoresResponse Int
efsrrsResponseStatus = lens _efsrrsResponseStatus (\s a -> s {_efsrrsResponseStatus = a})

instance NFData EnableFastSnapshotRestoresResponse
