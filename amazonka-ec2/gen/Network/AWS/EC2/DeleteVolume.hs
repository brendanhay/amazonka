{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified EBS volume. The volume must be in the @available@
-- state (not attached to an instance).
--
-- The volume may remain in the @deleting@ state for several minutes.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS Volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html>
module Network.AWS.EC2.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , dv2DryRun
    , dv2VolumeId

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dv2DryRun'
--
-- * 'dv2VolumeId'
data DeleteVolume = DeleteVolume'{_dv2DryRun :: Maybe Bool, _dv2VolumeId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteVolume' smart constructor.
deleteVolume :: Text -> DeleteVolume
deleteVolume pVolumeId = DeleteVolume'{_dv2DryRun = Nothing, _dv2VolumeId = pVolumeId};

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dv2DryRun :: Lens' DeleteVolume (Maybe Bool)
dv2DryRun = lens _dv2DryRun (\ s a -> s{_dv2DryRun = a});

-- | The ID of the volume.
dv2VolumeId :: Lens' DeleteVolume Text
dv2VolumeId = lens _dv2VolumeId (\ s a -> s{_dv2VolumeId = a});

instance AWSRequest DeleteVolume where
        type Sv DeleteVolume = EC2
        type Rs DeleteVolume = DeleteVolumeResponse
        request = post
        response = receiveNull DeleteVolumeResponse'

instance ToHeaders DeleteVolume where
        toHeaders = const mempty

instance ToPath DeleteVolume where
        toPath = const "/"

instance ToQuery DeleteVolume where
        toQuery DeleteVolume'{..}
          = mconcat
              ["Action" =: ("DeleteVolume" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _dv2DryRun, "VolumeId" =: _dv2VolumeId]

-- | /See:/ 'deleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse' deriving (Eq, Read, Show)

-- | 'DeleteVolumeResponse' smart constructor.
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse';
