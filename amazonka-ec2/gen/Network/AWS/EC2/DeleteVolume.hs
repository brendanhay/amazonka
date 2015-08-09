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
-- Module      : Network.AWS.EC2.DeleteVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EBS volume. The volume must be in the @available@
-- state (not attached to an instance).
--
-- The volume may remain in the @deleting@ state for several minutes.
--
-- For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS Volume>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVolume.html AWS API Reference> for DeleteVolume.
module Network.AWS.EC2.DeleteVolume
    (
    -- * Creating a Request
      DeleteVolume
    , deleteVolume
    -- * Request Lenses
    , dvvDryRun
    , dvvVolumeId

    -- * Destructuring the Response
    , DeleteVolumeResponse
    , deleteVolumeResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvvDryRun'
--
-- * 'dvvVolumeId'
data DeleteVolume = DeleteVolume'
    { _dvvDryRun   :: !(Maybe Bool)
    , _dvvVolumeId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVolume' smart constructor.
deleteVolume :: Text -> DeleteVolume
deleteVolume pVolumeId_ =
    DeleteVolume'
    { _dvvDryRun = Nothing
    , _dvvVolumeId = pVolumeId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dvvDryRun :: Lens' DeleteVolume (Maybe Bool)
dvvDryRun = lens _dvvDryRun (\ s a -> s{_dvvDryRun = a});

-- | The ID of the volume.
dvvVolumeId :: Lens' DeleteVolume Text
dvvVolumeId = lens _dvvVolumeId (\ s a -> s{_dvvVolumeId = a});

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
               "DryRun" =: _dvvDryRun, "VolumeId" =: _dvvVolumeId]

-- | /See:/ 'deleteVolumeResponse' smart constructor.
data DeleteVolumeResponse =
    DeleteVolumeResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteVolumeResponse' smart constructor.
deleteVolumeResponse :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse'
