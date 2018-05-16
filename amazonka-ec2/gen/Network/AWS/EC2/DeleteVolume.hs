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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified EBS volume. The volume must be in the @available@ state (not attached to an instance).
--
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-deleting-volume.html Deleting an Amazon EBS Volume> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.DeleteVolume
    (
    -- * Creating a Request
      deleteVolume
    , DeleteVolume
    -- * Request Lenses
    , dvvDryRun
    , dvvVolumeId

    -- * Destructuring the Response
    , deleteVolumeResponse
    , DeleteVolumeResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteVolume.
--
--
--
-- /See:/ 'deleteVolume' smart constructor.
data DeleteVolume = DeleteVolume'
  { _dvvDryRun   :: !(Maybe Bool)
  , _dvvVolumeId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dvvVolumeId' - The ID of the volume.
deleteVolume
    :: Text -- ^ 'dvvVolumeId'
    -> DeleteVolume
deleteVolume pVolumeId_ =
  DeleteVolume' {_dvvDryRun = Nothing, _dvvVolumeId = pVolumeId_}


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dvvDryRun :: Lens' DeleteVolume (Maybe Bool)
dvvDryRun = lens _dvvDryRun (\ s a -> s{_dvvDryRun = a})

-- | The ID of the volume.
dvvVolumeId :: Lens' DeleteVolume Text
dvvVolumeId = lens _dvvVolumeId (\ s a -> s{_dvvVolumeId = a})

instance AWSRequest DeleteVolume where
        type Rs DeleteVolume = DeleteVolumeResponse
        request = postQuery ec2
        response = receiveNull DeleteVolumeResponse'

instance Hashable DeleteVolume where

instance NFData DeleteVolume where

instance ToHeaders DeleteVolume where
        toHeaders = const mempty

instance ToPath DeleteVolume where
        toPath = const "/"

instance ToQuery DeleteVolume where
        toQuery DeleteVolume'{..}
          = mconcat
              ["Action" =: ("DeleteVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _dvvDryRun, "VolumeId" =: _dvvVolumeId]

-- | /See:/ 'deleteVolumeResponse' smart constructor.
data DeleteVolumeResponse =
  DeleteVolumeResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVolumeResponse' with the minimum fields required to make a request.
--
deleteVolumeResponse
    :: DeleteVolumeResponse
deleteVolumeResponse = DeleteVolumeResponse'


instance NFData DeleteVolumeResponse where
