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
-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an import volume task using metadata from the specified disk image. After importing the image, you then upload it using the 'ec2-import-volume' command in the Amazon EC2 command-line interface (CLI) tools. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- For information about the import manifest referenced by this API action, see <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/manifest.html VM Import Manifest>.
module Network.AWS.EC2.ImportVolume
    (
    -- * Creating a Request
      importVolume
    , ImportVolume
    -- * Request Lenses
    , ivDescription
    , ivDryRun
    , ivAvailabilityZone
    , ivImage
    , ivVolume

    -- * Destructuring the Response
    , importVolumeResponse
    , ImportVolumeResponse
    -- * Response Lenses
    , ivrsConversionTask
    , ivrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for ImportVolume.
--
-- /See:/ 'importVolume' smart constructor.
data ImportVolume = ImportVolume'
    { _ivDescription      :: !(Maybe Text)
    , _ivDryRun           :: !(Maybe Bool)
    , _ivAvailabilityZone :: !Text
    , _ivImage            :: !DiskImageDetail
    , _ivVolume           :: !VolumeDetail
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivDescription'
--
-- * 'ivDryRun'
--
-- * 'ivAvailabilityZone'
--
-- * 'ivImage'
--
-- * 'ivVolume'
importVolume
    :: Text -- ^ 'ivAvailabilityZone'
    -> DiskImageDetail -- ^ 'ivImage'
    -> VolumeDetail -- ^ 'ivVolume'
    -> ImportVolume
importVolume pAvailabilityZone_ pImage_ pVolume_ =
    ImportVolume'
    { _ivDescription = Nothing
    , _ivDryRun = Nothing
    , _ivAvailabilityZone = pAvailabilityZone_
    , _ivImage = pImage_
    , _ivVolume = pVolume_
    }

-- | A description of the volume.
ivDescription :: Lens' ImportVolume (Maybe Text)
ivDescription = lens _ivDescription (\ s a -> s{_ivDescription = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
ivDryRun :: Lens' ImportVolume (Maybe Bool)
ivDryRun = lens _ivDryRun (\ s a -> s{_ivDryRun = a});

-- | The Availability Zone for the resulting EBS volume.
ivAvailabilityZone :: Lens' ImportVolume Text
ivAvailabilityZone = lens _ivAvailabilityZone (\ s a -> s{_ivAvailabilityZone = a});

-- | The disk image.
ivImage :: Lens' ImportVolume DiskImageDetail
ivImage = lens _ivImage (\ s a -> s{_ivImage = a});

-- | The volume size.
ivVolume :: Lens' ImportVolume VolumeDetail
ivVolume = lens _ivVolume (\ s a -> s{_ivVolume = a});

instance AWSRequest ImportVolume where
        type Rs ImportVolume = ImportVolumeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ImportVolumeResponse' <$>
                   (x .@? "conversionTask") <*> (pure (fromEnum s)))

instance Hashable ImportVolume

instance NFData ImportVolume

instance ToHeaders ImportVolume where
        toHeaders = const mempty

instance ToPath ImportVolume where
        toPath = const "/"

instance ToQuery ImportVolume where
        toQuery ImportVolume'{..}
          = mconcat
              ["Action" =: ("ImportVolume" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "Description" =: _ivDescription,
               "DryRun" =: _ivDryRun,
               "AvailabilityZone" =: _ivAvailabilityZone,
               "Image" =: _ivImage, "Volume" =: _ivVolume]

-- | Contains the output for ImportVolume.
--
-- /See:/ 'importVolumeResponse' smart constructor.
data ImportVolumeResponse = ImportVolumeResponse'
    { _ivrsConversionTask :: !(Maybe ConversionTask)
    , _ivrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ImportVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ivrsConversionTask'
--
-- * 'ivrsResponseStatus'
importVolumeResponse
    :: Int -- ^ 'ivrsResponseStatus'
    -> ImportVolumeResponse
importVolumeResponse pResponseStatus_ =
    ImportVolumeResponse'
    { _ivrsConversionTask = Nothing
    , _ivrsResponseStatus = pResponseStatus_
    }

-- | Information about the conversion task.
ivrsConversionTask :: Lens' ImportVolumeResponse (Maybe ConversionTask)
ivrsConversionTask = lens _ivrsConversionTask (\ s a -> s{_ivrsConversionTask = a});

-- | The response status code.
ivrsResponseStatus :: Lens' ImportVolumeResponse Int
ivrsResponseStatus = lens _ivrsResponseStatus (\ s a -> s{_ivrsResponseStatus = a});

instance NFData ImportVolumeResponse
