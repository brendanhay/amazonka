{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Creates an import volume task using metadata from the specified disk
-- image. After importing the image, you then upload it using the
-- @ec2-import-volume@ command in the Amazon EC2 command-line interface
-- (CLI) tools. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/UploadingYourInstancesandVolumes.html Using the Command Line Tools to Import Your Virtual Machine to Amazon EC2>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ImportVolume.html>
module Network.AWS.EC2.ImportVolume
    (
    -- * Request
      ImportVolume
    -- ** Request constructor
    , importVolume
    -- ** Request lenses
    , ivDryRun
    , ivDescription
    , ivAvailabilityZone
    , ivImage
    , ivVolume

    -- * Response
    , ImportVolumeResponse
    -- ** Response constructor
    , importVolumeResponse
    -- ** Response lenses
    , ivrConversionTask
    , ivrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivDryRun'
--
-- * 'ivDescription'
--
-- * 'ivAvailabilityZone'
--
-- * 'ivImage'
--
-- * 'ivVolume'
data ImportVolume = ImportVolume'
    { _ivDryRun           :: !(Maybe Bool)
    , _ivDescription      :: !(Maybe Text)
    , _ivAvailabilityZone :: !Text
    , _ivImage            :: !DiskImageDetail
    , _ivVolume           :: !VolumeDetail
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportVolume' smart constructor.
importVolume :: Text -> DiskImageDetail -> VolumeDetail -> ImportVolume
importVolume pAvailabilityZone pImage pVolume =
    ImportVolume'
    { _ivDryRun = Nothing
    , _ivDescription = Nothing
    , _ivAvailabilityZone = pAvailabilityZone
    , _ivImage = pImage
    , _ivVolume = pVolume
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ivDryRun :: Lens' ImportVolume (Maybe Bool)
ivDryRun = lens _ivDryRun (\ s a -> s{_ivDryRun = a});

-- | A description of the volume.
ivDescription :: Lens' ImportVolume (Maybe Text)
ivDescription = lens _ivDescription (\ s a -> s{_ivDescription = a});

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
        type Sv ImportVolume = EC2
        type Rs ImportVolume = ImportVolumeResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 ImportVolumeResponse' <$>
                   (x .@? "conversionTask") <*> (pure (fromEnum s)))

instance ToHeaders ImportVolume where
        toHeaders = const mempty

instance ToPath ImportVolume where
        toPath = const "/"

instance ToQuery ImportVolume where
        toQuery ImportVolume'{..}
          = mconcat
              ["Action" =: ("ImportVolume" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _ivDryRun,
               "Description" =: _ivDescription,
               "AvailabilityZone" =: _ivAvailabilityZone,
               "Image" =: _ivImage, "Volume" =: _ivVolume]

-- | /See:/ 'importVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivrConversionTask'
--
-- * 'ivrStatus'
data ImportVolumeResponse = ImportVolumeResponse'
    { _ivrConversionTask :: !(Maybe ConversionTask)
    , _ivrStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportVolumeResponse' smart constructor.
importVolumeResponse :: Int -> ImportVolumeResponse
importVolumeResponse pStatus =
    ImportVolumeResponse'
    { _ivrConversionTask = Nothing
    , _ivrStatus = pStatus
    }

-- | Information about the conversion task.
ivrConversionTask :: Lens' ImportVolumeResponse (Maybe ConversionTask)
ivrConversionTask = lens _ivrConversionTask (\ s a -> s{_ivrConversionTask = a});

-- | FIXME: Undocumented member.
ivrStatus :: Lens' ImportVolumeResponse Int
ivrStatus = lens _ivrStatus (\ s a -> s{_ivrStatus = a});
