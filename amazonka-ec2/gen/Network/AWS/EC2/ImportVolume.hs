{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ImportVolume
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates an import volume task using metadata from the specified disk
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
    , ivrqDryRun
    , ivrqDescription
    , ivrqAvailabilityZone
    , ivrqImage
    , ivrqVolume

    -- * Response
    , ImportVolumeResponse
    -- ** Response constructor
    , importVolumeResponse
    -- ** Response lenses
    , ivrsConversionTask
    , ivrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'importVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivrqDryRun'
--
-- * 'ivrqDescription'
--
-- * 'ivrqAvailabilityZone'
--
-- * 'ivrqImage'
--
-- * 'ivrqVolume'
data ImportVolume = ImportVolume'
    { _ivrqDryRun           :: !(Maybe Bool)
    , _ivrqDescription      :: !(Maybe Text)
    , _ivrqAvailabilityZone :: !Text
    , _ivrqImage            :: !DiskImageDetail
    , _ivrqVolume           :: !VolumeDetail
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportVolume' smart constructor.
importVolume :: Text -> DiskImageDetail -> VolumeDetail -> ImportVolume
importVolume pAvailabilityZone pImage pVolume =
    ImportVolume'
    { _ivrqDryRun = Nothing
    , _ivrqDescription = Nothing
    , _ivrqAvailabilityZone = pAvailabilityZone
    , _ivrqImage = pImage
    , _ivrqVolume = pVolume
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
ivrqDryRun :: Lens' ImportVolume (Maybe Bool)
ivrqDryRun = lens _ivrqDryRun (\ s a -> s{_ivrqDryRun = a});

-- | A description of the volume.
ivrqDescription :: Lens' ImportVolume (Maybe Text)
ivrqDescription = lens _ivrqDescription (\ s a -> s{_ivrqDescription = a});

-- | The Availability Zone for the resulting EBS volume.
ivrqAvailabilityZone :: Lens' ImportVolume Text
ivrqAvailabilityZone = lens _ivrqAvailabilityZone (\ s a -> s{_ivrqAvailabilityZone = a});

-- | The disk image.
ivrqImage :: Lens' ImportVolume DiskImageDetail
ivrqImage = lens _ivrqImage (\ s a -> s{_ivrqImage = a});

-- | The volume size.
ivrqVolume :: Lens' ImportVolume VolumeDetail
ivrqVolume = lens _ivrqVolume (\ s a -> s{_ivrqVolume = a});

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
               "DryRun" =: _ivrqDryRun,
               "Description" =: _ivrqDescription,
               "AvailabilityZone" =: _ivrqAvailabilityZone,
               "Image" =: _ivrqImage, "Volume" =: _ivrqVolume]

-- | /See:/ 'importVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ivrsConversionTask'
--
-- * 'ivrsStatus'
data ImportVolumeResponse = ImportVolumeResponse'
    { _ivrsConversionTask :: !(Maybe ConversionTask)
    , _ivrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ImportVolumeResponse' smart constructor.
importVolumeResponse :: Int -> ImportVolumeResponse
importVolumeResponse pStatus =
    ImportVolumeResponse'
    { _ivrsConversionTask = Nothing
    , _ivrsStatus = pStatus
    }

-- | Information about the conversion task.
ivrsConversionTask :: Lens' ImportVolumeResponse (Maybe ConversionTask)
ivrsConversionTask = lens _ivrsConversionTask (\ s a -> s{_ivrsConversionTask = a});

-- | FIXME: Undocumented member.
ivrsStatus :: Lens' ImportVolumeResponse Int
ivrsStatus = lens _ivrsStatus (\ s a -> s{_ivrsStatus = a});
