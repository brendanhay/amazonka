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
-- Module      : Network.AWS.EC2.ModifyVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can modify several parameters of an existing EBS volume, including volume size, volume type, and IOPS capacity. If your EBS volume is attached to a current-generation EC2 instance type, you may be able to apply these changes without stopping the instance or detaching the volume from it. For more information about modifying an EBS volume running Linux, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html Modifying the Size, IOPS, or Type of an EBS Volume on Linux> . For more information about modifying an EBS volume running Windows, see <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html Modifying the Size, IOPS, or Type of an EBS Volume on Windows> .
--
--
-- When you complete a resize operation on your volume, you need to extend the volume's file-system size to take advantage of the new storage capacity. For information about extending a Linux file system, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#recognize-expanded-volume-linux Extending a Linux File System> . For information about extending a Windows file system, see <http://docs.aws.amazon.com/AWSEC2/latest/WindowsGuide/ebs-expand-volume.html#recognize-expanded-volume-windows Extending a Windows File System> .
--
-- You can use CloudWatch Events to check the status of a modification to an EBS volume. For information about CloudWatch Events, see the <http://docs.aws.amazon.com/AmazonCloudWatch/latest/events/ Amazon CloudWatch Events User Guide> . You can also track the status of a modification using the 'DescribeVolumesModifications' API. For information about tracking status changes using either method, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ebs-expand-volume.html#monitoring_mods Monitoring Volume Modifications> .
--
module Network.AWS.EC2.ModifyVolume
    (
    -- * Creating a Request
      modifyVolume
    , ModifyVolume
    -- * Request Lenses
    , mvSize
    , mvIOPS
    , mvVolumeType
    , mvDryRun
    , mvVolumeId

    -- * Destructuring the Response
    , modifyVolumeResponse
    , ModifyVolumeResponse
    -- * Response Lenses
    , mvrsVolumeModification
    , mvrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVolume' smart constructor.
data ModifyVolume = ModifyVolume'
  { _mvSize       :: !(Maybe Int)
  , _mvIOPS       :: !(Maybe Int)
  , _mvVolumeType :: !(Maybe VolumeType)
  , _mvDryRun     :: !(Maybe Bool)
  , _mvVolumeId   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvSize' - Target size in GiB of the volume to be modified. Target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html> . Default: If no size is specified, the existing size is retained.
--
-- * 'mvIOPS' - Target IOPS rate of the volume to be modified. Only valid for Provisioned IOPS SSD (@io1@ ) volumes. For more information about @io1@ IOPS configuration, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops> . Default: If no IOPS value is specified, the existing value is retained.
--
-- * 'mvVolumeType' - Target EBS volume type of the volume to be modified The API does not support modifications for volume type @standard@ . You also cannot change the type of a volume to @standard@ .  Default: If no type is specified, the existing type is retained.
--
-- * 'mvDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvVolumeId' - The ID of the volume.
modifyVolume
    :: Text -- ^ 'mvVolumeId'
    -> ModifyVolume
modifyVolume pVolumeId_ =
  ModifyVolume'
    { _mvSize = Nothing
    , _mvIOPS = Nothing
    , _mvVolumeType = Nothing
    , _mvDryRun = Nothing
    , _mvVolumeId = pVolumeId_
    }


-- | Target size in GiB of the volume to be modified. Target volume size must be greater than or equal to than the existing size of the volume. For information about available EBS volume sizes, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html> . Default: If no size is specified, the existing size is retained.
mvSize :: Lens' ModifyVolume (Maybe Int)
mvSize = lens _mvSize (\ s a -> s{_mvSize = a})

-- | Target IOPS rate of the volume to be modified. Only valid for Provisioned IOPS SSD (@io1@ ) volumes. For more information about @io1@ IOPS configuration, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/EBSVolumeTypes.html#EBSVolumeTypes_piops> . Default: If no IOPS value is specified, the existing value is retained.
mvIOPS :: Lens' ModifyVolume (Maybe Int)
mvIOPS = lens _mvIOPS (\ s a -> s{_mvIOPS = a})

-- | Target EBS volume type of the volume to be modified The API does not support modifications for volume type @standard@ . You also cannot change the type of a volume to @standard@ .  Default: If no type is specified, the existing type is retained.
mvVolumeType :: Lens' ModifyVolume (Maybe VolumeType)
mvVolumeType = lens _mvVolumeType (\ s a -> s{_mvVolumeType = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvDryRun :: Lens' ModifyVolume (Maybe Bool)
mvDryRun = lens _mvDryRun (\ s a -> s{_mvDryRun = a})

-- | The ID of the volume.
mvVolumeId :: Lens' ModifyVolume Text
mvVolumeId = lens _mvVolumeId (\ s a -> s{_mvVolumeId = a})

instance AWSRequest ModifyVolume where
        type Rs ModifyVolume = ModifyVolumeResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVolumeResponse' <$>
                   (x .@? "volumeModification") <*> (pure (fromEnum s)))

instance Hashable ModifyVolume where

instance NFData ModifyVolume where

instance ToHeaders ModifyVolume where
        toHeaders = const mempty

instance ToPath ModifyVolume where
        toPath = const "/"

instance ToQuery ModifyVolume where
        toQuery ModifyVolume'{..}
          = mconcat
              ["Action" =: ("ModifyVolume" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Size" =: _mvSize, "Iops" =: _mvIOPS,
               "VolumeType" =: _mvVolumeType, "DryRun" =: _mvDryRun,
               "VolumeId" =: _mvVolumeId]

-- | /See:/ 'modifyVolumeResponse' smart constructor.
data ModifyVolumeResponse = ModifyVolumeResponse'
  { _mvrsVolumeModification :: !(Maybe VolumeModification)
  , _mvrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvrsVolumeModification' - A 'VolumeModification' object.
--
-- * 'mvrsResponseStatus' - -- | The response status code.
modifyVolumeResponse
    :: Int -- ^ 'mvrsResponseStatus'
    -> ModifyVolumeResponse
modifyVolumeResponse pResponseStatus_ =
  ModifyVolumeResponse'
    {_mvrsVolumeModification = Nothing, _mvrsResponseStatus = pResponseStatus_}


-- | A 'VolumeModification' object.
mvrsVolumeModification :: Lens' ModifyVolumeResponse (Maybe VolumeModification)
mvrsVolumeModification = lens _mvrsVolumeModification (\ s a -> s{_mvrsVolumeModification = a})

-- | -- | The response status code.
mvrsResponseStatus :: Lens' ModifyVolumeResponse Int
mvrsResponseStatus = lens _mvrsResponseStatus (\ s a -> s{_mvrsResponseStatus = a})

instance NFData ModifyVolumeResponse where
