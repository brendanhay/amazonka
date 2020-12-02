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
-- Module      : Network.AWS.EC2.ExportImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Exports an Amazon Machine Image (AMI) to a VM file. For more information, see <https://docs.aws.amazon.com/vm-import/latest/userguide/vmexport_image.html Exporting a VM Directory from an Amazon Machine Image (AMI)> in the /VM Import\/Export User Guide/ .
module Network.AWS.EC2.ExportImage
  ( -- * Creating a Request
    exportImage,
    ExportImage,

    -- * Request Lenses
    eiClientToken,
    eiRoleName,
    eiTagSpecifications,
    eiDescription,
    eiDryRun,
    eiDiskImageFormat,
    eiImageId,
    eiS3ExportLocation,

    -- * Destructuring the Response
    exportImageResponse,
    ExportImageResponse,

    -- * Response Lenses
    eirsStatus,
    eirsProgress,
    eirsExportImageTaskId,
    eirsRoleName,
    eirsStatusMessage,
    eirsImageId,
    eirsDescription,
    eirsTags,
    eirsS3ExportLocation,
    eirsDiskImageFormat,
    eirsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'exportImage' smart constructor.
data ExportImage = ExportImage'
  { _eiClientToken :: !(Maybe Text),
    _eiRoleName :: !(Maybe Text),
    _eiTagSpecifications :: !(Maybe [TagSpecification]),
    _eiDescription :: !(Maybe Text),
    _eiDryRun :: !(Maybe Bool),
    _eiDiskImageFormat :: !DiskImageFormat,
    _eiImageId :: !Text,
    _eiS3ExportLocation :: !ExportTaskS3LocationRequest
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eiClientToken' - Token to enable idempotency for export image requests.
--
-- * 'eiRoleName' - The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
--
-- * 'eiTagSpecifications' - The tags to apply to the image being exported.
--
-- * 'eiDescription' - A description of the image being exported. The maximum length is 255 characters.
--
-- * 'eiDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'eiDiskImageFormat' - The disk image format.
--
-- * 'eiImageId' - The ID of the image.
--
-- * 'eiS3ExportLocation' - Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
exportImage ::
  -- | 'eiDiskImageFormat'
  DiskImageFormat ->
  -- | 'eiImageId'
  Text ->
  -- | 'eiS3ExportLocation'
  ExportTaskS3LocationRequest ->
  ExportImage
exportImage pDiskImageFormat_ pImageId_ pS3ExportLocation_ =
  ExportImage'
    { _eiClientToken = Nothing,
      _eiRoleName = Nothing,
      _eiTagSpecifications = Nothing,
      _eiDescription = Nothing,
      _eiDryRun = Nothing,
      _eiDiskImageFormat = pDiskImageFormat_,
      _eiImageId = pImageId_,
      _eiS3ExportLocation = pS3ExportLocation_
    }

-- | Token to enable idempotency for export image requests.
eiClientToken :: Lens' ExportImage (Maybe Text)
eiClientToken = lens _eiClientToken (\s a -> s {_eiClientToken = a})

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket. If this parameter is not specified, the default role is named 'vmimport'.
eiRoleName :: Lens' ExportImage (Maybe Text)
eiRoleName = lens _eiRoleName (\s a -> s {_eiRoleName = a})

-- | The tags to apply to the image being exported.
eiTagSpecifications :: Lens' ExportImage [TagSpecification]
eiTagSpecifications = lens _eiTagSpecifications (\s a -> s {_eiTagSpecifications = a}) . _Default . _Coerce

-- | A description of the image being exported. The maximum length is 255 characters.
eiDescription :: Lens' ExportImage (Maybe Text)
eiDescription = lens _eiDescription (\s a -> s {_eiDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
eiDryRun :: Lens' ExportImage (Maybe Bool)
eiDryRun = lens _eiDryRun (\s a -> s {_eiDryRun = a})

-- | The disk image format.
eiDiskImageFormat :: Lens' ExportImage DiskImageFormat
eiDiskImageFormat = lens _eiDiskImageFormat (\s a -> s {_eiDiskImageFormat = a})

-- | The ID of the image.
eiImageId :: Lens' ExportImage Text
eiImageId = lens _eiImageId (\s a -> s {_eiImageId = a})

-- | Information about the destination Amazon S3 bucket. The bucket must exist and grant WRITE and READ_ACP permissions to the AWS account vm-import-export@amazon.com.
eiS3ExportLocation :: Lens' ExportImage ExportTaskS3LocationRequest
eiS3ExportLocation = lens _eiS3ExportLocation (\s a -> s {_eiS3ExportLocation = a})

instance AWSRequest ExportImage where
  type Rs ExportImage = ExportImageResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          ExportImageResponse'
            <$> (x .@? "status")
            <*> (x .@? "progress")
            <*> (x .@? "exportImageTaskId")
            <*> (x .@? "roleName")
            <*> (x .@? "statusMessage")
            <*> (x .@? "imageId")
            <*> (x .@? "description")
            <*> (x .@? "tagSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "s3ExportLocation")
            <*> (x .@? "diskImageFormat")
            <*> (pure (fromEnum s))
      )

instance Hashable ExportImage

instance NFData ExportImage

instance ToHeaders ExportImage where
  toHeaders = const mempty

instance ToPath ExportImage where
  toPath = const "/"

instance ToQuery ExportImage where
  toQuery ExportImage' {..} =
    mconcat
      [ "Action" =: ("ExportImage" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "ClientToken" =: _eiClientToken,
        "RoleName" =: _eiRoleName,
        toQuery (toQueryList "TagSpecification" <$> _eiTagSpecifications),
        "Description" =: _eiDescription,
        "DryRun" =: _eiDryRun,
        "DiskImageFormat" =: _eiDiskImageFormat,
        "ImageId" =: _eiImageId,
        "S3ExportLocation" =: _eiS3ExportLocation
      ]

-- | /See:/ 'exportImageResponse' smart constructor.
data ExportImageResponse = ExportImageResponse'
  { _eirsStatus ::
      !(Maybe Text),
    _eirsProgress :: !(Maybe Text),
    _eirsExportImageTaskId :: !(Maybe Text),
    _eirsRoleName :: !(Maybe Text),
    _eirsStatusMessage :: !(Maybe Text),
    _eirsImageId :: !(Maybe Text),
    _eirsDescription :: !(Maybe Text),
    _eirsTags :: !(Maybe [Tag]),
    _eirsS3ExportLocation ::
      !(Maybe ExportTaskS3Location),
    _eirsDiskImageFormat :: !(Maybe DiskImageFormat),
    _eirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExportImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eirsStatus' - The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
--
-- * 'eirsProgress' - The percent complete of the export image task.
--
-- * 'eirsExportImageTaskId' - The ID of the export image task.
--
-- * 'eirsRoleName' - The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
--
-- * 'eirsStatusMessage' - The status message for the export image task.
--
-- * 'eirsImageId' - The ID of the image.
--
-- * 'eirsDescription' - A description of the image being exported.
--
-- * 'eirsTags' - Any tags assigned to the image being exported.
--
-- * 'eirsS3ExportLocation' - Information about the destination Amazon S3 bucket.
--
-- * 'eirsDiskImageFormat' - The disk image format for the exported image.
--
-- * 'eirsResponseStatus' - -- | The response status code.
exportImageResponse ::
  -- | 'eirsResponseStatus'
  Int ->
  ExportImageResponse
exportImageResponse pResponseStatus_ =
  ExportImageResponse'
    { _eirsStatus = Nothing,
      _eirsProgress = Nothing,
      _eirsExportImageTaskId = Nothing,
      _eirsRoleName = Nothing,
      _eirsStatusMessage = Nothing,
      _eirsImageId = Nothing,
      _eirsDescription = Nothing,
      _eirsTags = Nothing,
      _eirsS3ExportLocation = Nothing,
      _eirsDiskImageFormat = Nothing,
      _eirsResponseStatus = pResponseStatus_
    }

-- | The status of the export image task. The possible values are @active@ , @completed@ , @deleting@ , and @deleted@ .
eirsStatus :: Lens' ExportImageResponse (Maybe Text)
eirsStatus = lens _eirsStatus (\s a -> s {_eirsStatus = a})

-- | The percent complete of the export image task.
eirsProgress :: Lens' ExportImageResponse (Maybe Text)
eirsProgress = lens _eirsProgress (\s a -> s {_eirsProgress = a})

-- | The ID of the export image task.
eirsExportImageTaskId :: Lens' ExportImageResponse (Maybe Text)
eirsExportImageTaskId = lens _eirsExportImageTaskId (\s a -> s {_eirsExportImageTaskId = a})

-- | The name of the role that grants VM Import/Export permission to export images to your Amazon S3 bucket.
eirsRoleName :: Lens' ExportImageResponse (Maybe Text)
eirsRoleName = lens _eirsRoleName (\s a -> s {_eirsRoleName = a})

-- | The status message for the export image task.
eirsStatusMessage :: Lens' ExportImageResponse (Maybe Text)
eirsStatusMessage = lens _eirsStatusMessage (\s a -> s {_eirsStatusMessage = a})

-- | The ID of the image.
eirsImageId :: Lens' ExportImageResponse (Maybe Text)
eirsImageId = lens _eirsImageId (\s a -> s {_eirsImageId = a})

-- | A description of the image being exported.
eirsDescription :: Lens' ExportImageResponse (Maybe Text)
eirsDescription = lens _eirsDescription (\s a -> s {_eirsDescription = a})

-- | Any tags assigned to the image being exported.
eirsTags :: Lens' ExportImageResponse [Tag]
eirsTags = lens _eirsTags (\s a -> s {_eirsTags = a}) . _Default . _Coerce

-- | Information about the destination Amazon S3 bucket.
eirsS3ExportLocation :: Lens' ExportImageResponse (Maybe ExportTaskS3Location)
eirsS3ExportLocation = lens _eirsS3ExportLocation (\s a -> s {_eirsS3ExportLocation = a})

-- | The disk image format for the exported image.
eirsDiskImageFormat :: Lens' ExportImageResponse (Maybe DiskImageFormat)
eirsDiskImageFormat = lens _eirsDiskImageFormat (\s a -> s {_eirsDiskImageFormat = a})

-- | -- | The response status code.
eirsResponseStatus :: Lens' ExportImageResponse Int
eirsResponseStatus = lens _eirsResponseStatus (\s a -> s {_eirsResponseStatus = a})

instance NFData ExportImageResponse
