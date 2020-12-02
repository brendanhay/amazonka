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
-- Module      : Network.AWS.ECR.StartImageScan
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an image vulnerability scan. An image scan can only be started once per day on an individual image. This limit includes if an image was scanned on initial push. For more information, see <https://docs.aws.amazon.com/AmazonECR/latest/userguide/image-scanning.html Image Scanning> in the /Amazon Elastic Container Registry User Guide/ .
module Network.AWS.ECR.StartImageScan
  ( -- * Creating a Request
    startImageScan,
    StartImageScan,

    -- * Request Lenses
    sisRegistryId,
    sisRepositoryName,
    sisImageId,

    -- * Destructuring the Response
    startImageScanResponse,
    StartImageScanResponse,

    -- * Response Lenses
    sisrsRegistryId,
    sisrsImageScanStatus,
    sisrsImageId,
    sisrsRepositoryName,
    sisrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startImageScan' smart constructor.
data StartImageScan = StartImageScan'
  { _sisRegistryId ::
      !(Maybe Text),
    _sisRepositoryName :: !Text,
    _sisImageId :: !ImageIdentifier
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartImageScan' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sisRegistryId' - The AWS account ID associated with the registry that contains the repository in which to start an image scan request. If you do not specify a registry, the default registry is assumed.
--
-- * 'sisRepositoryName' - The name of the repository that contains the images to scan.
--
-- * 'sisImageId' - Undocumented member.
startImageScan ::
  -- | 'sisRepositoryName'
  Text ->
  -- | 'sisImageId'
  ImageIdentifier ->
  StartImageScan
startImageScan pRepositoryName_ pImageId_ =
  StartImageScan'
    { _sisRegistryId = Nothing,
      _sisRepositoryName = pRepositoryName_,
      _sisImageId = pImageId_
    }

-- | The AWS account ID associated with the registry that contains the repository in which to start an image scan request. If you do not specify a registry, the default registry is assumed.
sisRegistryId :: Lens' StartImageScan (Maybe Text)
sisRegistryId = lens _sisRegistryId (\s a -> s {_sisRegistryId = a})

-- | The name of the repository that contains the images to scan.
sisRepositoryName :: Lens' StartImageScan Text
sisRepositoryName = lens _sisRepositoryName (\s a -> s {_sisRepositoryName = a})

-- | Undocumented member.
sisImageId :: Lens' StartImageScan ImageIdentifier
sisImageId = lens _sisImageId (\s a -> s {_sisImageId = a})

instance AWSRequest StartImageScan where
  type Rs StartImageScan = StartImageScanResponse
  request = postJSON ecr
  response =
    receiveJSON
      ( \s h x ->
          StartImageScanResponse'
            <$> (x .?> "registryId")
            <*> (x .?> "imageScanStatus")
            <*> (x .?> "imageId")
            <*> (x .?> "repositoryName")
            <*> (pure (fromEnum s))
      )

instance Hashable StartImageScan

instance NFData StartImageScan

instance ToHeaders StartImageScan where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerRegistry_V20150921.StartImageScan" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON StartImageScan where
  toJSON StartImageScan' {..} =
    object
      ( catMaybes
          [ ("registryId" .=) <$> _sisRegistryId,
            Just ("repositoryName" .= _sisRepositoryName),
            Just ("imageId" .= _sisImageId)
          ]
      )

instance ToPath StartImageScan where
  toPath = const "/"

instance ToQuery StartImageScan where
  toQuery = const mempty

-- | /See:/ 'startImageScanResponse' smart constructor.
data StartImageScanResponse = StartImageScanResponse'
  { _sisrsRegistryId ::
      !(Maybe Text),
    _sisrsImageScanStatus ::
      !(Maybe ImageScanStatus),
    _sisrsImageId :: !(Maybe ImageIdentifier),
    _sisrsRepositoryName :: !(Maybe Text),
    _sisrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StartImageScanResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sisrsRegistryId' - The registry ID associated with the request.
--
-- * 'sisrsImageScanStatus' - The current state of the scan.
--
-- * 'sisrsImageId' - Undocumented member.
--
-- * 'sisrsRepositoryName' - The repository name associated with the request.
--
-- * 'sisrsResponseStatus' - -- | The response status code.
startImageScanResponse ::
  -- | 'sisrsResponseStatus'
  Int ->
  StartImageScanResponse
startImageScanResponse pResponseStatus_ =
  StartImageScanResponse'
    { _sisrsRegistryId = Nothing,
      _sisrsImageScanStatus = Nothing,
      _sisrsImageId = Nothing,
      _sisrsRepositoryName = Nothing,
      _sisrsResponseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
sisrsRegistryId :: Lens' StartImageScanResponse (Maybe Text)
sisrsRegistryId = lens _sisrsRegistryId (\s a -> s {_sisrsRegistryId = a})

-- | The current state of the scan.
sisrsImageScanStatus :: Lens' StartImageScanResponse (Maybe ImageScanStatus)
sisrsImageScanStatus = lens _sisrsImageScanStatus (\s a -> s {_sisrsImageScanStatus = a})

-- | Undocumented member.
sisrsImageId :: Lens' StartImageScanResponse (Maybe ImageIdentifier)
sisrsImageId = lens _sisrsImageId (\s a -> s {_sisrsImageId = a})

-- | The repository name associated with the request.
sisrsRepositoryName :: Lens' StartImageScanResponse (Maybe Text)
sisrsRepositoryName = lens _sisrsRepositoryName (\s a -> s {_sisrsRepositoryName = a})

-- | -- | The response status code.
sisrsResponseStatus :: Lens' StartImageScanResponse Int
sisrsResponseStatus = lens _sisrsResponseStatus (\s a -> s {_sisrsResponseStatus = a})

instance NFData StartImageScanResponse
