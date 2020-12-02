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
-- Module      : Network.AWS.ECR.PutImageScanningConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the image scanning configuration for the specified repository.
module Network.AWS.ECR.PutImageScanningConfiguration
  ( -- * Creating a Request
    putImageScanningConfiguration,
    PutImageScanningConfiguration,

    -- * Request Lenses
    piscRegistryId,
    piscRepositoryName,
    piscImageScanningConfiguration,

    -- * Destructuring the Response
    putImageScanningConfigurationResponse,
    PutImageScanningConfigurationResponse,

    -- * Response Lenses
    piscrsRegistryId,
    piscrsImageScanningConfiguration,
    piscrsRepositoryName,
    piscrsResponseStatus,
  )
where

import Network.AWS.ECR.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putImageScanningConfiguration' smart constructor.
data PutImageScanningConfiguration = PutImageScanningConfiguration'
  { _piscRegistryId ::
      !(Maybe Text),
    _piscRepositoryName :: !Text,
    _piscImageScanningConfiguration ::
      !ImageScanningConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutImageScanningConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piscRegistryId' - The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
--
-- * 'piscRepositoryName' - The name of the repository in which to update the image scanning configuration setting.
--
-- * 'piscImageScanningConfiguration' - The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
putImageScanningConfiguration ::
  -- | 'piscRepositoryName'
  Text ->
  -- | 'piscImageScanningConfiguration'
  ImageScanningConfiguration ->
  PutImageScanningConfiguration
putImageScanningConfiguration
  pRepositoryName_
  pImageScanningConfiguration_ =
    PutImageScanningConfiguration'
      { _piscRegistryId = Nothing,
        _piscRepositoryName = pRepositoryName_,
        _piscImageScanningConfiguration = pImageScanningConfiguration_
      }

-- | The AWS account ID associated with the registry that contains the repository in which to update the image scanning configuration setting. If you do not specify a registry, the default registry is assumed.
piscRegistryId :: Lens' PutImageScanningConfiguration (Maybe Text)
piscRegistryId = lens _piscRegistryId (\s a -> s {_piscRegistryId = a})

-- | The name of the repository in which to update the image scanning configuration setting.
piscRepositoryName :: Lens' PutImageScanningConfiguration Text
piscRepositoryName = lens _piscRepositoryName (\s a -> s {_piscRepositoryName = a})

-- | The image scanning configuration for the repository. This setting determines whether images are scanned for known vulnerabilities after being pushed to the repository.
piscImageScanningConfiguration :: Lens' PutImageScanningConfiguration ImageScanningConfiguration
piscImageScanningConfiguration = lens _piscImageScanningConfiguration (\s a -> s {_piscImageScanningConfiguration = a})

instance AWSRequest PutImageScanningConfiguration where
  type
    Rs PutImageScanningConfiguration =
      PutImageScanningConfigurationResponse
  request = postJSON ecr
  response =
    receiveJSON
      ( \s h x ->
          PutImageScanningConfigurationResponse'
            <$> (x .?> "registryId")
            <*> (x .?> "imageScanningConfiguration")
            <*> (x .?> "repositoryName")
            <*> (pure (fromEnum s))
      )

instance Hashable PutImageScanningConfiguration

instance NFData PutImageScanningConfiguration

instance ToHeaders PutImageScanningConfiguration where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ( "AmazonEC2ContainerRegistry_V20150921.PutImageScanningConfiguration" ::
                     ByteString
                 ),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON PutImageScanningConfiguration where
  toJSON PutImageScanningConfiguration' {..} =
    object
      ( catMaybes
          [ ("registryId" .=) <$> _piscRegistryId,
            Just ("repositoryName" .= _piscRepositoryName),
            Just
              ("imageScanningConfiguration" .= _piscImageScanningConfiguration)
          ]
      )

instance ToPath PutImageScanningConfiguration where
  toPath = const "/"

instance ToQuery PutImageScanningConfiguration where
  toQuery = const mempty

-- | /See:/ 'putImageScanningConfigurationResponse' smart constructor.
data PutImageScanningConfigurationResponse = PutImageScanningConfigurationResponse'
  { _piscrsRegistryId ::
      !(Maybe Text),
    _piscrsImageScanningConfiguration ::
      !( Maybe
           ImageScanningConfiguration
       ),
    _piscrsRepositoryName ::
      !(Maybe Text),
    _piscrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PutImageScanningConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'piscrsRegistryId' - The registry ID associated with the request.
--
-- * 'piscrsImageScanningConfiguration' - The image scanning configuration setting for the repository.
--
-- * 'piscrsRepositoryName' - The repository name associated with the request.
--
-- * 'piscrsResponseStatus' - -- | The response status code.
putImageScanningConfigurationResponse ::
  -- | 'piscrsResponseStatus'
  Int ->
  PutImageScanningConfigurationResponse
putImageScanningConfigurationResponse pResponseStatus_ =
  PutImageScanningConfigurationResponse'
    { _piscrsRegistryId =
        Nothing,
      _piscrsImageScanningConfiguration = Nothing,
      _piscrsRepositoryName = Nothing,
      _piscrsResponseStatus = pResponseStatus_
    }

-- | The registry ID associated with the request.
piscrsRegistryId :: Lens' PutImageScanningConfigurationResponse (Maybe Text)
piscrsRegistryId = lens _piscrsRegistryId (\s a -> s {_piscrsRegistryId = a})

-- | The image scanning configuration setting for the repository.
piscrsImageScanningConfiguration :: Lens' PutImageScanningConfigurationResponse (Maybe ImageScanningConfiguration)
piscrsImageScanningConfiguration = lens _piscrsImageScanningConfiguration (\s a -> s {_piscrsImageScanningConfiguration = a})

-- | The repository name associated with the request.
piscrsRepositoryName :: Lens' PutImageScanningConfigurationResponse (Maybe Text)
piscrsRepositoryName = lens _piscrsRepositoryName (\s a -> s {_piscrsRepositoryName = a})

-- | -- | The response status code.
piscrsResponseStatus :: Lens' PutImageScanningConfigurationResponse Int
piscrsResponseStatus = lens _piscrsResponseStatus (\s a -> s {_piscrsResponseStatus = a})

instance NFData PutImageScanningConfigurationResponse
