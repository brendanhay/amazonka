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
-- Module      : Network.AWS.Lightsail.RegisterContainerImage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a container image to your Amazon Lightsail container service.
module Network.AWS.Lightsail.RegisterContainerImage
  ( -- * Creating a Request
    registerContainerImage,
    RegisterContainerImage,

    -- * Request Lenses
    rciServiceName,
    rciLabel,
    rciDigest,

    -- * Destructuring the Response
    registerContainerImageResponse,
    RegisterContainerImageResponse,

    -- * Response Lenses
    rcirsContainerImage,
    rcirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'registerContainerImage' smart constructor.
data RegisterContainerImage = RegisterContainerImage'
  { _rciServiceName ::
      !Text,
    _rciLabel :: !Text,
    _rciDigest :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterContainerImage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rciServiceName' - The name of the container service for which to register a container image.
--
-- * 'rciLabel' - The label for the container image when it's registered to the container service. Use a descriptive label that you can use to track the different versions of your registered container images. Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:     * @:container-service-1.<imagelabel>.1@  If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ . The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
--
-- * 'rciDigest' - The digest of the container image to be registered.
registerContainerImage ::
  -- | 'rciServiceName'
  Text ->
  -- | 'rciLabel'
  Text ->
  -- | 'rciDigest'
  Text ->
  RegisterContainerImage
registerContainerImage pServiceName_ pLabel_ pDigest_ =
  RegisterContainerImage'
    { _rciServiceName = pServiceName_,
      _rciLabel = pLabel_,
      _rciDigest = pDigest_
    }

-- | The name of the container service for which to register a container image.
rciServiceName :: Lens' RegisterContainerImage Text
rciServiceName = lens _rciServiceName (\s a -> s {_rciServiceName = a})

-- | The label for the container image when it's registered to the container service. Use a descriptive label that you can use to track the different versions of your registered container images. Use the @GetContainerImages@ action to return the container images registered to a Lightsail container service. The label is the @<imagelabel>@ portion of the following image name example:     * @:container-service-1.<imagelabel>.1@  If the name of your container service is @mycontainerservice@ , and the label that you specify is @mystaticwebsite@ , then the name of the registered container image will be @:mycontainerservice.mystaticwebsite.1@ . The number at the end of these image name examples represents the version of the registered container image. If you push and register another container image to the same Lightsail container service, with the same label, then the version number for the new registered container image will be @2@ . If you push and register another container image, the version number will be @3@ , and so on.
rciLabel :: Lens' RegisterContainerImage Text
rciLabel = lens _rciLabel (\s a -> s {_rciLabel = a})

-- | The digest of the container image to be registered.
rciDigest :: Lens' RegisterContainerImage Text
rciDigest = lens _rciDigest (\s a -> s {_rciDigest = a})

instance AWSRequest RegisterContainerImage where
  type Rs RegisterContainerImage = RegisterContainerImageResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          RegisterContainerImageResponse'
            <$> (x .?> "containerImage") <*> (pure (fromEnum s))
      )

instance Hashable RegisterContainerImage

instance NFData RegisterContainerImage

instance ToHeaders RegisterContainerImage where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.RegisterContainerImage" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON RegisterContainerImage where
  toJSON RegisterContainerImage' {..} =
    object
      ( catMaybes
          [ Just ("serviceName" .= _rciServiceName),
            Just ("label" .= _rciLabel),
            Just ("digest" .= _rciDigest)
          ]
      )

instance ToPath RegisterContainerImage where
  toPath = const "/"

instance ToQuery RegisterContainerImage where
  toQuery = const mempty

-- | /See:/ 'registerContainerImageResponse' smart constructor.
data RegisterContainerImageResponse = RegisterContainerImageResponse'
  { _rcirsContainerImage ::
      !(Maybe ContainerImage),
    _rcirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RegisterContainerImageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rcirsContainerImage' - Undocumented member.
--
-- * 'rcirsResponseStatus' - -- | The response status code.
registerContainerImageResponse ::
  -- | 'rcirsResponseStatus'
  Int ->
  RegisterContainerImageResponse
registerContainerImageResponse pResponseStatus_ =
  RegisterContainerImageResponse'
    { _rcirsContainerImage = Nothing,
      _rcirsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rcirsContainerImage :: Lens' RegisterContainerImageResponse (Maybe ContainerImage)
rcirsContainerImage = lens _rcirsContainerImage (\s a -> s {_rcirsContainerImage = a})

-- | -- | The response status code.
rcirsResponseStatus :: Lens' RegisterContainerImageResponse Int
rcirsResponseStatus = lens _rcirsResponseStatus (\s a -> s {_rcirsResponseStatus = a})

instance NFData RegisterContainerImageResponse
