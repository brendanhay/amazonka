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
-- Module      : Network.AWS.Lightsail.GetContainerImages
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the container images that are registered to your Amazon Lightsail container service.
module Network.AWS.Lightsail.GetContainerImages
  ( -- * Creating a Request
    getContainerImages,
    GetContainerImages,

    -- * Request Lenses
    gciServiceName,

    -- * Destructuring the Response
    getContainerImagesResponse,
    GetContainerImagesResponse,

    -- * Response Lenses
    gcirsContainerImages,
    gcirsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getContainerImages' smart constructor.
newtype GetContainerImages = GetContainerImages'
  { _gciServiceName ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContainerImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gciServiceName' - The name of the container service for which to return registered container images.
getContainerImages ::
  -- | 'gciServiceName'
  Text ->
  GetContainerImages
getContainerImages pServiceName_ =
  GetContainerImages' {_gciServiceName = pServiceName_}

-- | The name of the container service for which to return registered container images.
gciServiceName :: Lens' GetContainerImages Text
gciServiceName = lens _gciServiceName (\s a -> s {_gciServiceName = a})

instance AWSRequest GetContainerImages where
  type Rs GetContainerImages = GetContainerImagesResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          GetContainerImagesResponse'
            <$> (x .?> "containerImages" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable GetContainerImages

instance NFData GetContainerImages

instance ToHeaders GetContainerImages where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.GetContainerImages" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON GetContainerImages where
  toJSON GetContainerImages' {..} =
    object (catMaybes [Just ("serviceName" .= _gciServiceName)])

instance ToPath GetContainerImages where
  toPath = const "/"

instance ToQuery GetContainerImages where
  toQuery = const mempty

-- | /See:/ 'getContainerImagesResponse' smart constructor.
data GetContainerImagesResponse = GetContainerImagesResponse'
  { _gcirsContainerImages ::
      !(Maybe [ContainerImage]),
    _gcirsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GetContainerImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gcirsContainerImages' - An array of objects that describe container images that are registered to the container service.
--
-- * 'gcirsResponseStatus' - -- | The response status code.
getContainerImagesResponse ::
  -- | 'gcirsResponseStatus'
  Int ->
  GetContainerImagesResponse
getContainerImagesResponse pResponseStatus_ =
  GetContainerImagesResponse'
    { _gcirsContainerImages = Nothing,
      _gcirsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe container images that are registered to the container service.
gcirsContainerImages :: Lens' GetContainerImagesResponse [ContainerImage]
gcirsContainerImages = lens _gcirsContainerImages (\s a -> s {_gcirsContainerImages = a}) . _Default . _Coerce

-- | -- | The response status code.
gcirsResponseStatus :: Lens' GetContainerImagesResponse Int
gcirsResponseStatus = lens _gcirsResponseStatus (\s a -> s {_gcirsResponseStatus = a})

instance NFData GetContainerImagesResponse
