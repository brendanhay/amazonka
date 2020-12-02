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
-- Module      : Network.AWS.CodeBuild.ListCuratedEnvironmentImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about Docker images that are managed by AWS CodeBuild.
--
--
module Network.AWS.CodeBuild.ListCuratedEnvironmentImages
    (
    -- * Creating a Request
      listCuratedEnvironmentImages
    , ListCuratedEnvironmentImages

    -- * Destructuring the Response
    , listCuratedEnvironmentImagesResponse
    , ListCuratedEnvironmentImagesResponse
    -- * Response Lenses
    , lceirsPlatforms
    , lceirsResponseStatus
    ) where

import Network.AWS.CodeBuild.Types
import Network.AWS.CodeBuild.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listCuratedEnvironmentImages' smart constructor.
data ListCuratedEnvironmentImages =
  ListCuratedEnvironmentImages'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCuratedEnvironmentImages' with the minimum fields required to make a request.
--
listCuratedEnvironmentImages
    :: ListCuratedEnvironmentImages
listCuratedEnvironmentImages = ListCuratedEnvironmentImages'


instance AWSRequest ListCuratedEnvironmentImages
         where
        type Rs ListCuratedEnvironmentImages =
             ListCuratedEnvironmentImagesResponse
        request = postJSON codeBuild
        response
          = receiveJSON
              (\ s h x ->
                 ListCuratedEnvironmentImagesResponse' <$>
                   (x .?> "platforms" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable ListCuratedEnvironmentImages where

instance NFData ListCuratedEnvironmentImages where

instance ToHeaders ListCuratedEnvironmentImages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("CodeBuild_20161006.ListCuratedEnvironmentImages" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCuratedEnvironmentImages where
        toJSON = const (Object mempty)

instance ToPath ListCuratedEnvironmentImages where
        toPath = const "/"

instance ToQuery ListCuratedEnvironmentImages where
        toQuery = const mempty

-- | /See:/ 'listCuratedEnvironmentImagesResponse' smart constructor.
data ListCuratedEnvironmentImagesResponse = ListCuratedEnvironmentImagesResponse'
  { _lceirsPlatforms      :: !(Maybe [EnvironmentPlatform])
  , _lceirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCuratedEnvironmentImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lceirsPlatforms' - Information about supported platforms for Docker images that are managed by AWS CodeBuild.
--
-- * 'lceirsResponseStatus' - -- | The response status code.
listCuratedEnvironmentImagesResponse
    :: Int -- ^ 'lceirsResponseStatus'
    -> ListCuratedEnvironmentImagesResponse
listCuratedEnvironmentImagesResponse pResponseStatus_ =
  ListCuratedEnvironmentImagesResponse'
    {_lceirsPlatforms = Nothing, _lceirsResponseStatus = pResponseStatus_}


-- | Information about supported platforms for Docker images that are managed by AWS CodeBuild.
lceirsPlatforms :: Lens' ListCuratedEnvironmentImagesResponse [EnvironmentPlatform]
lceirsPlatforms = lens _lceirsPlatforms (\ s a -> s{_lceirsPlatforms = a}) . _Default . _Coerce

-- | -- | The response status code.
lceirsResponseStatus :: Lens' ListCuratedEnvironmentImagesResponse Int
lceirsResponseStatus = lens _lceirsResponseStatus (\ s a -> s{_lceirsResponseStatus = a})

instance NFData ListCuratedEnvironmentImagesResponse
         where
