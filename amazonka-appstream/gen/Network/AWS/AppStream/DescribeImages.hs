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
-- Module      : Network.AWS.AppStream.DescribeImages
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the images. If a list of names is not provided, all images in your account are returned. This operation does not return a paginated result.
--
--
module Network.AWS.AppStream.DescribeImages
    (
    -- * Creating a Request
      describeImages
    , DescribeImages
    -- * Request Lenses
    , diNames

    -- * Destructuring the Response
    , describeImagesResponse
    , DescribeImagesResponse
    -- * Response Lenses
    , dirsImages
    , dirsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImages' smart constructor.
newtype DescribeImages = DescribeImages'
  { _diNames :: Maybe [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diNames' - A specific list of images to describe.
describeImages
    :: DescribeImages
describeImages = DescribeImages' {_diNames = Nothing}


-- | A specific list of images to describe.
diNames :: Lens' DescribeImages [Text]
diNames = lens _diNames (\ s a -> s{_diNames = a}) . _Default . _Coerce;

instance AWSRequest DescribeImages where
        type Rs DescribeImages = DescribeImagesResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeImagesResponse' <$>
                   (x .?> "Images" .!@ mempty) <*> (pure (fromEnum s)))

instance Hashable DescribeImages where

instance NFData DescribeImages where

instance ToHeaders DescribeImages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("PhotonAdminProxyService.DescribeImages" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeImages where
        toJSON DescribeImages'{..}
          = object (catMaybes [("Names" .=) <$> _diNames])

instance ToPath DescribeImages where
        toPath = const "/"

instance ToQuery DescribeImages where
        toQuery = const mempty

-- | /See:/ 'describeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { _dirsImages         :: {-# NOUNPACK #-}!(Maybe [Image])
  , _dirsResponseStatus :: {-# NOUNPACK #-}!Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dirsImages' - The list of images.
--
-- * 'dirsResponseStatus' - -- | The response status code.
describeImagesResponse
    :: Int -- ^ 'dirsResponseStatus'
    -> DescribeImagesResponse
describeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
  {_dirsImages = Nothing, _dirsResponseStatus = pResponseStatus_}


-- | The list of images.
dirsImages :: Lens' DescribeImagesResponse [Image]
dirsImages = lens _dirsImages (\ s a -> s{_dirsImages = a}) . _Default . _Coerce;

-- | -- | The response status code.
dirsResponseStatus :: Lens' DescribeImagesResponse Int
dirsResponseStatus = lens _dirsResponseStatus (\ s a -> s{_dirsResponseStatus = a});

instance NFData DescribeImagesResponse where
