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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified images or all images in the account.
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
    , disrsImages
    , disrsResponseStatus
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
-- * 'diNames' - The names of the images to describe.
describeImages
    :: DescribeImages
describeImages = DescribeImages' {_diNames = Nothing}


-- | The names of the images to describe.
diNames :: Lens' DescribeImages [Text]
diNames = lens _diNames (\ s a -> s{_diNames = a}) . _Default . _Coerce

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
  { _disrsImages         :: !(Maybe [Image])
  , _disrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsImages' - Information about the images.
--
-- * 'disrsResponseStatus' - -- | The response status code.
describeImagesResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DescribeImagesResponse
describeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
    {_disrsImages = Nothing, _disrsResponseStatus = pResponseStatus_}


-- | Information about the images.
disrsImages :: Lens' DescribeImagesResponse [Image]
disrsImages = lens _disrsImages (\ s a -> s{_disrsImages = a}) . _Default . _Coerce

-- | -- | The response status code.
disrsResponseStatus :: Lens' DescribeImagesResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DescribeImagesResponse where
