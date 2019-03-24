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
-- Retrieves a list that describes one or more specified images, if the image names or image ARNs are provided. Otherwise, all images in the account are described.
--
--
--
-- This operation returns paginated results.
module Network.AWS.AppStream.DescribeImages
    (
    -- * Creating a Request
      describeImages
    , DescribeImages
    -- * Request Lenses
    , diNextToken
    , diNames
    , diType
    , diARNs
    , diMaxResults

    -- * Destructuring the Response
    , describeImagesResponse
    , DescribeImagesResponse
    -- * Response Lenses
    , diirsImages
    , diirsNextToken
    , diirsResponseStatus
    ) where

import Network.AWS.AppStream.Types
import Network.AWS.AppStream.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeImages' smart constructor.
data DescribeImages = DescribeImages'
  { _diNextToken  :: !(Maybe Text)
  , _diNames      :: !(Maybe [Text])
  , _diType       :: !(Maybe VisibilityType)
  , _diARNs       :: !(Maybe [Text])
  , _diMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diNextToken' - The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
--
-- * 'diNames' - The names of the public or private images to describe.
--
-- * 'diType' - The type of image (public, private, or shared) to describe.
--
-- * 'diARNs' - The ARNs of the public, private, and shared images to describe.
--
-- * 'diMaxResults' - The maximum size of each page of results.
describeImages
    :: DescribeImages
describeImages =
  DescribeImages'
    { _diNextToken = Nothing
    , _diNames = Nothing
    , _diType = Nothing
    , _diARNs = Nothing
    , _diMaxResults = Nothing
    }


-- | The pagination token to use to retrieve the next page of results for this operation. If this value is null, it retrieves the first page.
diNextToken :: Lens' DescribeImages (Maybe Text)
diNextToken = lens _diNextToken (\ s a -> s{_diNextToken = a})

-- | The names of the public or private images to describe.
diNames :: Lens' DescribeImages [Text]
diNames = lens _diNames (\ s a -> s{_diNames = a}) . _Default . _Coerce

-- | The type of image (public, private, or shared) to describe.
diType :: Lens' DescribeImages (Maybe VisibilityType)
diType = lens _diType (\ s a -> s{_diType = a})

-- | The ARNs of the public, private, and shared images to describe.
diARNs :: Lens' DescribeImages [Text]
diARNs = lens _diARNs (\ s a -> s{_diARNs = a}) . _Default . _Coerce

-- | The maximum size of each page of results.
diMaxResults :: Lens' DescribeImages (Maybe Natural)
diMaxResults = lens _diMaxResults (\ s a -> s{_diMaxResults = a}) . mapping _Nat

instance AWSPager DescribeImages where
        page rq rs
          | stop (rs ^. diirsNextToken) = Nothing
          | stop (rs ^. diirsImages) = Nothing
          | otherwise =
            Just $ rq & diNextToken .~ rs ^. diirsNextToken

instance AWSRequest DescribeImages where
        type Rs DescribeImages = DescribeImagesResponse
        request = postJSON appStream
        response
          = receiveJSON
              (\ s h x ->
                 DescribeImagesResponse' <$>
                   (x .?> "Images" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

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
          = object
              (catMaybes
                 [("NextToken" .=) <$> _diNextToken,
                  ("Names" .=) <$> _diNames, ("Type" .=) <$> _diType,
                  ("Arns" .=) <$> _diARNs,
                  ("MaxResults" .=) <$> _diMaxResults])

instance ToPath DescribeImages where
        toPath = const "/"

instance ToQuery DescribeImages where
        toQuery = const mempty

-- | /See:/ 'describeImagesResponse' smart constructor.
data DescribeImagesResponse = DescribeImagesResponse'
  { _diirsImages         :: !(Maybe [Image])
  , _diirsNextToken      :: !(Maybe Text)
  , _diirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diirsImages' - Information about the images.
--
-- * 'diirsNextToken' - The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
--
-- * 'diirsResponseStatus' - -- | The response status code.
describeImagesResponse
    :: Int -- ^ 'diirsResponseStatus'
    -> DescribeImagesResponse
describeImagesResponse pResponseStatus_ =
  DescribeImagesResponse'
    { _diirsImages = Nothing
    , _diirsNextToken = Nothing
    , _diirsResponseStatus = pResponseStatus_
    }


-- | Information about the images.
diirsImages :: Lens' DescribeImagesResponse [Image]
diirsImages = lens _diirsImages (\ s a -> s{_diirsImages = a}) . _Default . _Coerce

-- | The pagination token to use to retrieve the next page of results for this operation. If there are no more pages, this value is null.
diirsNextToken :: Lens' DescribeImagesResponse (Maybe Text)
diirsNextToken = lens _diirsNextToken (\ s a -> s{_diirsNextToken = a})

-- | -- | The response status code.
diirsResponseStatus :: Lens' DescribeImagesResponse Int
diirsResponseStatus = lens _diirsResponseStatus (\ s a -> s{_diirsResponseStatus = a})

instance NFData DescribeImagesResponse where
