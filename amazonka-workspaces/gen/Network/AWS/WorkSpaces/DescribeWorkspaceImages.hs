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
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes one or more specified images, if the image identifiers are provided. Otherwise, all images in the account are described.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceImages
    (
    -- * Creating a Request
      describeWorkspaceImages
    , DescribeWorkspaceImages
    -- * Request Lenses
    , dwiImageIds
    , dwiNextToken
    , dwiMaxResults

    -- * Destructuring the Response
    , describeWorkspaceImagesResponse
    , DescribeWorkspaceImagesResponse
    -- * Response Lenses
    , dwirsImages
    , dwirsNextToken
    , dwirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WorkSpaces.Types
import Network.AWS.WorkSpaces.Types.Product

-- | /See:/ 'describeWorkspaceImages' smart constructor.
data DescribeWorkspaceImages = DescribeWorkspaceImages'
  { _dwiImageIds   :: !(Maybe (List1 Text))
  , _dwiNextToken  :: !(Maybe Text)
  , _dwiMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwiImageIds' - The identifier of the image.
--
-- * 'dwiNextToken' - If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
--
-- * 'dwiMaxResults' - The maximum number of items to return.
describeWorkspaceImages
    :: DescribeWorkspaceImages
describeWorkspaceImages =
  DescribeWorkspaceImages'
    {_dwiImageIds = Nothing, _dwiNextToken = Nothing, _dwiMaxResults = Nothing}


-- | The identifier of the image.
dwiImageIds :: Lens' DescribeWorkspaceImages (Maybe (NonEmpty Text))
dwiImageIds = lens _dwiImageIds (\ s a -> s{_dwiImageIds = a}) . mapping _List1

-- | If you received a @NextToken@ from a previous call that was paginated, provide this token to receive the next set of results.
dwiNextToken :: Lens' DescribeWorkspaceImages (Maybe Text)
dwiNextToken = lens _dwiNextToken (\ s a -> s{_dwiNextToken = a})

-- | The maximum number of items to return.
dwiMaxResults :: Lens' DescribeWorkspaceImages (Maybe Natural)
dwiMaxResults = lens _dwiMaxResults (\ s a -> s{_dwiMaxResults = a}) . mapping _Nat

instance AWSPager DescribeWorkspaceImages where
        page rq rs
          | stop (rs ^. dwirsNextToken) = Nothing
          | stop (rs ^. dwirsImages) = Nothing
          | otherwise =
            Just $ rq & dwiNextToken .~ rs ^. dwirsNextToken

instance AWSRequest DescribeWorkspaceImages where
        type Rs DescribeWorkspaceImages =
             DescribeWorkspaceImagesResponse
        request = postJSON workSpaces
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkspaceImagesResponse' <$>
                   (x .?> "Images" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeWorkspaceImages where

instance NFData DescribeWorkspaceImages where

instance ToHeaders DescribeWorkspaceImages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("WorkspacesService.DescribeWorkspaceImages" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkspaceImages where
        toJSON DescribeWorkspaceImages'{..}
          = object
              (catMaybes
                 [("ImageIds" .=) <$> _dwiImageIds,
                  ("NextToken" .=) <$> _dwiNextToken,
                  ("MaxResults" .=) <$> _dwiMaxResults])

instance ToPath DescribeWorkspaceImages where
        toPath = const "/"

instance ToQuery DescribeWorkspaceImages where
        toQuery = const mempty

-- | /See:/ 'describeWorkspaceImagesResponse' smart constructor.
data DescribeWorkspaceImagesResponse = DescribeWorkspaceImagesResponse'
  { _dwirsImages         :: !(Maybe [WorkspaceImage])
  , _dwirsNextToken      :: !(Maybe Text)
  , _dwirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkspaceImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwirsImages' - Information about the images.
--
-- * 'dwirsNextToken' - The token to use to retrieve the next set of results, or null if no more results are available.
--
-- * 'dwirsResponseStatus' - -- | The response status code.
describeWorkspaceImagesResponse
    :: Int -- ^ 'dwirsResponseStatus'
    -> DescribeWorkspaceImagesResponse
describeWorkspaceImagesResponse pResponseStatus_ =
  DescribeWorkspaceImagesResponse'
    { _dwirsImages = Nothing
    , _dwirsNextToken = Nothing
    , _dwirsResponseStatus = pResponseStatus_
    }


-- | Information about the images.
dwirsImages :: Lens' DescribeWorkspaceImagesResponse [WorkspaceImage]
dwirsImages = lens _dwirsImages (\ s a -> s{_dwirsImages = a}) . _Default . _Coerce

-- | The token to use to retrieve the next set of results, or null if no more results are available.
dwirsNextToken :: Lens' DescribeWorkspaceImagesResponse (Maybe Text)
dwirsNextToken = lens _dwirsNextToken (\ s a -> s{_dwirsNextToken = a})

-- | -- | The response status code.
dwirsResponseStatus :: Lens' DescribeWorkspaceImagesResponse Int
dwirsResponseStatus = lens _dwirsResponseStatus (\ s a -> s{_dwirsResponseStatus = a})

instance NFData DescribeWorkspaceImagesResponse where
