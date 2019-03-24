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
-- Module      : Network.AWS.Snowball.ListCompatibleImages
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action returns a list of the different Amazon EC2 Amazon Machine Images (AMIs) that are owned by your AWS account that would be supported for use on @EDGE@ , @EDGE_C@ , and @EDGE_CG@ devices. For more information on compatible AMIs, see <http://docs.aws.amazon.com/snowball/latest/developer-guide/using-ec2.html Using Amazon EC2 Compute Instances> in the /AWS Snowball Developer Guide/ .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListCompatibleImages
    (
    -- * Creating a Request
      listCompatibleImages
    , ListCompatibleImages
    -- * Request Lenses
    , lciNextToken
    , lciMaxResults

    -- * Destructuring the Response
    , listCompatibleImagesResponse
    , ListCompatibleImagesResponse
    -- * Response Lenses
    , lcirsCompatibleImages
    , lcirsNextToken
    , lcirsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'listCompatibleImages' smart constructor.
data ListCompatibleImages = ListCompatibleImages'
  { _lciNextToken  :: !(Maybe Text)
  , _lciMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCompatibleImages' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lciNextToken' - HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
--
-- * 'lciMaxResults' - The maximum number of results for the list of compatible images. Currently, each supported device can store 10 AMIs.
listCompatibleImages
    :: ListCompatibleImages
listCompatibleImages =
  ListCompatibleImages' {_lciNextToken = Nothing, _lciMaxResults = Nothing}


-- | HTTP requests are stateless. To identify what object comes "next" in the list of compatible images, you can specify a value for @NextToken@ as the starting point for your list of returned images.
lciNextToken :: Lens' ListCompatibleImages (Maybe Text)
lciNextToken = lens _lciNextToken (\ s a -> s{_lciNextToken = a})

-- | The maximum number of results for the list of compatible images. Currently, each supported device can store 10 AMIs.
lciMaxResults :: Lens' ListCompatibleImages (Maybe Natural)
lciMaxResults = lens _lciMaxResults (\ s a -> s{_lciMaxResults = a}) . mapping _Nat

instance AWSPager ListCompatibleImages where
        page rq rs
          | stop (rs ^. lcirsNextToken) = Nothing
          | stop (rs ^. lcirsCompatibleImages) = Nothing
          | otherwise =
            Just $ rq & lciNextToken .~ rs ^. lcirsNextToken

instance AWSRequest ListCompatibleImages where
        type Rs ListCompatibleImages =
             ListCompatibleImagesResponse
        request = postJSON snowball
        response
          = receiveJSON
              (\ s h x ->
                 ListCompatibleImagesResponse' <$>
                   (x .?> "CompatibleImages" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListCompatibleImages where

instance NFData ListCompatibleImages where

instance ToHeaders ListCompatibleImages where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.ListCompatibleImages"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCompatibleImages where
        toJSON ListCompatibleImages'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lciNextToken,
                  ("MaxResults" .=) <$> _lciMaxResults])

instance ToPath ListCompatibleImages where
        toPath = const "/"

instance ToQuery ListCompatibleImages where
        toQuery = const mempty

-- | /See:/ 'listCompatibleImagesResponse' smart constructor.
data ListCompatibleImagesResponse = ListCompatibleImagesResponse'
  { _lcirsCompatibleImages :: !(Maybe [CompatibleImage])
  , _lcirsNextToken        :: !(Maybe Text)
  , _lcirsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCompatibleImagesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcirsCompatibleImages' - A JSON-formatted object that describes a compatible AMI.
--
-- * 'lcirsNextToken' - Because HTTP requests are stateless, this is the starting point for your next list of returned images.
--
-- * 'lcirsResponseStatus' - -- | The response status code.
listCompatibleImagesResponse
    :: Int -- ^ 'lcirsResponseStatus'
    -> ListCompatibleImagesResponse
listCompatibleImagesResponse pResponseStatus_ =
  ListCompatibleImagesResponse'
    { _lcirsCompatibleImages = Nothing
    , _lcirsNextToken = Nothing
    , _lcirsResponseStatus = pResponseStatus_
    }


-- | A JSON-formatted object that describes a compatible AMI.
lcirsCompatibleImages :: Lens' ListCompatibleImagesResponse [CompatibleImage]
lcirsCompatibleImages = lens _lcirsCompatibleImages (\ s a -> s{_lcirsCompatibleImages = a}) . _Default . _Coerce

-- | Because HTTP requests are stateless, this is the starting point for your next list of returned images.
lcirsNextToken :: Lens' ListCompatibleImagesResponse (Maybe Text)
lcirsNextToken = lens _lcirsNextToken (\ s a -> s{_lcirsNextToken = a})

-- | -- | The response status code.
lcirsResponseStatus :: Lens' ListCompatibleImagesResponse Int
lcirsResponseStatus = lens _lcirsResponseStatus (\ s a -> s{_lcirsResponseStatus = a})

instance NFData ListCompatibleImagesResponse where
