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
-- Module      : Network.AWS.Rekognition.ListStreamProcessors
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of stream processors that you have created with .
--
--
--
-- This operation returns paginated results.
module Network.AWS.Rekognition.ListStreamProcessors
    (
    -- * Creating a Request
      listStreamProcessors
    , ListStreamProcessors
    -- * Request Lenses
    , lspNextToken
    , lspMaxResults

    -- * Destructuring the Response
    , listStreamProcessorsResponse
    , ListStreamProcessorsResponse
    -- * Response Lenses
    , lsprsStreamProcessors
    , lsprsNextToken
    , lsprsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Rekognition.Types
import Network.AWS.Rekognition.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listStreamProcessors' smart constructor.
data ListStreamProcessors = ListStreamProcessors'
  { _lspNextToken  :: !(Maybe Text)
  , _lspMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamProcessors' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lspNextToken' - If the previous response was incomplete (because there are more stream processors to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of stream processors.
--
-- * 'lspMaxResults' - Maximum number of stream processors you want Rekognition Video to return in the response. The default is 1000.
listStreamProcessors
    :: ListStreamProcessors
listStreamProcessors =
  ListStreamProcessors' {_lspNextToken = Nothing, _lspMaxResults = Nothing}


-- | If the previous response was incomplete (because there are more stream processors to retrieve), Rekognition Video returns a pagination token in the response. You can use this pagination token to retrieve the next set of stream processors.
lspNextToken :: Lens' ListStreamProcessors (Maybe Text)
lspNextToken = lens _lspNextToken (\ s a -> s{_lspNextToken = a})

-- | Maximum number of stream processors you want Rekognition Video to return in the response. The default is 1000.
lspMaxResults :: Lens' ListStreamProcessors (Maybe Natural)
lspMaxResults = lens _lspMaxResults (\ s a -> s{_lspMaxResults = a}) . mapping _Nat

instance AWSPager ListStreamProcessors where
        page rq rs
          | stop (rs ^. lsprsNextToken) = Nothing
          | stop (rs ^. lsprsStreamProcessors) = Nothing
          | otherwise =
            Just $ rq & lspNextToken .~ rs ^. lsprsNextToken

instance AWSRequest ListStreamProcessors where
        type Rs ListStreamProcessors =
             ListStreamProcessorsResponse
        request = postJSON rekognition
        response
          = receiveJSON
              (\ s h x ->
                 ListStreamProcessorsResponse' <$>
                   (x .?> "StreamProcessors" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListStreamProcessors where

instance NFData ListStreamProcessors where

instance ToHeaders ListStreamProcessors where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("RekognitionService.ListStreamProcessors" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListStreamProcessors where
        toJSON ListStreamProcessors'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lspNextToken,
                  ("MaxResults" .=) <$> _lspMaxResults])

instance ToPath ListStreamProcessors where
        toPath = const "/"

instance ToQuery ListStreamProcessors where
        toQuery = const mempty

-- | /See:/ 'listStreamProcessorsResponse' smart constructor.
data ListStreamProcessorsResponse = ListStreamProcessorsResponse'
  { _lsprsStreamProcessors :: !(Maybe [StreamProcessor])
  , _lsprsNextToken        :: !(Maybe Text)
  , _lsprsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListStreamProcessorsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsprsStreamProcessors' - List of stream processors that you have created.
--
-- * 'lsprsNextToken' - If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of stream processors.
--
-- * 'lsprsResponseStatus' - -- | The response status code.
listStreamProcessorsResponse
    :: Int -- ^ 'lsprsResponseStatus'
    -> ListStreamProcessorsResponse
listStreamProcessorsResponse pResponseStatus_ =
  ListStreamProcessorsResponse'
    { _lsprsStreamProcessors = Nothing
    , _lsprsNextToken = Nothing
    , _lsprsResponseStatus = pResponseStatus_
    }


-- | List of stream processors that you have created.
lsprsStreamProcessors :: Lens' ListStreamProcessorsResponse [StreamProcessor]
lsprsStreamProcessors = lens _lsprsStreamProcessors (\ s a -> s{_lsprsStreamProcessors = a}) . _Default . _Coerce

-- | If the response is truncated, Rekognition Video returns this token that you can use in the subsequent request to retrieve the next set of stream processors.
lsprsNextToken :: Lens' ListStreamProcessorsResponse (Maybe Text)
lsprsNextToken = lens _lsprsNextToken (\ s a -> s{_lsprsNextToken = a})

-- | -- | The response status code.
lsprsResponseStatus :: Lens' ListStreamProcessorsResponse Int
lsprsResponseStatus = lens _lsprsResponseStatus (\ s a -> s{_lsprsResponseStatus = a})

instance NFData ListStreamProcessorsResponse where
