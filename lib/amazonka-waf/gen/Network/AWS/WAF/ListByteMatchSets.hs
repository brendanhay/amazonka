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
-- Module      : Network.AWS.WAF.ListByteMatchSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'ByteMatchSetSummary' objects.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListByteMatchSets
    (
    -- * Creating a Request
      listByteMatchSets
    , ListByteMatchSets
    -- * Request Lenses
    , lbmsNextMarker
    , lbmsLimit

    -- * Destructuring the Response
    , listByteMatchSetsResponse
    , ListByteMatchSetsResponse
    -- * Response Lenses
    , lbmsrsByteMatchSets
    , lbmsrsNextMarker
    , lbmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'listByteMatchSets' smart constructor.
data ListByteMatchSets = ListByteMatchSets'
  { _lbmsNextMarker :: !(Maybe Text)
  , _lbmsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListByteMatchSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbmsNextMarker' - If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
--
-- * 'lbmsLimit' - Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
listByteMatchSets
    :: ListByteMatchSets
listByteMatchSets =
  ListByteMatchSets' {_lbmsNextMarker = Nothing, _lbmsLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ByteMatchSets@ . For the second and subsequent @ListByteMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ByteMatchSets@ .
lbmsNextMarker :: Lens' ListByteMatchSets (Maybe Text)
lbmsNextMarker = lens _lbmsNextMarker (\ s a -> s{_lbmsNextMarker = a})

-- | Specifies the number of @ByteMatchSet@ objects that you want AWS WAF to return for this request. If you have more @ByteMatchSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ByteMatchSet@ objects.
lbmsLimit :: Lens' ListByteMatchSets (Maybe Natural)
lbmsLimit = lens _lbmsLimit (\ s a -> s{_lbmsLimit = a}) . mapping _Nat

instance AWSPager ListByteMatchSets where
        page rq rs
          | stop (rs ^. lbmsrsNextMarker) = Nothing
          | stop (rs ^. lbmsrsByteMatchSets) = Nothing
          | otherwise =
            Just $ rq & lbmsNextMarker .~ rs ^. lbmsrsNextMarker

instance AWSRequest ListByteMatchSets where
        type Rs ListByteMatchSets = ListByteMatchSetsResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 ListByteMatchSetsResponse' <$>
                   (x .?> "ByteMatchSets" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListByteMatchSets where

instance NFData ListByteMatchSets where

instance ToHeaders ListByteMatchSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.ListByteMatchSets" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListByteMatchSets where
        toJSON ListByteMatchSets'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lbmsNextMarker,
                  ("Limit" .=) <$> _lbmsLimit])

instance ToPath ListByteMatchSets where
        toPath = const "/"

instance ToQuery ListByteMatchSets where
        toQuery = const mempty

-- | /See:/ 'listByteMatchSetsResponse' smart constructor.
data ListByteMatchSetsResponse = ListByteMatchSetsResponse'
  { _lbmsrsByteMatchSets  :: !(Maybe [ByteMatchSetSummary])
  , _lbmsrsNextMarker     :: !(Maybe Text)
  , _lbmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListByteMatchSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lbmsrsByteMatchSets' - An array of 'ByteMatchSetSummary' objects.
--
-- * 'lbmsrsNextMarker' - If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lbmsrsResponseStatus' - -- | The response status code.
listByteMatchSetsResponse
    :: Int -- ^ 'lbmsrsResponseStatus'
    -> ListByteMatchSetsResponse
listByteMatchSetsResponse pResponseStatus_ =
  ListByteMatchSetsResponse'
    { _lbmsrsByteMatchSets = Nothing
    , _lbmsrsNextMarker = Nothing
    , _lbmsrsResponseStatus = pResponseStatus_
    }


-- | An array of 'ByteMatchSetSummary' objects.
lbmsrsByteMatchSets :: Lens' ListByteMatchSetsResponse [ByteMatchSetSummary]
lbmsrsByteMatchSets = lens _lbmsrsByteMatchSets (\ s a -> s{_lbmsrsByteMatchSets = a}) . _Default . _Coerce

-- | If you have more @ByteMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ByteMatchSet@ objects, submit another @ListByteMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lbmsrsNextMarker :: Lens' ListByteMatchSetsResponse (Maybe Text)
lbmsrsNextMarker = lens _lbmsrsNextMarker (\ s a -> s{_lbmsrsNextMarker = a})

-- | -- | The response status code.
lbmsrsResponseStatus :: Lens' ListByteMatchSetsResponse Int
lbmsrsResponseStatus = lens _lbmsrsResponseStatus (\ s a -> s{_lbmsrsResponseStatus = a})

instance NFData ListByteMatchSetsResponse where
