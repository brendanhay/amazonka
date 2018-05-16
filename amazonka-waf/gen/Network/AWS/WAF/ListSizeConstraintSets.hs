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
-- Module      : Network.AWS.WAF.ListSizeConstraintSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'SizeConstraintSetSummary' objects.
--
--
--
-- This operation returns paginated results.
module Network.AWS.WAF.ListSizeConstraintSets
    (
    -- * Creating a Request
      listSizeConstraintSets
    , ListSizeConstraintSets
    -- * Request Lenses
    , lscsNextMarker
    , lscsLimit

    -- * Destructuring the Response
    , listSizeConstraintSetsResponse
    , ListSizeConstraintSetsResponse
    -- * Response Lenses
    , lscsrsSizeConstraintSets
    , lscsrsNextMarker
    , lscsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAF.Types
import Network.AWS.WAF.Types.Product

-- | /See:/ 'listSizeConstraintSets' smart constructor.
data ListSizeConstraintSets = ListSizeConstraintSets'
  { _lscsNextMarker :: !(Maybe Text)
  , _lscsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSizeConstraintSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscsNextMarker' - If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
--
-- * 'lscsLimit' - Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
listSizeConstraintSets
    :: ListSizeConstraintSets
listSizeConstraintSets =
  ListSizeConstraintSets' {_lscsNextMarker = Nothing, _lscsLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @SizeConstraintSets@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SizeConstraintSets@ . For the second and subsequent @ListSizeConstraintSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SizeConstraintSets@ .
lscsNextMarker :: Lens' ListSizeConstraintSets (Maybe Text)
lscsNextMarker = lens _lscsNextMarker (\ s a -> s{_lscsNextMarker = a})

-- | Specifies the number of @SizeConstraintSet@ objects that you want AWS WAF to return for this request. If you have more @SizeConstraintSets@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @SizeConstraintSet@ objects.
lscsLimit :: Lens' ListSizeConstraintSets (Maybe Natural)
lscsLimit = lens _lscsLimit (\ s a -> s{_lscsLimit = a}) . mapping _Nat

instance AWSPager ListSizeConstraintSets where
        page rq rs
          | stop (rs ^. lscsrsNextMarker) = Nothing
          | stop (rs ^. lscsrsSizeConstraintSets) = Nothing
          | otherwise =
            Just $ rq & lscsNextMarker .~ rs ^. lscsrsNextMarker

instance AWSRequest ListSizeConstraintSets where
        type Rs ListSizeConstraintSets =
             ListSizeConstraintSetsResponse
        request = postJSON waf
        response
          = receiveJSON
              (\ s h x ->
                 ListSizeConstraintSetsResponse' <$>
                   (x .?> "SizeConstraintSets" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListSizeConstraintSets where

instance NFData ListSizeConstraintSets where

instance ToHeaders ListSizeConstraintSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_20150824.ListSizeConstraintSets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSizeConstraintSets where
        toJSON ListSizeConstraintSets'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lscsNextMarker,
                  ("Limit" .=) <$> _lscsLimit])

instance ToPath ListSizeConstraintSets where
        toPath = const "/"

instance ToQuery ListSizeConstraintSets where
        toQuery = const mempty

-- | /See:/ 'listSizeConstraintSetsResponse' smart constructor.
data ListSizeConstraintSetsResponse = ListSizeConstraintSetsResponse'
  { _lscsrsSizeConstraintSets :: !(Maybe [SizeConstraintSetSummary])
  , _lscsrsNextMarker         :: !(Maybe Text)
  , _lscsrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSizeConstraintSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lscsrsSizeConstraintSets' - An array of 'SizeConstraintSetSummary' objects.
--
-- * 'lscsrsNextMarker' - If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lscsrsResponseStatus' - -- | The response status code.
listSizeConstraintSetsResponse
    :: Int -- ^ 'lscsrsResponseStatus'
    -> ListSizeConstraintSetsResponse
listSizeConstraintSetsResponse pResponseStatus_ =
  ListSizeConstraintSetsResponse'
    { _lscsrsSizeConstraintSets = Nothing
    , _lscsrsNextMarker = Nothing
    , _lscsrsResponseStatus = pResponseStatus_
    }


-- | An array of 'SizeConstraintSetSummary' objects.
lscsrsSizeConstraintSets :: Lens' ListSizeConstraintSetsResponse [SizeConstraintSetSummary]
lscsrsSizeConstraintSets = lens _lscsrsSizeConstraintSets (\ s a -> s{_lscsrsSizeConstraintSets = a}) . _Default . _Coerce

-- | If you have more @SizeConstraintSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SizeConstraintSet@ objects, submit another @ListSizeConstraintSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lscsrsNextMarker :: Lens' ListSizeConstraintSetsResponse (Maybe Text)
lscsrsNextMarker = lens _lscsrsNextMarker (\ s a -> s{_lscsrsNextMarker = a})

-- | -- | The response status code.
lscsrsResponseStatus :: Lens' ListSizeConstraintSetsResponse Int
lscsrsResponseStatus = lens _lscsrsResponseStatus (\ s a -> s{_lscsrsResponseStatus = a})

instance NFData ListSizeConstraintSetsResponse where
