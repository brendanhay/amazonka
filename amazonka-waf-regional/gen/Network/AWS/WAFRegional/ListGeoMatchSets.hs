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
-- Module      : Network.AWS.WAFRegional.ListGeoMatchSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'GeoMatchSetSummary' objects in the response.
--
--
module Network.AWS.WAFRegional.ListGeoMatchSets
    (
    -- * Creating a Request
      listGeoMatchSets
    , ListGeoMatchSets
    -- * Request Lenses
    , lgmsNextMarker
    , lgmsLimit

    -- * Destructuring the Response
    , listGeoMatchSetsResponse
    , ListGeoMatchSetsResponse
    -- * Response Lenses
    , lgmsrsGeoMatchSets
    , lgmsrsNextMarker
    , lgmsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listGeoMatchSets' smart constructor.
data ListGeoMatchSets = ListGeoMatchSets'
  { _lgmsNextMarker :: !(Maybe Text)
  , _lgmsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGeoMatchSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgmsNextMarker' - If you specify a value for @Limit@ and you have more @GeoMatchSet@ s than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @GeoMatchSet@ objects. For the second and subsequent @ListGeoMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @GeoMatchSet@ objects.
--
-- * 'lgmsLimit' - Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to return for this request. If you have more @GeoMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @GeoMatchSet@ objects.
listGeoMatchSets
    :: ListGeoMatchSets
listGeoMatchSets =
  ListGeoMatchSets' {_lgmsNextMarker = Nothing, _lgmsLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @GeoMatchSet@ s than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @GeoMatchSet@ objects. For the second and subsequent @ListGeoMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @GeoMatchSet@ objects.
lgmsNextMarker :: Lens' ListGeoMatchSets (Maybe Text)
lgmsNextMarker = lens _lgmsNextMarker (\ s a -> s{_lgmsNextMarker = a})

-- | Specifies the number of @GeoMatchSet@ objects that you want AWS WAF to return for this request. If you have more @GeoMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @GeoMatchSet@ objects.
lgmsLimit :: Lens' ListGeoMatchSets (Maybe Natural)
lgmsLimit = lens _lgmsLimit (\ s a -> s{_lgmsLimit = a}) . mapping _Nat

instance AWSRequest ListGeoMatchSets where
        type Rs ListGeoMatchSets = ListGeoMatchSetsResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListGeoMatchSetsResponse' <$>
                   (x .?> "GeoMatchSets" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListGeoMatchSets where

instance NFData ListGeoMatchSets where

instance ToHeaders ListGeoMatchSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListGeoMatchSets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListGeoMatchSets where
        toJSON ListGeoMatchSets'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lgmsNextMarker,
                  ("Limit" .=) <$> _lgmsLimit])

instance ToPath ListGeoMatchSets where
        toPath = const "/"

instance ToQuery ListGeoMatchSets where
        toQuery = const mempty

-- | /See:/ 'listGeoMatchSetsResponse' smart constructor.
data ListGeoMatchSetsResponse = ListGeoMatchSetsResponse'
  { _lgmsrsGeoMatchSets   :: !(Maybe [GeoMatchSetSummary])
  , _lgmsrsNextMarker     :: !(Maybe Text)
  , _lgmsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListGeoMatchSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lgmsrsGeoMatchSets' - An array of 'GeoMatchSetSummary' objects.
--
-- * 'lgmsrsNextMarker' - If you have more @GeoMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lgmsrsResponseStatus' - -- | The response status code.
listGeoMatchSetsResponse
    :: Int -- ^ 'lgmsrsResponseStatus'
    -> ListGeoMatchSetsResponse
listGeoMatchSetsResponse pResponseStatus_ =
  ListGeoMatchSetsResponse'
    { _lgmsrsGeoMatchSets = Nothing
    , _lgmsrsNextMarker = Nothing
    , _lgmsrsResponseStatus = pResponseStatus_
    }


-- | An array of 'GeoMatchSetSummary' objects.
lgmsrsGeoMatchSets :: Lens' ListGeoMatchSetsResponse [GeoMatchSetSummary]
lgmsrsGeoMatchSets = lens _lgmsrsGeoMatchSets (\ s a -> s{_lgmsrsGeoMatchSets = a}) . _Default . _Coerce

-- | If you have more @GeoMatchSet@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @GeoMatchSet@ objects, submit another @ListGeoMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lgmsrsNextMarker :: Lens' ListGeoMatchSetsResponse (Maybe Text)
lgmsrsNextMarker = lens _lgmsrsNextMarker (\ s a -> s{_lgmsrsNextMarker = a})

-- | -- | The response status code.
lgmsrsResponseStatus :: Lens' ListGeoMatchSetsResponse Int
lgmsrsResponseStatus = lens _lgmsrsResponseStatus (\ s a -> s{_lgmsrsResponseStatus = a})

instance NFData ListGeoMatchSetsResponse where
