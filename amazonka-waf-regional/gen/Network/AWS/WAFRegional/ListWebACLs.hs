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
-- Module      : Network.AWS.WAFRegional.ListWebACLs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'WebACLSummary' objects in the response.
--
--
module Network.AWS.WAFRegional.ListWebACLs
    (
    -- * Creating a Request
      listWebACLs
    , ListWebACLs
    -- * Request Lenses
    , lwaNextMarker
    , lwaLimit

    -- * Destructuring the Response
    , listWebACLsResponse
    , ListWebACLsResponse
    -- * Response Lenses
    , lwarsWebACLs
    , lwarsNextMarker
    , lwarsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listWebACLs' smart constructor.
data ListWebACLs = ListWebACLs'
  { _lwaNextMarker :: !(Maybe Text)
  , _lwaLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWebACLs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwaNextMarker' - If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
--
-- * 'lwaLimit' - Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
listWebACLs
    :: ListWebACLs
listWebACLs = ListWebACLs' {_lwaNextMarker = Nothing, _lwaLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @WebACL@ objects than the number that you specify for @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @WebACL@ objects. For the second and subsequent @ListWebACLs@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @WebACL@ objects.
lwaNextMarker :: Lens' ListWebACLs (Maybe Text)
lwaNextMarker = lens _lwaNextMarker (\ s a -> s{_lwaNextMarker = a})

-- | Specifies the number of @WebACL@ objects that you want AWS WAF to return for this request. If you have more @WebACL@ objects than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @WebACL@ objects.
lwaLimit :: Lens' ListWebACLs (Maybe Natural)
lwaLimit = lens _lwaLimit (\ s a -> s{_lwaLimit = a}) . mapping _Nat

instance AWSRequest ListWebACLs where
        type Rs ListWebACLs = ListWebACLsResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListWebACLsResponse' <$>
                   (x .?> "WebACLs" .!@ mempty) <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListWebACLs where

instance NFData ListWebACLs where

instance ToHeaders ListWebACLs where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListWebACLs" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListWebACLs where
        toJSON ListWebACLs'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lwaNextMarker,
                  ("Limit" .=) <$> _lwaLimit])

instance ToPath ListWebACLs where
        toPath = const "/"

instance ToQuery ListWebACLs where
        toQuery = const mempty

-- | /See:/ 'listWebACLsResponse' smart constructor.
data ListWebACLsResponse = ListWebACLsResponse'
  { _lwarsWebACLs        :: !(Maybe [WebACLSummary])
  , _lwarsNextMarker     :: !(Maybe Text)
  , _lwarsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListWebACLsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lwarsWebACLs' - An array of 'WebACLSummary' objects.
--
-- * 'lwarsNextMarker' - If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lwarsResponseStatus' - -- | The response status code.
listWebACLsResponse
    :: Int -- ^ 'lwarsResponseStatus'
    -> ListWebACLsResponse
listWebACLsResponse pResponseStatus_ =
  ListWebACLsResponse'
    { _lwarsWebACLs = Nothing
    , _lwarsNextMarker = Nothing
    , _lwarsResponseStatus = pResponseStatus_
    }


-- | An array of 'WebACLSummary' objects.
lwarsWebACLs :: Lens' ListWebACLsResponse [WebACLSummary]
lwarsWebACLs = lens _lwarsWebACLs (\ s a -> s{_lwarsWebACLs = a}) . _Default . _Coerce

-- | If you have more @WebACL@ objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @WebACL@ objects, submit another @ListWebACLs@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lwarsNextMarker :: Lens' ListWebACLsResponse (Maybe Text)
lwarsNextMarker = lens _lwarsNextMarker (\ s a -> s{_lwarsNextMarker = a})

-- | -- | The response status code.
lwarsResponseStatus :: Lens' ListWebACLsResponse Int
lwarsResponseStatus = lens _lwarsResponseStatus (\ s a -> s{_lwarsResponseStatus = a})

instance NFData ListWebACLsResponse where
