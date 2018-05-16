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
-- Module      : Network.AWS.WAFRegional.ListSqlInjectionMatchSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'SqlInjectionMatchSet' objects.
--
--
module Network.AWS.WAFRegional.ListSqlInjectionMatchSets
    (
    -- * Creating a Request
      listSqlInjectionMatchSets
    , ListSqlInjectionMatchSets
    -- * Request Lenses
    , lsimsNextMarker
    , lsimsLimit

    -- * Destructuring the Response
    , listSqlInjectionMatchSetsResponse
    , ListSqlInjectionMatchSetsResponse
    -- * Response Lenses
    , lsimsrsNextMarker
    , lsimsrsSqlInjectionMatchSets
    , lsimsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | A request to list the 'SqlInjectionMatchSet' objects created by the current AWS account.
--
--
--
-- /See:/ 'listSqlInjectionMatchSets' smart constructor.
data ListSqlInjectionMatchSets = ListSqlInjectionMatchSets'
  { _lsimsNextMarker :: !(Maybe Text)
  , _lsimsLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSqlInjectionMatchSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsimsNextMarker' - If you specify a value for @Limit@ and you have more 'SqlInjectionMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SqlInjectionMatchSets@ . For the second and subsequent @ListSqlInjectionMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SqlInjectionMatchSets@ .
--
-- * 'lsimsLimit' - Specifies the number of 'SqlInjectionMatchSet' objects that you want AWS WAF to return for this request. If you have more @SqlInjectionMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
listSqlInjectionMatchSets
    :: ListSqlInjectionMatchSets
listSqlInjectionMatchSets =
  ListSqlInjectionMatchSets' {_lsimsNextMarker = Nothing, _lsimsLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more 'SqlInjectionMatchSet' objects than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @SqlInjectionMatchSets@ . For the second and subsequent @ListSqlInjectionMatchSets@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @SqlInjectionMatchSets@ .
lsimsNextMarker :: Lens' ListSqlInjectionMatchSets (Maybe Text)
lsimsNextMarker = lens _lsimsNextMarker (\ s a -> s{_lsimsNextMarker = a})

-- | Specifies the number of 'SqlInjectionMatchSet' objects that you want AWS WAF to return for this request. If you have more @SqlInjectionMatchSet@ objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
lsimsLimit :: Lens' ListSqlInjectionMatchSets (Maybe Natural)
lsimsLimit = lens _lsimsLimit (\ s a -> s{_lsimsLimit = a}) . mapping _Nat

instance AWSRequest ListSqlInjectionMatchSets where
        type Rs ListSqlInjectionMatchSets =
             ListSqlInjectionMatchSetsResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListSqlInjectionMatchSetsResponse' <$>
                   (x .?> "NextMarker") <*>
                     (x .?> "SqlInjectionMatchSets" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListSqlInjectionMatchSets where

instance NFData ListSqlInjectionMatchSets where

instance ToHeaders ListSqlInjectionMatchSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListSqlInjectionMatchSets"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSqlInjectionMatchSets where
        toJSON ListSqlInjectionMatchSets'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lsimsNextMarker,
                  ("Limit" .=) <$> _lsimsLimit])

instance ToPath ListSqlInjectionMatchSets where
        toPath = const "/"

instance ToQuery ListSqlInjectionMatchSets where
        toQuery = const mempty

-- | The response to a 'ListSqlInjectionMatchSets' request.
--
--
--
-- /See:/ 'listSqlInjectionMatchSetsResponse' smart constructor.
data ListSqlInjectionMatchSetsResponse = ListSqlInjectionMatchSetsResponse'
  { _lsimsrsNextMarker            :: !(Maybe Text)
  , _lsimsrsSqlInjectionMatchSets :: !(Maybe [SqlInjectionMatchSetSummary])
  , _lsimsrsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSqlInjectionMatchSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsimsrsNextMarker' - If you have more 'SqlInjectionMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SqlInjectionMatchSet@ objects, submit another @ListSqlInjectionMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lsimsrsSqlInjectionMatchSets' - An array of 'SqlInjectionMatchSetSummary' objects.
--
-- * 'lsimsrsResponseStatus' - -- | The response status code.
listSqlInjectionMatchSetsResponse
    :: Int -- ^ 'lsimsrsResponseStatus'
    -> ListSqlInjectionMatchSetsResponse
listSqlInjectionMatchSetsResponse pResponseStatus_ =
  ListSqlInjectionMatchSetsResponse'
    { _lsimsrsNextMarker = Nothing
    , _lsimsrsSqlInjectionMatchSets = Nothing
    , _lsimsrsResponseStatus = pResponseStatus_
    }


-- | If you have more 'SqlInjectionMatchSet' objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @SqlInjectionMatchSet@ objects, submit another @ListSqlInjectionMatchSets@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lsimsrsNextMarker :: Lens' ListSqlInjectionMatchSetsResponse (Maybe Text)
lsimsrsNextMarker = lens _lsimsrsNextMarker (\ s a -> s{_lsimsrsNextMarker = a})

-- | An array of 'SqlInjectionMatchSetSummary' objects.
lsimsrsSqlInjectionMatchSets :: Lens' ListSqlInjectionMatchSetsResponse [SqlInjectionMatchSetSummary]
lsimsrsSqlInjectionMatchSets = lens _lsimsrsSqlInjectionMatchSets (\ s a -> s{_lsimsrsSqlInjectionMatchSets = a}) . _Default . _Coerce

-- | -- | The response status code.
lsimsrsResponseStatus :: Lens' ListSqlInjectionMatchSetsResponse Int
lsimsrsResponseStatus = lens _lsimsrsResponseStatus (\ s a -> s{_lsimsrsResponseStatus = a})

instance NFData ListSqlInjectionMatchSetsResponse
         where
