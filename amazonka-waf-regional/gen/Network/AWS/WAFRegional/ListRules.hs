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
-- Module      : Network.AWS.WAFRegional.ListRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleSummary' objects.
--
--
module Network.AWS.WAFRegional.ListRules
    (
    -- * Creating a Request
      listRules
    , ListRules
    -- * Request Lenses
    , lrNextMarker
    , lrLimit

    -- * Destructuring the Response
    , listRulesResponse
    , ListRulesResponse
    -- * Response Lenses
    , lrrsRules
    , lrrsNextMarker
    , lrrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listRules' smart constructor.
data ListRules = ListRules'
  { _lrNextMarker :: !(Maybe Text)
  , _lrLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextMarker' - If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
--
-- * 'lrLimit' - Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
listRules
    :: ListRules
listRules = ListRules' {_lrNextMarker = Nothing, _lrLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @Rules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @Rules@ . For the second and subsequent @ListRules@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @Rules@ .
lrNextMarker :: Lens' ListRules (Maybe Text)
lrNextMarker = lens _lrNextMarker (\ s a -> s{_lrNextMarker = a})

-- | Specifies the number of @Rules@ that you want AWS WAF to return for this request. If you have more @Rules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @Rules@ .
lrLimit :: Lens' ListRules (Maybe Natural)
lrLimit = lens _lrLimit (\ s a -> s{_lrLimit = a}) . mapping _Nat

instance AWSRequest ListRules where
        type Rs ListRules = ListRulesResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListRulesResponse' <$>
                   (x .?> "Rules" .!@ mempty) <*> (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListRules where

instance NFData ListRules where

instance ToHeaders ListRules where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListRules" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRules where
        toJSON ListRules'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lrNextMarker,
                  ("Limit" .=) <$> _lrLimit])

instance ToPath ListRules where
        toPath = const "/"

instance ToQuery ListRules where
        toQuery = const mempty

-- | /See:/ 'listRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { _lrrsRules          :: !(Maybe [RuleSummary])
  , _lrrsNextMarker     :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRules' - An array of 'RuleSummary' objects.
--
-- * 'lrrsNextMarker' - If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRulesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRulesResponse
listRulesResponse pResponseStatus_ =
  ListRulesResponse'
    { _lrrsRules = Nothing
    , _lrrsNextMarker = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }


-- | An array of 'RuleSummary' objects.
lrrsRules :: Lens' ListRulesResponse [RuleSummary]
lrrsRules = lens _lrrsRules (\ s a -> s{_lrrsRules = a}) . _Default . _Coerce

-- | If you have more @Rules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @Rules@ , submit another @ListRules@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lrrsNextMarker :: Lens' ListRulesResponse (Maybe Text)
lrrsNextMarker = lens _lrrsNextMarker (\ s a -> s{_lrrsNextMarker = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRulesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListRulesResponse where
