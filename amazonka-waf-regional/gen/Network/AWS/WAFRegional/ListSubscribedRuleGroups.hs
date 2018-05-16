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
-- Module      : Network.AWS.WAFRegional.ListSubscribedRuleGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'RuleGroup' objects that you are subscribed to.
--
--
module Network.AWS.WAFRegional.ListSubscribedRuleGroups
    (
    -- * Creating a Request
      listSubscribedRuleGroups
    , ListSubscribedRuleGroups
    -- * Request Lenses
    , lsrgNextMarker
    , lsrgLimit

    -- * Destructuring the Response
    , listSubscribedRuleGroupsResponse
    , ListSubscribedRuleGroupsResponse
    -- * Response Lenses
    , lsrgrsRuleGroups
    , lsrgrsNextMarker
    , lsrgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listSubscribedRuleGroups' smart constructor.
data ListSubscribedRuleGroups = ListSubscribedRuleGroups'
  { _lsrgNextMarker :: !(Maybe Text)
  , _lsrgLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscribedRuleGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrgNextMarker' - If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
--
-- * 'lsrgLimit' - Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
listSubscribedRuleGroups
    :: ListSubscribedRuleGroups
listSubscribedRuleGroups =
  ListSubscribedRuleGroups' {_lsrgNextMarker = Nothing, _lsrgLimit = Nothing}


-- | If you specify a value for @Limit@ and you have more @ByteMatchSets@ subscribed rule groups than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of subscribed rule groups. For the second and subsequent @ListSubscribedRuleGroupsRequest@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of subscribed rule groups.
lsrgNextMarker :: Lens' ListSubscribedRuleGroups (Maybe Text)
lsrgNextMarker = lens _lsrgNextMarker (\ s a -> s{_lsrgNextMarker = a})

-- | Specifies the number of subscribed rule groups that you want AWS WAF to return for this request. If you have more objects than the number you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of objects.
lsrgLimit :: Lens' ListSubscribedRuleGroups (Maybe Natural)
lsrgLimit = lens _lsrgLimit (\ s a -> s{_lsrgLimit = a}) . mapping _Nat

instance AWSRequest ListSubscribedRuleGroups where
        type Rs ListSubscribedRuleGroups =
             ListSubscribedRuleGroupsResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListSubscribedRuleGroupsResponse' <$>
                   (x .?> "RuleGroups" .!@ mempty) <*>
                     (x .?> "NextMarker")
                     <*> (pure (fromEnum s)))

instance Hashable ListSubscribedRuleGroups where

instance NFData ListSubscribedRuleGroups where

instance ToHeaders ListSubscribedRuleGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListSubscribedRuleGroups"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListSubscribedRuleGroups where
        toJSON ListSubscribedRuleGroups'{..}
          = object
              (catMaybes
                 [("NextMarker" .=) <$> _lsrgNextMarker,
                  ("Limit" .=) <$> _lsrgLimit])

instance ToPath ListSubscribedRuleGroups where
        toPath = const "/"

instance ToQuery ListSubscribedRuleGroups where
        toQuery = const mempty

-- | /See:/ 'listSubscribedRuleGroupsResponse' smart constructor.
data ListSubscribedRuleGroupsResponse = ListSubscribedRuleGroupsResponse'
  { _lsrgrsRuleGroups     :: !(Maybe [SubscribedRuleGroupSummary])
  , _lsrgrsNextMarker     :: !(Maybe Text)
  , _lsrgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListSubscribedRuleGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lsrgrsRuleGroups' - An array of 'RuleGroup' objects.
--
-- * 'lsrgrsNextMarker' - If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'lsrgrsResponseStatus' - -- | The response status code.
listSubscribedRuleGroupsResponse
    :: Int -- ^ 'lsrgrsResponseStatus'
    -> ListSubscribedRuleGroupsResponse
listSubscribedRuleGroupsResponse pResponseStatus_ =
  ListSubscribedRuleGroupsResponse'
    { _lsrgrsRuleGroups = Nothing
    , _lsrgrsNextMarker = Nothing
    , _lsrgrsResponseStatus = pResponseStatus_
    }


-- | An array of 'RuleGroup' objects.
lsrgrsRuleGroups :: Lens' ListSubscribedRuleGroupsResponse [SubscribedRuleGroupSummary]
lsrgrsRuleGroups = lens _lsrgrsRuleGroups (\ s a -> s{_lsrgrsRuleGroups = a}) . _Default . _Coerce

-- | If you have more objects than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more objects, submit another @ListSubscribedRuleGroups@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
lsrgrsNextMarker :: Lens' ListSubscribedRuleGroupsResponse (Maybe Text)
lsrgrsNextMarker = lens _lsrgrsNextMarker (\ s a -> s{_lsrgrsNextMarker = a})

-- | -- | The response status code.
lsrgrsResponseStatus :: Lens' ListSubscribedRuleGroupsResponse Int
lsrgrsResponseStatus = lens _lsrgrsResponseStatus (\ s a -> s{_lsrgrsResponseStatus = a})

instance NFData ListSubscribedRuleGroupsResponse
         where
