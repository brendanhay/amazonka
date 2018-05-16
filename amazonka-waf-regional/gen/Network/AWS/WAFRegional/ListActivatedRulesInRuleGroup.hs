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
-- Module      : Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of 'ActivatedRule' objects.
--
--
module Network.AWS.WAFRegional.ListActivatedRulesInRuleGroup
    (
    -- * Creating a Request
      listActivatedRulesInRuleGroup
    , ListActivatedRulesInRuleGroup
    -- * Request Lenses
    , larirgRuleGroupId
    , larirgNextMarker
    , larirgLimit

    -- * Destructuring the Response
    , listActivatedRulesInRuleGroupResponse
    , ListActivatedRulesInRuleGroupResponse
    -- * Response Lenses
    , larirgrsNextMarker
    , larirgrsActivatedRules
    , larirgrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.WAFRegional.Types
import Network.AWS.WAFRegional.Types.Product

-- | /See:/ 'listActivatedRulesInRuleGroup' smart constructor.
data ListActivatedRulesInRuleGroup = ListActivatedRulesInRuleGroup'
  { _larirgRuleGroupId :: !(Maybe Text)
  , _larirgNextMarker  :: !(Maybe Text)
  , _larirgLimit       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActivatedRulesInRuleGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larirgRuleGroupId' - The @RuleGroupId@ of the 'RuleGroup' for which you want to get a list of 'ActivatedRule' objects.
--
-- * 'larirgNextMarker' - If you specify a value for @Limit@ and you have more @ActivatedRules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ActivatedRules@ . For the second and subsequent @ListActivatedRulesInRuleGroup@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ActivatedRules@ .
--
-- * 'larirgLimit' - Specifies the number of @ActivatedRules@ that you want AWS WAF to return for this request. If you have more @ActivatedRules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ActivatedRules@ .
listActivatedRulesInRuleGroup
    :: ListActivatedRulesInRuleGroup
listActivatedRulesInRuleGroup =
  ListActivatedRulesInRuleGroup'
    { _larirgRuleGroupId = Nothing
    , _larirgNextMarker = Nothing
    , _larirgLimit = Nothing
    }


-- | The @RuleGroupId@ of the 'RuleGroup' for which you want to get a list of 'ActivatedRule' objects.
larirgRuleGroupId :: Lens' ListActivatedRulesInRuleGroup (Maybe Text)
larirgRuleGroupId = lens _larirgRuleGroupId (\ s a -> s{_larirgRuleGroupId = a})

-- | If you specify a value for @Limit@ and you have more @ActivatedRules@ than the value of @Limit@ , AWS WAF returns a @NextMarker@ value in the response that allows you to list another group of @ActivatedRules@ . For the second and subsequent @ListActivatedRulesInRuleGroup@ requests, specify the value of @NextMarker@ from the previous response to get information about another batch of @ActivatedRules@ .
larirgNextMarker :: Lens' ListActivatedRulesInRuleGroup (Maybe Text)
larirgNextMarker = lens _larirgNextMarker (\ s a -> s{_larirgNextMarker = a})

-- | Specifies the number of @ActivatedRules@ that you want AWS WAF to return for this request. If you have more @ActivatedRules@ than the number that you specify for @Limit@ , the response includes a @NextMarker@ value that you can use to get another batch of @ActivatedRules@ .
larirgLimit :: Lens' ListActivatedRulesInRuleGroup (Maybe Natural)
larirgLimit = lens _larirgLimit (\ s a -> s{_larirgLimit = a}) . mapping _Nat

instance AWSRequest ListActivatedRulesInRuleGroup
         where
        type Rs ListActivatedRulesInRuleGroup =
             ListActivatedRulesInRuleGroupResponse
        request = postJSON wAFRegional
        response
          = receiveJSON
              (\ s h x ->
                 ListActivatedRulesInRuleGroupResponse' <$>
                   (x .?> "NextMarker") <*>
                     (x .?> "ActivatedRules" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListActivatedRulesInRuleGroup where

instance NFData ListActivatedRulesInRuleGroup where

instance ToHeaders ListActivatedRulesInRuleGroup
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSWAF_Regional_20161128.ListActivatedRulesInRuleGroup"
                       :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListActivatedRulesInRuleGroup where
        toJSON ListActivatedRulesInRuleGroup'{..}
          = object
              (catMaybes
                 [("RuleGroupId" .=) <$> _larirgRuleGroupId,
                  ("NextMarker" .=) <$> _larirgNextMarker,
                  ("Limit" .=) <$> _larirgLimit])

instance ToPath ListActivatedRulesInRuleGroup where
        toPath = const "/"

instance ToQuery ListActivatedRulesInRuleGroup where
        toQuery = const mempty

-- | /See:/ 'listActivatedRulesInRuleGroupResponse' smart constructor.
data ListActivatedRulesInRuleGroupResponse = ListActivatedRulesInRuleGroupResponse'
  { _larirgrsNextMarker     :: !(Maybe Text)
  , _larirgrsActivatedRules :: !(Maybe [ActivatedRule])
  , _larirgrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListActivatedRulesInRuleGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larirgrsNextMarker' - If you have more @ActivatedRules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ActivatedRules@ , submit another @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
--
-- * 'larirgrsActivatedRules' - An array of @ActivatedRules@ objects.
--
-- * 'larirgrsResponseStatus' - -- | The response status code.
listActivatedRulesInRuleGroupResponse
    :: Int -- ^ 'larirgrsResponseStatus'
    -> ListActivatedRulesInRuleGroupResponse
listActivatedRulesInRuleGroupResponse pResponseStatus_ =
  ListActivatedRulesInRuleGroupResponse'
    { _larirgrsNextMarker = Nothing
    , _larirgrsActivatedRules = Nothing
    , _larirgrsResponseStatus = pResponseStatus_
    }


-- | If you have more @ActivatedRules@ than the number that you specified for @Limit@ in the request, the response includes a @NextMarker@ value. To list more @ActivatedRules@ , submit another @ListActivatedRulesInRuleGroup@ request, and specify the @NextMarker@ value from the response in the @NextMarker@ value in the next request.
larirgrsNextMarker :: Lens' ListActivatedRulesInRuleGroupResponse (Maybe Text)
larirgrsNextMarker = lens _larirgrsNextMarker (\ s a -> s{_larirgrsNextMarker = a})

-- | An array of @ActivatedRules@ objects.
larirgrsActivatedRules :: Lens' ListActivatedRulesInRuleGroupResponse [ActivatedRule]
larirgrsActivatedRules = lens _larirgrsActivatedRules (\ s a -> s{_larirgrsActivatedRules = a}) . _Default . _Coerce

-- | -- | The response status code.
larirgrsResponseStatus :: Lens' ListActivatedRulesInRuleGroupResponse Int
larirgrsResponseStatus = lens _larirgrsResponseStatus (\ s a -> s{_larirgrsResponseStatus = a})

instance NFData ListActivatedRulesInRuleGroupResponse
         where
