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
-- Module      : Network.AWS.IoT.ListTopicRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the specific topic.
--
--
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTopicRules
    (
    -- * Creating a Request
      listTopicRules
    , ListTopicRules
    -- * Request Lenses
    , ltrRuleDisabled
    , ltrTopic
    , ltrNextToken
    , ltrMaxResults

    -- * Destructuring the Response
    , listTopicRulesResponse
    , ListTopicRulesResponse
    -- * Response Lenses
    , ltrrsRules
    , ltrrsNextToken
    , ltrrsResponseStatus
    ) where

import Network.AWS.IoT.Types
import Network.AWS.IoT.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the ListTopicRules operation.
--
--
--
-- /See:/ 'listTopicRules' smart constructor.
data ListTopicRules = ListTopicRules'
  { _ltrRuleDisabled :: !(Maybe Bool)
  , _ltrTopic        :: !(Maybe Text)
  , _ltrNextToken    :: !(Maybe Text)
  , _ltrMaxResults   :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTopicRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrRuleDisabled' - Specifies whether the rule is disabled.
--
-- * 'ltrTopic' - The topic.
--
-- * 'ltrNextToken' - A token used to retrieve the next value.
--
-- * 'ltrMaxResults' - The maximum number of results to return.
listTopicRules
    :: ListTopicRules
listTopicRules =
  ListTopicRules'
    { _ltrRuleDisabled = Nothing
    , _ltrTopic = Nothing
    , _ltrNextToken = Nothing
    , _ltrMaxResults = Nothing
    }


-- | Specifies whether the rule is disabled.
ltrRuleDisabled :: Lens' ListTopicRules (Maybe Bool)
ltrRuleDisabled = lens _ltrRuleDisabled (\ s a -> s{_ltrRuleDisabled = a})

-- | The topic.
ltrTopic :: Lens' ListTopicRules (Maybe Text)
ltrTopic = lens _ltrTopic (\ s a -> s{_ltrTopic = a})

-- | A token used to retrieve the next value.
ltrNextToken :: Lens' ListTopicRules (Maybe Text)
ltrNextToken = lens _ltrNextToken (\ s a -> s{_ltrNextToken = a})

-- | The maximum number of results to return.
ltrMaxResults :: Lens' ListTopicRules (Maybe Natural)
ltrMaxResults = lens _ltrMaxResults (\ s a -> s{_ltrMaxResults = a}) . mapping _Nat

instance AWSPager ListTopicRules where
        page rq rs
          | stop (rs ^. ltrrsNextToken) = Nothing
          | stop (rs ^. ltrrsRules) = Nothing
          | otherwise =
            Just $ rq & ltrNextToken .~ rs ^. ltrrsNextToken

instance AWSRequest ListTopicRules where
        type Rs ListTopicRules = ListTopicRulesResponse
        request = get ioT
        response
          = receiveJSON
              (\ s h x ->
                 ListTopicRulesResponse' <$>
                   (x .?> "rules" .!@ mempty) <*> (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListTopicRules where

instance NFData ListTopicRules where

instance ToHeaders ListTopicRules where
        toHeaders = const mempty

instance ToPath ListTopicRules where
        toPath = const "/rules"

instance ToQuery ListTopicRules where
        toQuery ListTopicRules'{..}
          = mconcat
              ["ruleDisabled" =: _ltrRuleDisabled,
               "topic" =: _ltrTopic, "nextToken" =: _ltrNextToken,
               "maxResults" =: _ltrMaxResults]

-- | The output from the ListTopicRules operation.
--
--
--
-- /See:/ 'listTopicRulesResponse' smart constructor.
data ListTopicRulesResponse = ListTopicRulesResponse'
  { _ltrrsRules          :: !(Maybe [TopicRuleListItem])
  , _ltrrsNextToken      :: !(Maybe Text)
  , _ltrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTopicRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltrrsRules' - The rules.
--
-- * 'ltrrsNextToken' - A token used to retrieve the next value.
--
-- * 'ltrrsResponseStatus' - -- | The response status code.
listTopicRulesResponse
    :: Int -- ^ 'ltrrsResponseStatus'
    -> ListTopicRulesResponse
listTopicRulesResponse pResponseStatus_ =
  ListTopicRulesResponse'
    { _ltrrsRules = Nothing
    , _ltrrsNextToken = Nothing
    , _ltrrsResponseStatus = pResponseStatus_
    }


-- | The rules.
ltrrsRules :: Lens' ListTopicRulesResponse [TopicRuleListItem]
ltrrsRules = lens _ltrrsRules (\ s a -> s{_ltrrsRules = a}) . _Default . _Coerce

-- | A token used to retrieve the next value.
ltrrsNextToken :: Lens' ListTopicRulesResponse (Maybe Text)
ltrrsNextToken = lens _ltrrsNextToken (\ s a -> s{_ltrrsNextToken = a})

-- | -- | The response status code.
ltrrsResponseStatus :: Lens' ListTopicRulesResponse Int
ltrrsResponseStatus = lens _ltrrsResponseStatus (\ s a -> s{_ltrrsResponseStatus = a})

instance NFData ListTopicRulesResponse where
