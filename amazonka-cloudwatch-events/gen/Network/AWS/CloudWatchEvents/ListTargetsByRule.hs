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
-- Module      : Network.AWS.CloudWatchEvents.ListTargetsByRule
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the targets assigned to the specified rule.
--
--
module Network.AWS.CloudWatchEvents.ListTargetsByRule
    (
    -- * Creating a Request
      listTargetsByRule
    , ListTargetsByRule
    -- * Request Lenses
    , ltbrNextToken
    , ltbrLimit
    , ltbrRule

    -- * Destructuring the Response
    , listTargetsByRuleResponse
    , ListTargetsByRuleResponse
    -- * Response Lenses
    , ltbrrsNextToken
    , ltbrrsTargets
    , ltbrrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listTargetsByRule' smart constructor.
data ListTargetsByRule = ListTargetsByRule'
  { _ltbrNextToken :: !(Maybe Text)
  , _ltbrLimit     :: !(Maybe Nat)
  , _ltbrRule      :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTargetsByRule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltbrNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'ltbrLimit' - The maximum number of results to return.
--
-- * 'ltbrRule' - The name of the rule.
listTargetsByRule
    :: Text -- ^ 'ltbrRule'
    -> ListTargetsByRule
listTargetsByRule pRule_ =
  ListTargetsByRule'
    {_ltbrNextToken = Nothing, _ltbrLimit = Nothing, _ltbrRule = pRule_}


-- | The token returned by a previous call to retrieve the next set of results.
ltbrNextToken :: Lens' ListTargetsByRule (Maybe Text)
ltbrNextToken = lens _ltbrNextToken (\ s a -> s{_ltbrNextToken = a})

-- | The maximum number of results to return.
ltbrLimit :: Lens' ListTargetsByRule (Maybe Natural)
ltbrLimit = lens _ltbrLimit (\ s a -> s{_ltbrLimit = a}) . mapping _Nat

-- | The name of the rule.
ltbrRule :: Lens' ListTargetsByRule Text
ltbrRule = lens _ltbrRule (\ s a -> s{_ltbrRule = a})

instance AWSRequest ListTargetsByRule where
        type Rs ListTargetsByRule = ListTargetsByRuleResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListTargetsByRuleResponse' <$>
                   (x .?> "NextToken") <*> (x .?> "Targets") <*>
                     (pure (fromEnum s)))

instance Hashable ListTargetsByRule where

instance NFData ListTargetsByRule where

instance ToHeaders ListTargetsByRule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ListTargetsByRule" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListTargetsByRule where
        toJSON ListTargetsByRule'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ltbrNextToken,
                  ("Limit" .=) <$> _ltbrLimit,
                  Just ("Rule" .= _ltbrRule)])

instance ToPath ListTargetsByRule where
        toPath = const "/"

instance ToQuery ListTargetsByRule where
        toQuery = const mempty

-- | /See:/ 'listTargetsByRuleResponse' smart constructor.
data ListTargetsByRuleResponse = ListTargetsByRuleResponse'
  { _ltbrrsNextToken      :: !(Maybe Text)
  , _ltbrrsTargets        :: !(Maybe (List1 Target))
  , _ltbrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListTargetsByRuleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltbrrsNextToken' - Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- * 'ltbrrsTargets' - The targets assigned to the rule.
--
-- * 'ltbrrsResponseStatus' - -- | The response status code.
listTargetsByRuleResponse
    :: Int -- ^ 'ltbrrsResponseStatus'
    -> ListTargetsByRuleResponse
listTargetsByRuleResponse pResponseStatus_ =
  ListTargetsByRuleResponse'
    { _ltbrrsNextToken = Nothing
    , _ltbrrsTargets = Nothing
    , _ltbrrsResponseStatus = pResponseStatus_
    }


-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
ltbrrsNextToken :: Lens' ListTargetsByRuleResponse (Maybe Text)
ltbrrsNextToken = lens _ltbrrsNextToken (\ s a -> s{_ltbrrsNextToken = a})

-- | The targets assigned to the rule.
ltbrrsTargets :: Lens' ListTargetsByRuleResponse (Maybe (NonEmpty Target))
ltbrrsTargets = lens _ltbrrsTargets (\ s a -> s{_ltbrrsTargets = a}) . mapping _List1

-- | -- | The response status code.
ltbrrsResponseStatus :: Lens' ListTargetsByRuleResponse Int
ltbrrsResponseStatus = lens _ltbrrsResponseStatus (\ s a -> s{_ltbrrsResponseStatus = a})

instance NFData ListTargetsByRuleResponse where
