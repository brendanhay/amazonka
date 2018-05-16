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
-- Module      : Network.AWS.CloudWatchEvents.ListRules
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists your Amazon CloudWatch Events rules. You can either list all the rules or you can provide a prefix to match to the rule names.
--
--
module Network.AWS.CloudWatchEvents.ListRules
    (
    -- * Creating a Request
      listRules
    , ListRules
    -- * Request Lenses
    , lrNextToken
    , lrNamePrefix
    , lrLimit

    -- * Destructuring the Response
    , listRulesResponse
    , ListRulesResponse
    -- * Response Lenses
    , lrrsRules
    , lrrsNextToken
    , lrrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRules' smart constructor.
data ListRules = ListRules'
  { _lrNextToken  :: !(Maybe Text)
  , _lrNamePrefix :: !(Maybe Text)
  , _lrLimit      :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'lrNamePrefix' - The prefix matching the rule name.
--
-- * 'lrLimit' - The maximum number of results to return.
listRules
    :: ListRules
listRules =
  ListRules'
    {_lrNextToken = Nothing, _lrNamePrefix = Nothing, _lrLimit = Nothing}


-- | The token returned by a previous call to retrieve the next set of results.
lrNextToken :: Lens' ListRules (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a})

-- | The prefix matching the rule name.
lrNamePrefix :: Lens' ListRules (Maybe Text)
lrNamePrefix = lens _lrNamePrefix (\ s a -> s{_lrNamePrefix = a})

-- | The maximum number of results to return.
lrLimit :: Lens' ListRules (Maybe Natural)
lrLimit = lens _lrLimit (\ s a -> s{_lrLimit = a}) . mapping _Nat

instance AWSRequest ListRules where
        type Rs ListRules = ListRulesResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListRulesResponse' <$>
                   (x .?> "Rules" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRules where

instance NFData ListRules where

instance ToHeaders ListRules where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ListRules" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRules where
        toJSON ListRules'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lrNextToken,
                  ("NamePrefix" .=) <$> _lrNamePrefix,
                  ("Limit" .=) <$> _lrLimit])

instance ToPath ListRules where
        toPath = const "/"

instance ToQuery ListRules where
        toQuery = const mempty

-- | /See:/ 'listRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
  { _lrrsRules          :: !(Maybe [Rule])
  , _lrrsNextToken      :: !(Maybe Text)
  , _lrrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRules' - The rules that match the specified criteria.
--
-- * 'lrrsNextToken' - Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- * 'lrrsResponseStatus' - -- | The response status code.
listRulesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRulesResponse
listRulesResponse pResponseStatus_ =
  ListRulesResponse'
    { _lrrsRules = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }


-- | The rules that match the specified criteria.
lrrsRules :: Lens' ListRulesResponse [Rule]
lrrsRules = lens _lrrsRules (\ s a -> s{_lrrsRules = a}) . _Default . _Coerce

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
lrrsNextToken :: Lens' ListRulesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a})

-- | -- | The response status code.
lrrsResponseStatus :: Lens' ListRulesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a})

instance NFData ListRulesResponse where
