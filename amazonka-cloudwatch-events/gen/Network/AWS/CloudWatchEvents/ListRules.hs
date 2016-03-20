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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the Amazon CloudWatch Events rules in your account. You can either
-- list all the rules or you can provide a prefix to match to the rule
-- names. If you have more rules in your account than the given limit, the
-- results will be paginated. In that case, use the next token returned in
-- the response and repeat ListRules until the NextToken in the response is
-- returned as null.
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

import           Network.AWS.CloudWatchEvents.Types
import           Network.AWS.CloudWatchEvents.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the < ListRules> operation.
--
-- /See:/ 'listRules' smart constructor.
data ListRules = ListRules'
    { _lrNextToken  :: !(Maybe Text)
    , _lrNamePrefix :: !(Maybe Text)
    , _lrLimit      :: !(Maybe Nat)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRules' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrNextToken'
--
-- * 'lrNamePrefix'
--
-- * 'lrLimit'
listRules
    :: ListRules
listRules =
    ListRules'
    { _lrNextToken = Nothing
    , _lrNamePrefix = Nothing
    , _lrLimit = Nothing
    }

-- | The token returned by a previous call to indicate that there is more
-- data available.
lrNextToken :: Lens' ListRules (Maybe Text)
lrNextToken = lens _lrNextToken (\ s a -> s{_lrNextToken = a});

-- | The prefix matching the rule name.
lrNamePrefix :: Lens' ListRules (Maybe Text)
lrNamePrefix = lens _lrNamePrefix (\ s a -> s{_lrNamePrefix = a});

-- | The maximum number of results to return.
lrLimit :: Lens' ListRules (Maybe Natural)
lrLimit = lens _lrLimit (\ s a -> s{_lrLimit = a}) . mapping _Nat;

instance AWSRequest ListRules where
        type Rs ListRules = ListRulesResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListRulesResponse' <$>
                   (x .?> "Rules" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRules

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

-- | The result of the < ListRules> operation.
--
-- /See:/ 'listRulesResponse' smart constructor.
data ListRulesResponse = ListRulesResponse'
    { _lrrsRules          :: !(Maybe [Rule])
    , _lrrsNextToken      :: !(Maybe Text)
    , _lrrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListRulesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrrsRules'
--
-- * 'lrrsNextToken'
--
-- * 'lrrsResponseStatus'
listRulesResponse
    :: Int -- ^ 'lrrsResponseStatus'
    -> ListRulesResponse
listRulesResponse pResponseStatus_ =
    ListRulesResponse'
    { _lrrsRules = Nothing
    , _lrrsNextToken = Nothing
    , _lrrsResponseStatus = pResponseStatus_
    }

-- | List of rules matching the specified criteria.
lrrsRules :: Lens' ListRulesResponse [Rule]
lrrsRules = lens _lrrsRules (\ s a -> s{_lrrsRules = a}) . _Default . _Coerce;

-- | Indicates that there are additional results to retrieve.
lrrsNextToken :: Lens' ListRulesResponse (Maybe Text)
lrrsNextToken = lens _lrrsNextToken (\ s a -> s{_lrrsNextToken = a});

-- | The response status code.
lrrsResponseStatus :: Lens' ListRulesResponse Int
lrrsResponseStatus = lens _lrrsResponseStatus (\ s a -> s{_lrrsResponseStatus = a});
