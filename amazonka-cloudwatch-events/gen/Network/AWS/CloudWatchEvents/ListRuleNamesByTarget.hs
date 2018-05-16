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
-- Module      : Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the rules for the specified target. You can see which of the rules in Amazon CloudWatch Events can invoke a specific target in your account.
--
--
module Network.AWS.CloudWatchEvents.ListRuleNamesByTarget
    (
    -- * Creating a Request
      listRuleNamesByTarget
    , ListRuleNamesByTarget
    -- * Request Lenses
    , lrnbtNextToken
    , lrnbtLimit
    , lrnbtTargetARN

    -- * Destructuring the Response
    , listRuleNamesByTargetResponse
    , ListRuleNamesByTargetResponse
    -- * Response Lenses
    , lrnbtrsRuleNames
    , lrnbtrsNextToken
    , lrnbtrsResponseStatus
    ) where

import Network.AWS.CloudWatchEvents.Types
import Network.AWS.CloudWatchEvents.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'listRuleNamesByTarget' smart constructor.
data ListRuleNamesByTarget = ListRuleNamesByTarget'
  { _lrnbtNextToken :: !(Maybe Text)
  , _lrnbtLimit     :: !(Maybe Nat)
  , _lrnbtTargetARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRuleNamesByTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrnbtNextToken' - The token returned by a previous call to retrieve the next set of results.
--
-- * 'lrnbtLimit' - The maximum number of results to return.
--
-- * 'lrnbtTargetARN' - The Amazon Resource Name (ARN) of the target resource.
listRuleNamesByTarget
    :: Text -- ^ 'lrnbtTargetARN'
    -> ListRuleNamesByTarget
listRuleNamesByTarget pTargetARN_ =
  ListRuleNamesByTarget'
    { _lrnbtNextToken = Nothing
    , _lrnbtLimit = Nothing
    , _lrnbtTargetARN = pTargetARN_
    }


-- | The token returned by a previous call to retrieve the next set of results.
lrnbtNextToken :: Lens' ListRuleNamesByTarget (Maybe Text)
lrnbtNextToken = lens _lrnbtNextToken (\ s a -> s{_lrnbtNextToken = a})

-- | The maximum number of results to return.
lrnbtLimit :: Lens' ListRuleNamesByTarget (Maybe Natural)
lrnbtLimit = lens _lrnbtLimit (\ s a -> s{_lrnbtLimit = a}) . mapping _Nat

-- | The Amazon Resource Name (ARN) of the target resource.
lrnbtTargetARN :: Lens' ListRuleNamesByTarget Text
lrnbtTargetARN = lens _lrnbtTargetARN (\ s a -> s{_lrnbtTargetARN = a})

instance AWSRequest ListRuleNamesByTarget where
        type Rs ListRuleNamesByTarget =
             ListRuleNamesByTargetResponse
        request = postJSON cloudWatchEvents
        response
          = receiveJSON
              (\ s h x ->
                 ListRuleNamesByTargetResponse' <$>
                   (x .?> "RuleNames" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListRuleNamesByTarget where

instance NFData ListRuleNamesByTarget where

instance ToHeaders ListRuleNamesByTarget where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSEvents.ListRuleNamesByTarget" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListRuleNamesByTarget where
        toJSON ListRuleNamesByTarget'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _lrnbtNextToken,
                  ("Limit" .=) <$> _lrnbtLimit,
                  Just ("TargetArn" .= _lrnbtTargetARN)])

instance ToPath ListRuleNamesByTarget where
        toPath = const "/"

instance ToQuery ListRuleNamesByTarget where
        toQuery = const mempty

-- | /See:/ 'listRuleNamesByTargetResponse' smart constructor.
data ListRuleNamesByTargetResponse = ListRuleNamesByTargetResponse'
  { _lrnbtrsRuleNames      :: !(Maybe [Text])
  , _lrnbtrsNextToken      :: !(Maybe Text)
  , _lrnbtrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListRuleNamesByTargetResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrnbtrsRuleNames' - The names of the rules that can invoke the given target.
--
-- * 'lrnbtrsNextToken' - Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
--
-- * 'lrnbtrsResponseStatus' - -- | The response status code.
listRuleNamesByTargetResponse
    :: Int -- ^ 'lrnbtrsResponseStatus'
    -> ListRuleNamesByTargetResponse
listRuleNamesByTargetResponse pResponseStatus_ =
  ListRuleNamesByTargetResponse'
    { _lrnbtrsRuleNames = Nothing
    , _lrnbtrsNextToken = Nothing
    , _lrnbtrsResponseStatus = pResponseStatus_
    }


-- | The names of the rules that can invoke the given target.
lrnbtrsRuleNames :: Lens' ListRuleNamesByTargetResponse [Text]
lrnbtrsRuleNames = lens _lrnbtrsRuleNames (\ s a -> s{_lrnbtrsRuleNames = a}) . _Default . _Coerce

-- | Indicates whether there are additional results to retrieve. If there are no more results, the value is null.
lrnbtrsNextToken :: Lens' ListRuleNamesByTargetResponse (Maybe Text)
lrnbtrsNextToken = lens _lrnbtrsNextToken (\ s a -> s{_lrnbtrsNextToken = a})

-- | -- | The response status code.
lrnbtrsResponseStatus :: Lens' ListRuleNamesByTargetResponse Int
lrnbtrsResponseStatus = lens _lrnbtrsResponseStatus (\ s a -> s{_lrnbtrsResponseStatus = a})

instance NFData ListRuleNamesByTargetResponse where
