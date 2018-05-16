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
-- Module      : Network.AWS.GameLift.DescribeMatchmakingRuleSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details for FlexMatch matchmaking rule sets. You can request all existing rule sets for the region, or provide a list of one or more rule set names. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a rule set is returned for each requested name.
--
--
-- Operations related to match configurations and rule sets include:
--
--     * 'CreateMatchmakingConfiguration'
--
--     * 'DescribeMatchmakingConfigurations'
--
--     * 'UpdateMatchmakingConfiguration'
--
--     * 'DeleteMatchmakingConfiguration'
--
--     * 'CreateMatchmakingRuleSet'
--
--     * 'DescribeMatchmakingRuleSets'
--
--     * 'ValidateMatchmakingRuleSet'
--
--
--
module Network.AWS.GameLift.DescribeMatchmakingRuleSets
    (
    -- * Creating a Request
      describeMatchmakingRuleSets
    , DescribeMatchmakingRuleSets
    -- * Request Lenses
    , dmrsNextToken
    , dmrsNames
    , dmrsLimit

    -- * Destructuring the Response
    , describeMatchmakingRuleSetsResponse
    , DescribeMatchmakingRuleSetsResponse
    -- * Response Lenses
    , dmrsrsNextToken
    , dmrsrsResponseStatus
    , dmrsrsRuleSets
    ) where

import Network.AWS.GameLift.Types
import Network.AWS.GameLift.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a request action.
--
--
--
-- /See:/ 'describeMatchmakingRuleSets' smart constructor.
data DescribeMatchmakingRuleSets = DescribeMatchmakingRuleSets'
  { _dmrsNextToken :: !(Maybe Text)
  , _dmrsNames     :: !(Maybe (List1 Text))
  , _dmrsLimit     :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMatchmakingRuleSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'dmrsNames' - Unique identifier for a matchmaking rule set. This name is used to identify the rule set associated with a matchmaking configuration.
--
-- * 'dmrsLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
describeMatchmakingRuleSets
    :: DescribeMatchmakingRuleSets
describeMatchmakingRuleSets =
  DescribeMatchmakingRuleSets'
    {_dmrsNextToken = Nothing, _dmrsNames = Nothing, _dmrsLimit = Nothing}


-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
dmrsNextToken :: Lens' DescribeMatchmakingRuleSets (Maybe Text)
dmrsNextToken = lens _dmrsNextToken (\ s a -> s{_dmrsNextToken = a})

-- | Unique identifier for a matchmaking rule set. This name is used to identify the rule set associated with a matchmaking configuration.
dmrsNames :: Lens' DescribeMatchmakingRuleSets (Maybe (NonEmpty Text))
dmrsNames = lens _dmrsNames (\ s a -> s{_dmrsNames = a}) . mapping _List1

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
dmrsLimit :: Lens' DescribeMatchmakingRuleSets (Maybe Natural)
dmrsLimit = lens _dmrsLimit (\ s a -> s{_dmrsLimit = a}) . mapping _Nat

instance AWSRequest DescribeMatchmakingRuleSets where
        type Rs DescribeMatchmakingRuleSets =
             DescribeMatchmakingRuleSetsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMatchmakingRuleSetsResponse' <$>
                   (x .?> "NextToken") <*> (pure (fromEnum s)) <*>
                     (x .?> "RuleSets" .!@ mempty))

instance Hashable DescribeMatchmakingRuleSets where

instance NFData DescribeMatchmakingRuleSets where

instance ToHeaders DescribeMatchmakingRuleSets where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeMatchmakingRuleSets" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMatchmakingRuleSets where
        toJSON DescribeMatchmakingRuleSets'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _dmrsNextToken,
                  ("Names" .=) <$> _dmrsNames,
                  ("Limit" .=) <$> _dmrsLimit])

instance ToPath DescribeMatchmakingRuleSets where
        toPath = const "/"

instance ToQuery DescribeMatchmakingRuleSets where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeMatchmakingRuleSetsResponse' smart constructor.
data DescribeMatchmakingRuleSetsResponse = DescribeMatchmakingRuleSetsResponse'
  { _dmrsrsNextToken      :: !(Maybe Text)
  , _dmrsrsResponseStatus :: !Int
  , _dmrsrsRuleSets       :: ![MatchmakingRuleSet]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMatchmakingRuleSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dmrsrsResponseStatus' - -- | The response status code.
--
-- * 'dmrsrsRuleSets' - Collection of requested matchmaking rule set objects.
describeMatchmakingRuleSetsResponse
    :: Int -- ^ 'dmrsrsResponseStatus'
    -> DescribeMatchmakingRuleSetsResponse
describeMatchmakingRuleSetsResponse pResponseStatus_ =
  DescribeMatchmakingRuleSetsResponse'
    { _dmrsrsNextToken = Nothing
    , _dmrsrsResponseStatus = pResponseStatus_
    , _dmrsrsRuleSets = mempty
    }


-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dmrsrsNextToken :: Lens' DescribeMatchmakingRuleSetsResponse (Maybe Text)
dmrsrsNextToken = lens _dmrsrsNextToken (\ s a -> s{_dmrsrsNextToken = a})

-- | -- | The response status code.
dmrsrsResponseStatus :: Lens' DescribeMatchmakingRuleSetsResponse Int
dmrsrsResponseStatus = lens _dmrsrsResponseStatus (\ s a -> s{_dmrsrsResponseStatus = a})

-- | Collection of requested matchmaking rule set objects.
dmrsrsRuleSets :: Lens' DescribeMatchmakingRuleSetsResponse [MatchmakingRuleSet]
dmrsrsRuleSets = lens _dmrsrsRuleSets (\ s a -> s{_dmrsrsRuleSets = a}) . _Coerce

instance NFData DescribeMatchmakingRuleSetsResponse
         where
