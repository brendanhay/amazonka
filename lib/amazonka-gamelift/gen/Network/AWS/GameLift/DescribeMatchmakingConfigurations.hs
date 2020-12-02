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
-- Module      : Network.AWS.GameLift.DescribeMatchmakingConfigurations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of FlexMatch matchmaking configurations. with this operation, you have the following options: (1) retrieve all existing configurations, (2) provide the names of one or more configurations to retrieve, or (3) retrieve all configurations that use a specified rule set name. When requesting multiple items, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a configuration is returned for each requested name. When specifying a list of names, only configurations that currently exist are returned.
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
module Network.AWS.GameLift.DescribeMatchmakingConfigurations
    (
    -- * Creating a Request
      describeMatchmakingConfigurations
    , DescribeMatchmakingConfigurations
    -- * Request Lenses
    , dmcRuleSetName
    , dmcNextToken
    , dmcNames
    , dmcLimit

    -- * Destructuring the Response
    , describeMatchmakingConfigurationsResponse
    , DescribeMatchmakingConfigurationsResponse
    -- * Response Lenses
    , dmcsrsConfigurations
    , dmcsrsNextToken
    , dmcsrsResponseStatus
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
-- /See:/ 'describeMatchmakingConfigurations' smart constructor.
data DescribeMatchmakingConfigurations = DescribeMatchmakingConfigurations'
  { _dmcRuleSetName :: !(Maybe Text)
  , _dmcNextToken   :: !(Maybe Text)
  , _dmcNames       :: !(Maybe [Text])
  , _dmcLimit       :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMatchmakingConfigurations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcRuleSetName' - Unique identifier for a matchmaking rule set. Use this parameter to retrieve all matchmaking configurations that use this rule set.
--
-- * 'dmcNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'dmcNames' - Unique identifier for a matchmaking configuration(s) to retrieve. To request all existing configurations, leave this parameter empty.
--
-- * 'dmcLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
describeMatchmakingConfigurations
    :: DescribeMatchmakingConfigurations
describeMatchmakingConfigurations =
  DescribeMatchmakingConfigurations'
    { _dmcRuleSetName = Nothing
    , _dmcNextToken = Nothing
    , _dmcNames = Nothing
    , _dmcLimit = Nothing
    }


-- | Unique identifier for a matchmaking rule set. Use this parameter to retrieve all matchmaking configurations that use this rule set.
dmcRuleSetName :: Lens' DescribeMatchmakingConfigurations (Maybe Text)
dmcRuleSetName = lens _dmcRuleSetName (\ s a -> s{_dmcRuleSetName = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
dmcNextToken :: Lens' DescribeMatchmakingConfigurations (Maybe Text)
dmcNextToken = lens _dmcNextToken (\ s a -> s{_dmcNextToken = a})

-- | Unique identifier for a matchmaking configuration(s) to retrieve. To request all existing configurations, leave this parameter empty.
dmcNames :: Lens' DescribeMatchmakingConfigurations [Text]
dmcNames = lens _dmcNames (\ s a -> s{_dmcNames = a}) . _Default . _Coerce

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is limited to 10.
dmcLimit :: Lens' DescribeMatchmakingConfigurations (Maybe Natural)
dmcLimit = lens _dmcLimit (\ s a -> s{_dmcLimit = a}) . mapping _Nat

instance AWSRequest DescribeMatchmakingConfigurations
         where
        type Rs DescribeMatchmakingConfigurations =
             DescribeMatchmakingConfigurationsResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 DescribeMatchmakingConfigurationsResponse' <$>
                   (x .?> "Configurations" .!@ mempty) <*>
                     (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeMatchmakingConfigurations
         where

instance NFData DescribeMatchmakingConfigurations
         where

instance ToHeaders DescribeMatchmakingConfigurations
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.DescribeMatchmakingConfigurations" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeMatchmakingConfigurations
         where
        toJSON DescribeMatchmakingConfigurations'{..}
          = object
              (catMaybes
                 [("RuleSetName" .=) <$> _dmcRuleSetName,
                  ("NextToken" .=) <$> _dmcNextToken,
                  ("Names" .=) <$> _dmcNames,
                  ("Limit" .=) <$> _dmcLimit])

instance ToPath DescribeMatchmakingConfigurations
         where
        toPath = const "/"

instance ToQuery DescribeMatchmakingConfigurations
         where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'describeMatchmakingConfigurationsResponse' smart constructor.
data DescribeMatchmakingConfigurationsResponse = DescribeMatchmakingConfigurationsResponse'
  { _dmcsrsConfigurations :: !(Maybe [MatchmakingConfiguration])
  , _dmcsrsNextToken      :: !(Maybe Text)
  , _dmcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeMatchmakingConfigurationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmcsrsConfigurations' - Collection of requested matchmaking configuration objects.
--
-- * 'dmcsrsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'dmcsrsResponseStatus' - -- | The response status code.
describeMatchmakingConfigurationsResponse
    :: Int -- ^ 'dmcsrsResponseStatus'
    -> DescribeMatchmakingConfigurationsResponse
describeMatchmakingConfigurationsResponse pResponseStatus_ =
  DescribeMatchmakingConfigurationsResponse'
    { _dmcsrsConfigurations = Nothing
    , _dmcsrsNextToken = Nothing
    , _dmcsrsResponseStatus = pResponseStatus_
    }


-- | Collection of requested matchmaking configuration objects.
dmcsrsConfigurations :: Lens' DescribeMatchmakingConfigurationsResponse [MatchmakingConfiguration]
dmcsrsConfigurations = lens _dmcsrsConfigurations (\ s a -> s{_dmcsrsConfigurations = a}) . _Default . _Coerce

-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
dmcsrsNextToken :: Lens' DescribeMatchmakingConfigurationsResponse (Maybe Text)
dmcsrsNextToken = lens _dmcsrsNextToken (\ s a -> s{_dmcsrsNextToken = a})

-- | -- | The response status code.
dmcsrsResponseStatus :: Lens' DescribeMatchmakingConfigurationsResponse Int
dmcsrsResponseStatus = lens _dmcsrsResponseStatus (\ s a -> s{_dmcsrsResponseStatus = a})

instance NFData
           DescribeMatchmakingConfigurationsResponse
         where
