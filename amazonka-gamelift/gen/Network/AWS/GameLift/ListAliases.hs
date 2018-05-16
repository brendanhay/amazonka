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
-- Module      : Network.AWS.GameLift.ListAliases
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves all aliases for this AWS account. You can filter the result set by alias name and/or routing strategy type. Use the pagination parameters to retrieve results in sequential pages.
--
--
-- Alias-related operations include:
--
--     * 'CreateAlias'
--
--     * 'ListAliases'
--
--     * 'DescribeAlias'
--
--     * 'UpdateAlias'
--
--     * 'DeleteAlias'
--
--     * 'ResolveAlias'
--
--
--
module Network.AWS.GameLift.ListAliases
    (
    -- * Creating a Request
      listAliases
    , ListAliases
    -- * Request Lenses
    , laRoutingStrategyType
    , laNextToken
    , laName
    , laLimit

    -- * Destructuring the Response
    , listAliasesResponse
    , ListAliasesResponse
    -- * Response Lenses
    , larsAliases
    , larsNextToken
    , larsResponseStatus
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
-- /See:/ 'listAliases' smart constructor.
data ListAliases = ListAliases'
  { _laRoutingStrategyType :: !(Maybe RoutingStrategyType)
  , _laNextToken           :: !(Maybe Text)
  , _laName                :: !(Maybe Text)
  , _laLimit               :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAliases' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laRoutingStrategyType' - Type of routing to filter results on. Use this parameter to retrieve only aliases of a certain type. To retrieve all aliases, leave this parameter empty. Possible routing types include the following:     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
--
-- * 'laNextToken' - Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
--
-- * 'laName' - Descriptive label that is associated with an alias. Alias names do not need to be unique.
--
-- * 'laLimit' - Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
listAliases
    :: ListAliases
listAliases =
  ListAliases'
    { _laRoutingStrategyType = Nothing
    , _laNextToken = Nothing
    , _laName = Nothing
    , _laLimit = Nothing
    }


-- | Type of routing to filter results on. Use this parameter to retrieve only aliases of a certain type. To retrieve all aliases, leave this parameter empty. Possible routing types include the following:     * __SIMPLE__ -- The alias resolves to one specific fleet. Use this type when routing to active fleets.     * __TERMINAL__ -- The alias does not resolve to a fleet but instead can be used to display a message to the user. A terminal alias throws a TerminalRoutingStrategyException with the 'RoutingStrategy' message embedded.
laRoutingStrategyType :: Lens' ListAliases (Maybe RoutingStrategyType)
laRoutingStrategyType = lens _laRoutingStrategyType (\ s a -> s{_laRoutingStrategyType = a})

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this action. To start at the beginning of the result set, do not specify a value.
laNextToken :: Lens' ListAliases (Maybe Text)
laNextToken = lens _laNextToken (\ s a -> s{_laNextToken = a})

-- | Descriptive label that is associated with an alias. Alias names do not need to be unique.
laName :: Lens' ListAliases (Maybe Text)
laName = lens _laName (\ s a -> s{_laName = a})

-- | Maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
laLimit :: Lens' ListAliases (Maybe Natural)
laLimit = lens _laLimit (\ s a -> s{_laLimit = a}) . mapping _Nat

instance AWSRequest ListAliases where
        type Rs ListAliases = ListAliasesResponse
        request = postJSON gameLift
        response
          = receiveJSON
              (\ s h x ->
                 ListAliasesResponse' <$>
                   (x .?> "Aliases" .!@ mempty) <*> (x .?> "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable ListAliases where

instance NFData ListAliases where

instance ToHeaders ListAliases where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("GameLift.ListAliases" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListAliases where
        toJSON ListAliases'{..}
          = object
              (catMaybes
                 [("RoutingStrategyType" .=) <$>
                    _laRoutingStrategyType,
                  ("NextToken" .=) <$> _laNextToken,
                  ("Name" .=) <$> _laName, ("Limit" .=) <$> _laLimit])

instance ToPath ListAliases where
        toPath = const "/"

instance ToQuery ListAliases where
        toQuery = const mempty

-- | Represents the returned data in response to a request action.
--
--
--
-- /See:/ 'listAliasesResponse' smart constructor.
data ListAliasesResponse = ListAliasesResponse'
  { _larsAliases        :: !(Maybe [Alias])
  , _larsNextToken      :: !(Maybe Text)
  , _larsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAliasesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'larsAliases' - Collection of alias records that match the list request.
--
-- * 'larsNextToken' - Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
--
-- * 'larsResponseStatus' - -- | The response status code.
listAliasesResponse
    :: Int -- ^ 'larsResponseStatus'
    -> ListAliasesResponse
listAliasesResponse pResponseStatus_ =
  ListAliasesResponse'
    { _larsAliases = Nothing
    , _larsNextToken = Nothing
    , _larsResponseStatus = pResponseStatus_
    }


-- | Collection of alias records that match the list request.
larsAliases :: Lens' ListAliasesResponse [Alias]
larsAliases = lens _larsAliases (\ s a -> s{_larsAliases = a}) . _Default . _Coerce

-- | Token that indicates where to resume retrieving results on the next call to this action. If no token is returned, these results represent the end of the list.
larsNextToken :: Lens' ListAliasesResponse (Maybe Text)
larsNextToken = lens _larsNextToken (\ s a -> s{_larsNextToken = a})

-- | -- | The response status code.
larsResponseStatus :: Lens' ListAliasesResponse Int
larsResponseStatus = lens _larsResponseStatus (\ s a -> s{_larsResponseStatus = a})

instance NFData ListAliasesResponse where
