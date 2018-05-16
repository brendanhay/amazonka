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
-- Module      : Network.AWS.SSM.ListCommandInvocations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An invocation is copy of a command sent to a specific instance. A command can apply to one or more instances. A command invocation applies to one instance. For example, if a user executes SendCommand against three instances, then a command invocation is created for each requested instance ID. ListCommandInvocations provide status about command execution.
--
--
--
-- This operation returns paginated results.
module Network.AWS.SSM.ListCommandInvocations
    (
    -- * Creating a Request
      listCommandInvocations
    , ListCommandInvocations
    -- * Request Lenses
    , lciInstanceId
    , lciFilters
    , lciNextToken
    , lciCommandId
    , lciDetails
    , lciMaxResults

    -- * Destructuring the Response
    , listCommandInvocationsResponse
    , ListCommandInvocationsResponse
    -- * Response Lenses
    , lrsNextToken
    , lrsCommandInvocations
    , lrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'listCommandInvocations' smart constructor.
data ListCommandInvocations = ListCommandInvocations'
  { _lciInstanceId :: !(Maybe Text)
  , _lciFilters    :: !(Maybe (List1 CommandFilter))
  , _lciNextToken  :: !(Maybe Text)
  , _lciCommandId  :: !(Maybe Text)
  , _lciDetails    :: !(Maybe Bool)
  , _lciMaxResults :: !(Maybe Nat)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCommandInvocations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lciInstanceId' - (Optional) The command execution details for a specific instance ID.
--
-- * 'lciFilters' - (Optional) One or more filters. Use a filter to return a more specific list of results.
--
-- * 'lciNextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'lciCommandId' - (Optional) The invocations for a specific command ID.
--
-- * 'lciDetails' - (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
--
-- * 'lciMaxResults' - (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
listCommandInvocations
    :: ListCommandInvocations
listCommandInvocations =
  ListCommandInvocations'
    { _lciInstanceId = Nothing
    , _lciFilters = Nothing
    , _lciNextToken = Nothing
    , _lciCommandId = Nothing
    , _lciDetails = Nothing
    , _lciMaxResults = Nothing
    }


-- | (Optional) The command execution details for a specific instance ID.
lciInstanceId :: Lens' ListCommandInvocations (Maybe Text)
lciInstanceId = lens _lciInstanceId (\ s a -> s{_lciInstanceId = a})

-- | (Optional) One or more filters. Use a filter to return a more specific list of results.
lciFilters :: Lens' ListCommandInvocations (Maybe (NonEmpty CommandFilter))
lciFilters = lens _lciFilters (\ s a -> s{_lciFilters = a}) . mapping _List1

-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
lciNextToken :: Lens' ListCommandInvocations (Maybe Text)
lciNextToken = lens _lciNextToken (\ s a -> s{_lciNextToken = a})

-- | (Optional) The invocations for a specific command ID.
lciCommandId :: Lens' ListCommandInvocations (Maybe Text)
lciCommandId = lens _lciCommandId (\ s a -> s{_lciCommandId = a})

-- | (Optional) If set this returns the response of the command executions and any command output. By default this is set to False.
lciDetails :: Lens' ListCommandInvocations (Maybe Bool)
lciDetails = lens _lciDetails (\ s a -> s{_lciDetails = a})

-- | (Optional) The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
lciMaxResults :: Lens' ListCommandInvocations (Maybe Natural)
lciMaxResults = lens _lciMaxResults (\ s a -> s{_lciMaxResults = a}) . mapping _Nat

instance AWSPager ListCommandInvocations where
        page rq rs
          | stop (rs ^. lrsNextToken) = Nothing
          | stop (rs ^. lrsCommandInvocations) = Nothing
          | otherwise =
            Just $ rq & lciNextToken .~ rs ^. lrsNextToken

instance AWSRequest ListCommandInvocations where
        type Rs ListCommandInvocations =
             ListCommandInvocationsResponse
        request = postJSON ssm
        response
          = receiveJSON
              (\ s h x ->
                 ListCommandInvocationsResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "CommandInvocations" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListCommandInvocations where

instance NFData ListCommandInvocations where

instance ToHeaders ListCommandInvocations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.ListCommandInvocations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListCommandInvocations where
        toJSON ListCommandInvocations'{..}
          = object
              (catMaybes
                 [("InstanceId" .=) <$> _lciInstanceId,
                  ("Filters" .=) <$> _lciFilters,
                  ("NextToken" .=) <$> _lciNextToken,
                  ("CommandId" .=) <$> _lciCommandId,
                  ("Details" .=) <$> _lciDetails,
                  ("MaxResults" .=) <$> _lciMaxResults])

instance ToPath ListCommandInvocations where
        toPath = const "/"

instance ToQuery ListCommandInvocations where
        toQuery = const mempty

-- | /See:/ 'listCommandInvocationsResponse' smart constructor.
data ListCommandInvocationsResponse = ListCommandInvocationsResponse'
  { _lrsNextToken          :: !(Maybe Text)
  , _lrsCommandInvocations :: !(Maybe [CommandInvocation])
  , _lrsResponseStatus     :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListCommandInvocationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrsNextToken' - (Optional) The token for the next set of items to return. (You received this token from a previous call.)
--
-- * 'lrsCommandInvocations' - (Optional) A list of all invocations.
--
-- * 'lrsResponseStatus' - -- | The response status code.
listCommandInvocationsResponse
    :: Int -- ^ 'lrsResponseStatus'
    -> ListCommandInvocationsResponse
listCommandInvocationsResponse pResponseStatus_ =
  ListCommandInvocationsResponse'
    { _lrsNextToken = Nothing
    , _lrsCommandInvocations = Nothing
    , _lrsResponseStatus = pResponseStatus_
    }


-- | (Optional) The token for the next set of items to return. (You received this token from a previous call.)
lrsNextToken :: Lens' ListCommandInvocationsResponse (Maybe Text)
lrsNextToken = lens _lrsNextToken (\ s a -> s{_lrsNextToken = a})

-- | (Optional) A list of all invocations.
lrsCommandInvocations :: Lens' ListCommandInvocationsResponse [CommandInvocation]
lrsCommandInvocations = lens _lrsCommandInvocations (\ s a -> s{_lrsCommandInvocations = a}) . _Default . _Coerce

-- | -- | The response status code.
lrsResponseStatus :: Lens' ListCommandInvocationsResponse Int
lrsResponseStatus = lens _lrsResponseStatus (\ s a -> s{_lrsResponseStatus = a})

instance NFData ListCommandInvocationsResponse where
