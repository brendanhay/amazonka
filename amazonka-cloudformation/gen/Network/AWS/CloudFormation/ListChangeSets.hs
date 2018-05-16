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
-- Module      : Network.AWS.CloudFormation.ListChangeSets
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the ID and status of each active change set for a stack. For example, AWS CloudFormation lists change sets that are in the @CREATE_IN_PROGRESS@ or @CREATE_PENDING@ state.
--
--
module Network.AWS.CloudFormation.ListChangeSets
    (
    -- * Creating a Request
      listChangeSets
    , ListChangeSets
    -- * Request Lenses
    , lcsNextToken
    , lcsStackName

    -- * Destructuring the Response
    , listChangeSetsResponse
    , ListChangeSetsResponse
    -- * Response Lenses
    , lcsrsNextToken
    , lcsrsSummaries
    , lcsrsResponseStatus
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.CloudFormation.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input for the 'ListChangeSets' action.
--
--
--
-- /See:/ 'listChangeSets' smart constructor.
data ListChangeSets = ListChangeSets'
  { _lcsNextToken :: !(Maybe Text)
  , _lcsStackName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChangeSets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsNextToken' - A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
--
-- * 'lcsStackName' - The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
listChangeSets
    :: Text -- ^ 'lcsStackName'
    -> ListChangeSets
listChangeSets pStackName_ =
  ListChangeSets' {_lcsNextToken = Nothing, _lcsStackName = pStackName_}


-- | A string (provided by the 'ListChangeSets' response output) that identifies the next page of change sets that you want to retrieve.
lcsNextToken :: Lens' ListChangeSets (Maybe Text)
lcsNextToken = lens _lcsNextToken (\ s a -> s{_lcsNextToken = a})

-- | The name or the Amazon Resource Name (ARN) of the stack for which you want to list change sets.
lcsStackName :: Lens' ListChangeSets Text
lcsStackName = lens _lcsStackName (\ s a -> s{_lcsStackName = a})

instance AWSRequest ListChangeSets where
        type Rs ListChangeSets = ListChangeSetsResponse
        request = postQuery cloudFormation
        response
          = receiveXMLWrapper "ListChangeSetsResult"
              (\ s h x ->
                 ListChangeSetsResponse' <$>
                   (x .@? "NextToken") <*>
                     (x .@? "Summaries" .!@ mempty >>=
                        may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListChangeSets where

instance NFData ListChangeSets where

instance ToHeaders ListChangeSets where
        toHeaders = const mempty

instance ToPath ListChangeSets where
        toPath = const "/"

instance ToQuery ListChangeSets where
        toQuery ListChangeSets'{..}
          = mconcat
              ["Action" =: ("ListChangeSets" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "NextToken" =: _lcsNextToken,
               "StackName" =: _lcsStackName]

-- | The output for the 'ListChangeSets' action.
--
--
--
-- /See:/ 'listChangeSetsResponse' smart constructor.
data ListChangeSetsResponse = ListChangeSetsResponse'
  { _lcsrsNextToken      :: !(Maybe Text)
  , _lcsrsSummaries      :: !(Maybe [ChangeSetSummary])
  , _lcsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListChangeSetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcsrsNextToken' - If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
--
-- * 'lcsrsSummaries' - A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
--
-- * 'lcsrsResponseStatus' - -- | The response status code.
listChangeSetsResponse
    :: Int -- ^ 'lcsrsResponseStatus'
    -> ListChangeSetsResponse
listChangeSetsResponse pResponseStatus_ =
  ListChangeSetsResponse'
    { _lcsrsNextToken = Nothing
    , _lcsrsSummaries = Nothing
    , _lcsrsResponseStatus = pResponseStatus_
    }


-- | If the output exceeds 1 MB, a string that identifies the next page of change sets. If there is no additional page, this value is null.
lcsrsNextToken :: Lens' ListChangeSetsResponse (Maybe Text)
lcsrsNextToken = lens _lcsrsNextToken (\ s a -> s{_lcsrsNextToken = a})

-- | A list of @ChangeSetSummary@ structures that provides the ID and status of each change set for the specified stack.
lcsrsSummaries :: Lens' ListChangeSetsResponse [ChangeSetSummary]
lcsrsSummaries = lens _lcsrsSummaries (\ s a -> s{_lcsrsSummaries = a}) . _Default . _Coerce

-- | -- | The response status code.
lcsrsResponseStatus :: Lens' ListChangeSetsResponse Int
lcsrsResponseStatus = lens _lcsrsResponseStatus (\ s a -> s{_lcsrsResponseStatus = a})

instance NFData ListChangeSetsResponse where
