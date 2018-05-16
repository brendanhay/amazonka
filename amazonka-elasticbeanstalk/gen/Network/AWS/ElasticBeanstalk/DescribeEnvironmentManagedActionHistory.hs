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
-- Module      : Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists an environment's completed and failed managed actions.
--
--
module Network.AWS.ElasticBeanstalk.DescribeEnvironmentManagedActionHistory
    (
    -- * Creating a Request
      describeEnvironmentManagedActionHistory
    , DescribeEnvironmentManagedActionHistory
    -- * Request Lenses
    , demahNextToken
    , demahEnvironmentName
    , demahMaxItems
    , demahEnvironmentId

    -- * Destructuring the Response
    , describeEnvironmentManagedActionHistoryResponse
    , DescribeEnvironmentManagedActionHistoryResponse
    -- * Response Lenses
    , demahrsManagedActionHistoryItems
    , demahrsNextToken
    , demahrsResponseStatus
    ) where

import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.ElasticBeanstalk.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Request to list completed and failed managed actions.
--
--
--
-- /See:/ 'describeEnvironmentManagedActionHistory' smart constructor.
data DescribeEnvironmentManagedActionHistory = DescribeEnvironmentManagedActionHistory'
  { _demahNextToken       :: !(Maybe Text)
  , _demahEnvironmentName :: !(Maybe Text)
  , _demahMaxItems        :: !(Maybe Int)
  , _demahEnvironmentId   :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentManagedActionHistory' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demahNextToken' - The pagination token returned by a previous request.
--
-- * 'demahEnvironmentName' - The name of the target environment.
--
-- * 'demahMaxItems' - The maximum number of items to return for a single request.
--
-- * 'demahEnvironmentId' - The environment ID of the target environment.
describeEnvironmentManagedActionHistory
    :: DescribeEnvironmentManagedActionHistory
describeEnvironmentManagedActionHistory =
  DescribeEnvironmentManagedActionHistory'
    { _demahNextToken = Nothing
    , _demahEnvironmentName = Nothing
    , _demahMaxItems = Nothing
    , _demahEnvironmentId = Nothing
    }


-- | The pagination token returned by a previous request.
demahNextToken :: Lens' DescribeEnvironmentManagedActionHistory (Maybe Text)
demahNextToken = lens _demahNextToken (\ s a -> s{_demahNextToken = a})

-- | The name of the target environment.
demahEnvironmentName :: Lens' DescribeEnvironmentManagedActionHistory (Maybe Text)
demahEnvironmentName = lens _demahEnvironmentName (\ s a -> s{_demahEnvironmentName = a})

-- | The maximum number of items to return for a single request.
demahMaxItems :: Lens' DescribeEnvironmentManagedActionHistory (Maybe Int)
demahMaxItems = lens _demahMaxItems (\ s a -> s{_demahMaxItems = a})

-- | The environment ID of the target environment.
demahEnvironmentId :: Lens' DescribeEnvironmentManagedActionHistory (Maybe Text)
demahEnvironmentId = lens _demahEnvironmentId (\ s a -> s{_demahEnvironmentId = a})

instance AWSRequest
           DescribeEnvironmentManagedActionHistory
         where
        type Rs DescribeEnvironmentManagedActionHistory =
             DescribeEnvironmentManagedActionHistoryResponse
        request = postQuery elasticBeanstalk
        response
          = receiveXMLWrapper
              "DescribeEnvironmentManagedActionHistoryResult"
              (\ s h x ->
                 DescribeEnvironmentManagedActionHistoryResponse' <$>
                   (x .@? "ManagedActionHistoryItems" .!@ mempty >>=
                      may (parseXMLList1 "member"))
                     <*> (x .@? "NextToken")
                     <*> (pure (fromEnum s)))

instance Hashable
           DescribeEnvironmentManagedActionHistory
         where

instance NFData
           DescribeEnvironmentManagedActionHistory
         where

instance ToHeaders
           DescribeEnvironmentManagedActionHistory
         where
        toHeaders = const mempty

instance ToPath
           DescribeEnvironmentManagedActionHistory
         where
        toPath = const "/"

instance ToQuery
           DescribeEnvironmentManagedActionHistory
         where
        toQuery DescribeEnvironmentManagedActionHistory'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEnvironmentManagedActionHistory" ::
                    ByteString),
               "Version" =: ("2010-12-01" :: ByteString),
               "NextToken" =: _demahNextToken,
               "EnvironmentName" =: _demahEnvironmentName,
               "MaxItems" =: _demahMaxItems,
               "EnvironmentId" =: _demahEnvironmentId]

-- | A result message containing a list of completed and failed managed actions.
--
--
--
-- /See:/ 'describeEnvironmentManagedActionHistoryResponse' smart constructor.
data DescribeEnvironmentManagedActionHistoryResponse = DescribeEnvironmentManagedActionHistoryResponse'
  { _demahrsManagedActionHistoryItems :: !(Maybe (List1 ManagedActionHistoryItem))
  , _demahrsNextToken :: !(Maybe Text)
  , _demahrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEnvironmentManagedActionHistoryResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'demahrsManagedActionHistoryItems' - A list of completed and failed managed actions.
--
-- * 'demahrsNextToken' - A pagination token that you pass to 'DescribeEnvironmentManagedActionHistory' to get the next page of results.
--
-- * 'demahrsResponseStatus' - -- | The response status code.
describeEnvironmentManagedActionHistoryResponse
    :: Int -- ^ 'demahrsResponseStatus'
    -> DescribeEnvironmentManagedActionHistoryResponse
describeEnvironmentManagedActionHistoryResponse pResponseStatus_ =
  DescribeEnvironmentManagedActionHistoryResponse'
    { _demahrsManagedActionHistoryItems = Nothing
    , _demahrsNextToken = Nothing
    , _demahrsResponseStatus = pResponseStatus_
    }


-- | A list of completed and failed managed actions.
demahrsManagedActionHistoryItems :: Lens' DescribeEnvironmentManagedActionHistoryResponse (Maybe (NonEmpty ManagedActionHistoryItem))
demahrsManagedActionHistoryItems = lens _demahrsManagedActionHistoryItems (\ s a -> s{_demahrsManagedActionHistoryItems = a}) . mapping _List1

-- | A pagination token that you pass to 'DescribeEnvironmentManagedActionHistory' to get the next page of results.
demahrsNextToken :: Lens' DescribeEnvironmentManagedActionHistoryResponse (Maybe Text)
demahrsNextToken = lens _demahrsNextToken (\ s a -> s{_demahrsNextToken = a})

-- | -- | The response status code.
demahrsResponseStatus :: Lens' DescribeEnvironmentManagedActionHistoryResponse Int
demahrsResponseStatus = lens _demahrsResponseStatus (\ s a -> s{_demahrsResponseStatus = a})

instance NFData
           DescribeEnvironmentManagedActionHistoryResponse
         where
