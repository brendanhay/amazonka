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
-- Module      : Network.AWS.Batch.DescribeJobDefinitions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a list of job definitions. You can specify a @status@ (such as @ACTIVE@ ) to only return job definitions that match that status.
--
--
module Network.AWS.Batch.DescribeJobDefinitions
    (
    -- * Creating a Request
      describeJobDefinitions
    , DescribeJobDefinitions
    -- * Request Lenses
    , djdStatus
    , djdJobDefinitionName
    , djdJobDefinitions
    , djdNextToken
    , djdMaxResults

    -- * Destructuring the Response
    , describeJobDefinitionsResponse
    , DescribeJobDefinitionsResponse
    -- * Response Lenses
    , djdrsJobDefinitions
    , djdrsNextToken
    , djdrsResponseStatus
    ) where

import Network.AWS.Batch.Types
import Network.AWS.Batch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeJobDefinitions' smart constructor.
data DescribeJobDefinitions = DescribeJobDefinitions'
  { _djdStatus            :: !(Maybe Text)
  , _djdJobDefinitionName :: !(Maybe Text)
  , _djdJobDefinitions    :: !(Maybe [Text])
  , _djdNextToken         :: !(Maybe Text)
  , _djdMaxResults        :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobDefinitions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djdStatus' - The status with which to filter job definitions.
--
-- * 'djdJobDefinitionName' - The name of the job definition to describe.
--
-- * 'djdJobDefinitions' - A space-separated list of up to 100 job definition names or full Amazon Resource Name (ARN) entries.
--
-- * 'djdNextToken' - The @nextToken@ value returned from a previous paginated @DescribeJobDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
--
-- * 'djdMaxResults' - The maximum number of results returned by @DescribeJobDefinitions@ in paginated output. When this parameter is used, @DescribeJobDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
describeJobDefinitions
    :: DescribeJobDefinitions
describeJobDefinitions =
  DescribeJobDefinitions'
    { _djdStatus = Nothing
    , _djdJobDefinitionName = Nothing
    , _djdJobDefinitions = Nothing
    , _djdNextToken = Nothing
    , _djdMaxResults = Nothing
    }


-- | The status with which to filter job definitions.
djdStatus :: Lens' DescribeJobDefinitions (Maybe Text)
djdStatus = lens _djdStatus (\ s a -> s{_djdStatus = a})

-- | The name of the job definition to describe.
djdJobDefinitionName :: Lens' DescribeJobDefinitions (Maybe Text)
djdJobDefinitionName = lens _djdJobDefinitionName (\ s a -> s{_djdJobDefinitionName = a})

-- | A space-separated list of up to 100 job definition names or full Amazon Resource Name (ARN) entries.
djdJobDefinitions :: Lens' DescribeJobDefinitions [Text]
djdJobDefinitions = lens _djdJobDefinitions (\ s a -> s{_djdJobDefinitions = a}) . _Default . _Coerce

-- | The @nextToken@ value returned from a previous paginated @DescribeJobDefinitions@ request where @maxResults@ was used and the results exceeded the value of that parameter. Pagination continues from the end of the previous results that returned the @nextToken@ value. This value is @null@ when there are no more results to return.
djdNextToken :: Lens' DescribeJobDefinitions (Maybe Text)
djdNextToken = lens _djdNextToken (\ s a -> s{_djdNextToken = a})

-- | The maximum number of results returned by @DescribeJobDefinitions@ in paginated output. When this parameter is used, @DescribeJobDefinitions@ only returns @maxResults@ results in a single page along with a @nextToken@ response element. The remaining results of the initial request can be seen by sending another @DescribeJobDefinitions@ request with the returned @nextToken@ value. This value can be between 1 and 100. If this parameter is not used, then @DescribeJobDefinitions@ returns up to 100 results and a @nextToken@ value if applicable.
djdMaxResults :: Lens' DescribeJobDefinitions (Maybe Int)
djdMaxResults = lens _djdMaxResults (\ s a -> s{_djdMaxResults = a})

instance AWSRequest DescribeJobDefinitions where
        type Rs DescribeJobDefinitions =
             DescribeJobDefinitionsResponse
        request = postJSON batch
        response
          = receiveJSON
              (\ s h x ->
                 DescribeJobDefinitionsResponse' <$>
                   (x .?> "jobDefinitions" .!@ mempty) <*>
                     (x .?> "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeJobDefinitions where

instance NFData DescribeJobDefinitions where

instance ToHeaders DescribeJobDefinitions where
        toHeaders
          = const
              (mconcat
                 ["Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeJobDefinitions where
        toJSON DescribeJobDefinitions'{..}
          = object
              (catMaybes
                 [("status" .=) <$> _djdStatus,
                  ("jobDefinitionName" .=) <$> _djdJobDefinitionName,
                  ("jobDefinitions" .=) <$> _djdJobDefinitions,
                  ("nextToken" .=) <$> _djdNextToken,
                  ("maxResults" .=) <$> _djdMaxResults])

instance ToPath DescribeJobDefinitions where
        toPath = const "/v1/describejobdefinitions"

instance ToQuery DescribeJobDefinitions where
        toQuery = const mempty

-- | /See:/ 'describeJobDefinitionsResponse' smart constructor.
data DescribeJobDefinitionsResponse = DescribeJobDefinitionsResponse'
  { _djdrsJobDefinitions :: !(Maybe [JobDefinition])
  , _djdrsNextToken      :: !(Maybe Text)
  , _djdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeJobDefinitionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'djdrsJobDefinitions' - The list of job definitions.
--
-- * 'djdrsNextToken' - The @nextToken@ value to include in a future @DescribeJobDefinitions@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'djdrsResponseStatus' - -- | The response status code.
describeJobDefinitionsResponse
    :: Int -- ^ 'djdrsResponseStatus'
    -> DescribeJobDefinitionsResponse
describeJobDefinitionsResponse pResponseStatus_ =
  DescribeJobDefinitionsResponse'
    { _djdrsJobDefinitions = Nothing
    , _djdrsNextToken = Nothing
    , _djdrsResponseStatus = pResponseStatus_
    }


-- | The list of job definitions.
djdrsJobDefinitions :: Lens' DescribeJobDefinitionsResponse [JobDefinition]
djdrsJobDefinitions = lens _djdrsJobDefinitions (\ s a -> s{_djdrsJobDefinitions = a}) . _Default . _Coerce

-- | The @nextToken@ value to include in a future @DescribeJobDefinitions@ request. When the results of a @DescribeJobDefinitions@ request exceed @maxResults@ , this value can be used to retrieve the next page of results. This value is @null@ when there are no more results to return.
djdrsNextToken :: Lens' DescribeJobDefinitionsResponse (Maybe Text)
djdrsNextToken = lens _djdrsNextToken (\ s a -> s{_djdrsNextToken = a})

-- | -- | The response status code.
djdrsResponseStatus :: Lens' DescribeJobDefinitionsResponse Int
djdrsResponseStatus = lens _djdrsResponseStatus (\ s a -> s{_djdrsResponseStatus = a})

instance NFData DescribeJobDefinitionsResponse where
