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
-- Module      : Network.AWS.DAX.DescribeDefaultParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default system parameter information for the DAX caching software.
--
--
module Network.AWS.DAX.DescribeDefaultParameters
    (
    -- * Creating a Request
      describeDefaultParameters
    , DescribeDefaultParameters
    -- * Request Lenses
    , ddpNextToken
    , ddpMaxResults

    -- * Destructuring the Response
    , describeDefaultParametersResponse
    , DescribeDefaultParametersResponse
    -- * Response Lenses
    , ddprsNextToken
    , ddprsParameters
    , ddprsResponseStatus
    ) where

import Network.AWS.DAX.Types
import Network.AWS.DAX.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeDefaultParameters' smart constructor.
data DescribeDefaultParameters = DescribeDefaultParameters'
  { _ddpNextToken  :: !(Maybe Text)
  , _ddpMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDefaultParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddpNextToken' - An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
--
-- * 'ddpMaxResults' - The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
describeDefaultParameters
    :: DescribeDefaultParameters
describeDefaultParameters =
  DescribeDefaultParameters' {_ddpNextToken = Nothing, _ddpMaxResults = Nothing}


-- | An optional token returned from a prior request. Use this token for pagination of results from this action. If this parameter is specified, the response includes only results beyond the token, up to the value specified by @MaxResults@ .
ddpNextToken :: Lens' DescribeDefaultParameters (Maybe Text)
ddpNextToken = lens _ddpNextToken (\ s a -> s{_ddpNextToken = a})

-- | The maximum number of results to include in the response. If more results exist than the specified @MaxResults@ value, a token is included in the response so that the remaining results can be retrieved. The value for @MaxResults@ must be between 20 and 100.
ddpMaxResults :: Lens' DescribeDefaultParameters (Maybe Int)
ddpMaxResults = lens _ddpMaxResults (\ s a -> s{_ddpMaxResults = a})

instance AWSRequest DescribeDefaultParameters where
        type Rs DescribeDefaultParameters =
             DescribeDefaultParametersResponse
        request = postJSON dax
        response
          = receiveJSON
              (\ s h x ->
                 DescribeDefaultParametersResponse' <$>
                   (x .?> "NextToken") <*>
                     (x .?> "Parameters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable DescribeDefaultParameters where

instance NFData DescribeDefaultParameters where

instance ToHeaders DescribeDefaultParameters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDAXV3.DescribeDefaultParameters" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeDefaultParameters where
        toJSON DescribeDefaultParameters'{..}
          = object
              (catMaybes
                 [("NextToken" .=) <$> _ddpNextToken,
                  ("MaxResults" .=) <$> _ddpMaxResults])

instance ToPath DescribeDefaultParameters where
        toPath = const "/"

instance ToQuery DescribeDefaultParameters where
        toQuery = const mempty

-- | /See:/ 'describeDefaultParametersResponse' smart constructor.
data DescribeDefaultParametersResponse = DescribeDefaultParametersResponse'
  { _ddprsNextToken      :: !(Maybe Text)
  , _ddprsParameters     :: !(Maybe [Parameter])
  , _ddprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeDefaultParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ddprsNextToken' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'ddprsParameters' - A list of parameters. Each element in the list represents one parameter.
--
-- * 'ddprsResponseStatus' - -- | The response status code.
describeDefaultParametersResponse
    :: Int -- ^ 'ddprsResponseStatus'
    -> DescribeDefaultParametersResponse
describeDefaultParametersResponse pResponseStatus_ =
  DescribeDefaultParametersResponse'
    { _ddprsNextToken = Nothing
    , _ddprsParameters = Nothing
    , _ddprsResponseStatus = pResponseStatus_
    }


-- | Provides an identifier to allow retrieval of paginated results.
ddprsNextToken :: Lens' DescribeDefaultParametersResponse (Maybe Text)
ddprsNextToken = lens _ddprsNextToken (\ s a -> s{_ddprsNextToken = a})

-- | A list of parameters. Each element in the list represents one parameter.
ddprsParameters :: Lens' DescribeDefaultParametersResponse [Parameter]
ddprsParameters = lens _ddprsParameters (\ s a -> s{_ddprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
ddprsResponseStatus :: Lens' DescribeDefaultParametersResponse Int
ddprsResponseStatus = lens _ddprsResponseStatus (\ s a -> s{_ddprsResponseStatus = a})

instance NFData DescribeDefaultParametersResponse
         where
