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
-- Module      : Network.AWS.Athena.StopQueryExecution
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops a query execution.
--
--
-- For code samples using the AWS SDK for Java, see <http://docs.aws.amazon.com/athena/latest/ug/code-samples.html Examples and Code Samples> in the /Amazon Athena User Guide/ .
--
module Network.AWS.Athena.StopQueryExecution
    (
    -- * Creating a Request
      stopQueryExecution
    , StopQueryExecution
    -- * Request Lenses
    , sqeQueryExecutionId

    -- * Destructuring the Response
    , stopQueryExecutionResponse
    , StopQueryExecutionResponse
    -- * Response Lenses
    , srsResponseStatus
    ) where

import Network.AWS.Athena.Types
import Network.AWS.Athena.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopQueryExecution' smart constructor.
newtype StopQueryExecution = StopQueryExecution'
  { _sqeQueryExecutionId :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopQueryExecution' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sqeQueryExecutionId' - The unique ID of the query execution to stop.
stopQueryExecution
    :: Text -- ^ 'sqeQueryExecutionId'
    -> StopQueryExecution
stopQueryExecution pQueryExecutionId_ =
  StopQueryExecution' {_sqeQueryExecutionId = pQueryExecutionId_}


-- | The unique ID of the query execution to stop.
sqeQueryExecutionId :: Lens' StopQueryExecution Text
sqeQueryExecutionId = lens _sqeQueryExecutionId (\ s a -> s{_sqeQueryExecutionId = a})

instance AWSRequest StopQueryExecution where
        type Rs StopQueryExecution =
             StopQueryExecutionResponse
        request = postJSON athena
        response
          = receiveEmpty
              (\ s h x ->
                 StopQueryExecutionResponse' <$> (pure (fromEnum s)))

instance Hashable StopQueryExecution where

instance NFData StopQueryExecution where

instance ToHeaders StopQueryExecution where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonAthena.StopQueryExecution" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopQueryExecution where
        toJSON StopQueryExecution'{..}
          = object
              (catMaybes
                 [Just ("QueryExecutionId" .= _sqeQueryExecutionId)])

instance ToPath StopQueryExecution where
        toPath = const "/"

instance ToQuery StopQueryExecution where
        toQuery = const mempty

-- | /See:/ 'stopQueryExecutionResponse' smart constructor.
newtype StopQueryExecutionResponse = StopQueryExecutionResponse'
  { _srsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StopQueryExecutionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'srsResponseStatus' - -- | The response status code.
stopQueryExecutionResponse
    :: Int -- ^ 'srsResponseStatus'
    -> StopQueryExecutionResponse
stopQueryExecutionResponse pResponseStatus_ =
  StopQueryExecutionResponse' {_srsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
srsResponseStatus :: Lens' StopQueryExecutionResponse Int
srsResponseStatus = lens _srsResponseStatus (\ s a -> s{_srsResponseStatus = a})

instance NFData StopQueryExecutionResponse where
