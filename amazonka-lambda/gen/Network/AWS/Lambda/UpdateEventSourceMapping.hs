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
-- Module      : Network.AWS.Lambda.UpdateEventSourceMapping
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an event source mapping. You can change the function that AWS Lambda invokes, or pause invocation and resume later from the same location.
--
--
module Network.AWS.Lambda.UpdateEventSourceMapping
    (
    -- * Creating a Request
      updateEventSourceMapping
    , UpdateEventSourceMapping
    -- * Request Lenses
    , uesmEnabled
    , uesmBatchSize
    , uesmFunctionName
    , uesmUUId

    -- * Destructuring the Response
    , eventSourceMappingConfiguration
    , EventSourceMappingConfiguration
    -- * Response Lenses
    , esmcEventSourceARN
    , esmcState
    , esmcFunctionARN
    , esmcUUId
    , esmcLastProcessingResult
    , esmcBatchSize
    , esmcStateTransitionReason
    , esmcLastModified
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateEventSourceMapping' smart constructor.
data UpdateEventSourceMapping = UpdateEventSourceMapping'
  { _uesmEnabled      :: !(Maybe Bool)
  , _uesmBatchSize    :: !(Maybe Nat)
  , _uesmFunctionName :: !(Maybe Text)
  , _uesmUUId         :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateEventSourceMapping' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uesmEnabled' - Disables the event source mapping to pause polling and invocation.
--
-- * 'uesmBatchSize' - The maximum number of items to retrieve in a single batch.     * __Amazon Kinesis__ - Default 100. Max 10,000.     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.     * __Amazon Simple Queue Service__ - Default 10. Max 10.
--
-- * 'uesmFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
--
-- * 'uesmUUId' - The identifier of the event source mapping.
updateEventSourceMapping
    :: Text -- ^ 'uesmUUId'
    -> UpdateEventSourceMapping
updateEventSourceMapping pUUId_ =
  UpdateEventSourceMapping'
    { _uesmEnabled = Nothing
    , _uesmBatchSize = Nothing
    , _uesmFunctionName = Nothing
    , _uesmUUId = pUUId_
    }


-- | Disables the event source mapping to pause polling and invocation.
uesmEnabled :: Lens' UpdateEventSourceMapping (Maybe Bool)
uesmEnabled = lens _uesmEnabled (\ s a -> s{_uesmEnabled = a})

-- | The maximum number of items to retrieve in a single batch.     * __Amazon Kinesis__ - Default 100. Max 10,000.     * __Amazon DynamoDB Streams__ - Default 100. Max 1,000.     * __Amazon Simple Queue Service__ - Default 10. Max 10.
uesmBatchSize :: Lens' UpdateEventSourceMapping (Maybe Natural)
uesmBatchSize = lens _uesmBatchSize (\ s a -> s{_uesmBatchSize = a}) . mapping _Nat

-- | The name of the Lambda function. __Name formats__      * __Function name__ - @MyFunction@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction@ .     * __Version or Alias ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:MyFunction:PROD@ .     * __Partial ARN__ - @123456789012:function:MyFunction@ . The length constraint applies only to the full ARN. If you specify only the function name, it's limited to 64 characters in length.
uesmFunctionName :: Lens' UpdateEventSourceMapping (Maybe Text)
uesmFunctionName = lens _uesmFunctionName (\ s a -> s{_uesmFunctionName = a})

-- | The identifier of the event source mapping.
uesmUUId :: Lens' UpdateEventSourceMapping Text
uesmUUId = lens _uesmUUId (\ s a -> s{_uesmUUId = a})

instance AWSRequest UpdateEventSourceMapping where
        type Rs UpdateEventSourceMapping =
             EventSourceMappingConfiguration
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable UpdateEventSourceMapping where

instance NFData UpdateEventSourceMapping where

instance ToHeaders UpdateEventSourceMapping where
        toHeaders = const mempty

instance ToJSON UpdateEventSourceMapping where
        toJSON UpdateEventSourceMapping'{..}
          = object
              (catMaybes
                 [("Enabled" .=) <$> _uesmEnabled,
                  ("BatchSize" .=) <$> _uesmBatchSize,
                  ("FunctionName" .=) <$> _uesmFunctionName])

instance ToPath UpdateEventSourceMapping where
        toPath UpdateEventSourceMapping'{..}
          = mconcat
              ["/2015-03-31/event-source-mappings/",
               toBS _uesmUUId]

instance ToQuery UpdateEventSourceMapping where
        toQuery = const mempty
