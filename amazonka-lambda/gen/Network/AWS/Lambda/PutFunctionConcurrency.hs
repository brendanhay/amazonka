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
-- Module      : Network.AWS.Lambda.PutFunctionConcurrency
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the maximum number of simultaneous executions for a function, and reserves capacity for that concurrency level.
--
--
-- Concurrency settings apply to the function as a whole, including all published versions and the unpublished version. Reserving concurrency both ensures that your function has capacity to process the specified number of events simultaneously, and prevents it from scaling beyond that level. Use 'GetFunction' to see the current setting for a function.
--
-- Use 'GetAccountSettings' to see your regional concurrency limit. You can reserve concurrency for as many functions as you like, as long as you leave at least 100 simultaneous executions unreserved for functions that aren't configured with a per-function limit. For more information, see <https://docs.aws.amazon.com/lambda/latest/dg/concurrent-executions.html Managing Concurrency> .
--
module Network.AWS.Lambda.PutFunctionConcurrency
    (
    -- * Creating a Request
      putFunctionConcurrency
    , PutFunctionConcurrency
    -- * Request Lenses
    , pfcFunctionName
    , pfcReservedConcurrentExecutions

    -- * Destructuring the Response
    , concurrency
    , Concurrency
    -- * Response Lenses
    , cReservedConcurrentExecutions
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'putFunctionConcurrency' smart constructor.
data PutFunctionConcurrency = PutFunctionConcurrency'
  { _pfcFunctionName                 :: !Text
  , _pfcReservedConcurrentExecutions :: !Nat
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutFunctionConcurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pfcFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
--
-- * 'pfcReservedConcurrentExecutions' - The number of simultaneous executions to reserve for the function.
putFunctionConcurrency
    :: Text -- ^ 'pfcFunctionName'
    -> Natural -- ^ 'pfcReservedConcurrentExecutions'
    -> PutFunctionConcurrency
putFunctionConcurrency pFunctionName_ pReservedConcurrentExecutions_ =
  PutFunctionConcurrency'
    { _pfcFunctionName = pFunctionName_
    , _pfcReservedConcurrentExecutions = _Nat # pReservedConcurrentExecutions_
    }


-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
pfcFunctionName :: Lens' PutFunctionConcurrency Text
pfcFunctionName = lens _pfcFunctionName (\ s a -> s{_pfcFunctionName = a})

-- | The number of simultaneous executions to reserve for the function.
pfcReservedConcurrentExecutions :: Lens' PutFunctionConcurrency Natural
pfcReservedConcurrentExecutions = lens _pfcReservedConcurrentExecutions (\ s a -> s{_pfcReservedConcurrentExecutions = a}) . _Nat

instance AWSRequest PutFunctionConcurrency where
        type Rs PutFunctionConcurrency = Concurrency
        request = putJSON lambda
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance Hashable PutFunctionConcurrency where

instance NFData PutFunctionConcurrency where

instance ToHeaders PutFunctionConcurrency where
        toHeaders = const mempty

instance ToJSON PutFunctionConcurrency where
        toJSON PutFunctionConcurrency'{..}
          = object
              (catMaybes
                 [Just
                    ("ReservedConcurrentExecutions" .=
                       _pfcReservedConcurrentExecutions)])

instance ToPath PutFunctionConcurrency where
        toPath PutFunctionConcurrency'{..}
          = mconcat
              ["/2017-10-31/functions/", toBS _pfcFunctionName,
               "/concurrency"]

instance ToQuery PutFunctionConcurrency where
        toQuery = const mempty
