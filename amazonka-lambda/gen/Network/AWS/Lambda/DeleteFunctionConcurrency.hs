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
-- Module      : Network.AWS.Lambda.DeleteFunctionConcurrency
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a concurrent execution limit from a function.
--
--
module Network.AWS.Lambda.DeleteFunctionConcurrency
    (
    -- * Creating a Request
      deleteFunctionConcurrency
    , DeleteFunctionConcurrency
    -- * Request Lenses
    , dfcFunctionName

    -- * Destructuring the Response
    , deleteFunctionConcurrencyResponse
    , DeleteFunctionConcurrencyResponse
    ) where

import Network.AWS.Lambda.Types
import Network.AWS.Lambda.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteFunctionConcurrency' smart constructor.
newtype DeleteFunctionConcurrency = DeleteFunctionConcurrency'
  { _dfcFunctionName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFunctionConcurrency' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfcFunctionName' - The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
deleteFunctionConcurrency
    :: Text -- ^ 'dfcFunctionName'
    -> DeleteFunctionConcurrency
deleteFunctionConcurrency pFunctionName_ =
  DeleteFunctionConcurrency' {_dfcFunctionName = pFunctionName_}


-- | The name of the Lambda function. __Name formats__      * __Function name__ - @my-function@ .     * __Function ARN__ - @arn:aws:lambda:us-west-2:123456789012:function:my-function@ .     * __Partial ARN__ - @123456789012:function:my-function@ . The length constraint applies only to the full ARN. If you specify only the function name, it is limited to 64 characters in length.
dfcFunctionName :: Lens' DeleteFunctionConcurrency Text
dfcFunctionName = lens _dfcFunctionName (\ s a -> s{_dfcFunctionName = a})

instance AWSRequest DeleteFunctionConcurrency where
        type Rs DeleteFunctionConcurrency =
             DeleteFunctionConcurrencyResponse
        request = delete lambda
        response
          = receiveNull DeleteFunctionConcurrencyResponse'

instance Hashable DeleteFunctionConcurrency where

instance NFData DeleteFunctionConcurrency where

instance ToHeaders DeleteFunctionConcurrency where
        toHeaders = const mempty

instance ToPath DeleteFunctionConcurrency where
        toPath DeleteFunctionConcurrency'{..}
          = mconcat
              ["/2017-10-31/functions/", toBS _dfcFunctionName,
               "/concurrency"]

instance ToQuery DeleteFunctionConcurrency where
        toQuery = const mempty

-- | /See:/ 'deleteFunctionConcurrencyResponse' smart constructor.
data DeleteFunctionConcurrencyResponse =
  DeleteFunctionConcurrencyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteFunctionConcurrencyResponse' with the minimum fields required to make a request.
--
deleteFunctionConcurrencyResponse
    :: DeleteFunctionConcurrencyResponse
deleteFunctionConcurrencyResponse = DeleteFunctionConcurrencyResponse'


instance NFData DeleteFunctionConcurrencyResponse
         where
