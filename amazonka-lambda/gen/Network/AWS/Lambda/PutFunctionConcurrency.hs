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
-- Sets a limit on the number of concurrent executions available to this function. It is a subset of your account's total concurrent execution limit per region. Note that Lambda automatically reserves a buffer of 100 concurrent executions for functions without any reserved concurrency limit. This means if your account limit is 1000, you have a total of 900 available to allocate to individual functions. For more information, see 'concurrent-executions' .
--
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
-- * 'pfcFunctionName' - The name of the function you are setting concurrent execution limits on. For more information, see 'concurrent-executions' .
--
-- * 'pfcReservedConcurrentExecutions' - The concurrent execution limit reserved for this function. For more information, see 'concurrent-executions' .
putFunctionConcurrency
    :: Text -- ^ 'pfcFunctionName'
    -> Natural -- ^ 'pfcReservedConcurrentExecutions'
    -> PutFunctionConcurrency
putFunctionConcurrency pFunctionName_ pReservedConcurrentExecutions_ =
  PutFunctionConcurrency'
    { _pfcFunctionName = pFunctionName_
    , _pfcReservedConcurrentExecutions = _Nat # pReservedConcurrentExecutions_
    }


-- | The name of the function you are setting concurrent execution limits on. For more information, see 'concurrent-executions' .
pfcFunctionName :: Lens' PutFunctionConcurrency Text
pfcFunctionName = lens _pfcFunctionName (\ s a -> s{_pfcFunctionName = a})

-- | The concurrent execution limit reserved for this function. For more information, see 'concurrent-executions' .
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
