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
-- Module      : Network.AWS.Config.PutEvaluations
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by an AWS Lambda function to deliver evaluation results to AWS Config. This action is required in every AWS Lambda function that is invoked by an AWS Config rule.
--
--
module Network.AWS.Config.PutEvaluations
    (
    -- * Creating a Request
      putEvaluations
    , PutEvaluations
    -- * Request Lenses
    , peEvaluations
    , peTestMode
    , peResultToken

    -- * Destructuring the Response
    , putEvaluationsResponse
    , PutEvaluationsResponse
    -- * Response Lenses
    , persFailedEvaluations
    , persResponseStatus
    ) where

import Network.AWS.Config.Types
import Network.AWS.Config.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- |
--
--
--
-- /See:/ 'putEvaluations' smart constructor.
data PutEvaluations = PutEvaluations'
  { _peEvaluations :: !(Maybe [Evaluation])
  , _peTestMode    :: !(Maybe Bool)
  , _peResultToken :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEvaluations' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'peEvaluations' - The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
--
-- * 'peTestMode' - Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
--
-- * 'peResultToken' - An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
putEvaluations
    :: Text -- ^ 'peResultToken'
    -> PutEvaluations
putEvaluations pResultToken_ =
  PutEvaluations'
    { _peEvaluations = Nothing
    , _peTestMode = Nothing
    , _peResultToken = pResultToken_
    }


-- | The assessments that the AWS Lambda function performs. Each evaluation identifies an AWS resource and indicates whether it complies with the AWS Config rule that invokes the AWS Lambda function.
peEvaluations :: Lens' PutEvaluations [Evaluation]
peEvaluations = lens _peEvaluations (\ s a -> s{_peEvaluations = a}) . _Default . _Coerce

-- | Use this parameter to specify a test run for @PutEvaluations@ . You can verify whether your AWS Lambda function will deliver evaluation results to AWS Config. No updates occur to your existing evaluations, and evaluation results are not sent to AWS Config.
peTestMode :: Lens' PutEvaluations (Maybe Bool)
peTestMode = lens _peTestMode (\ s a -> s{_peTestMode = a})

-- | An encrypted token that associates an evaluation with an AWS Config rule. Identifies the rule and the event that triggered the evaluation.
peResultToken :: Lens' PutEvaluations Text
peResultToken = lens _peResultToken (\ s a -> s{_peResultToken = a})

instance AWSRequest PutEvaluations where
        type Rs PutEvaluations = PutEvaluationsResponse
        request = postJSON config
        response
          = receiveJSON
              (\ s h x ->
                 PutEvaluationsResponse' <$>
                   (x .?> "FailedEvaluations" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable PutEvaluations where

instance NFData PutEvaluations where

instance ToHeaders PutEvaluations where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StarlingDoveService.PutEvaluations" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON PutEvaluations where
        toJSON PutEvaluations'{..}
          = object
              (catMaybes
                 [("Evaluations" .=) <$> _peEvaluations,
                  ("TestMode" .=) <$> _peTestMode,
                  Just ("ResultToken" .= _peResultToken)])

instance ToPath PutEvaluations where
        toPath = const "/"

instance ToQuery PutEvaluations where
        toQuery = const mempty

-- |
--
--
--
-- /See:/ 'putEvaluationsResponse' smart constructor.
data PutEvaluationsResponse = PutEvaluationsResponse'
  { _persFailedEvaluations :: !(Maybe [Evaluation])
  , _persResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutEvaluationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'persFailedEvaluations' - Requests that failed because of a client or server error.
--
-- * 'persResponseStatus' - -- | The response status code.
putEvaluationsResponse
    :: Int -- ^ 'persResponseStatus'
    -> PutEvaluationsResponse
putEvaluationsResponse pResponseStatus_ =
  PutEvaluationsResponse'
    {_persFailedEvaluations = Nothing, _persResponseStatus = pResponseStatus_}


-- | Requests that failed because of a client or server error.
persFailedEvaluations :: Lens' PutEvaluationsResponse [Evaluation]
persFailedEvaluations = lens _persFailedEvaluations (\ s a -> s{_persFailedEvaluations = a}) . _Default . _Coerce

-- | -- | The response status code.
persResponseStatus :: Lens' PutEvaluationsResponse Int
persResponseStatus = lens _persResponseStatus (\ s a -> s{_persResponseStatus = a})

instance NFData PutEvaluationsResponse where
