{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LambdaActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LambdaActivity where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An activity that runs a Lambda function to modify the message.
--
--
--
-- /See:/ 'lambdaActivity' smart constructor.
data LambdaActivity = LambdaActivity'
  { _laNext :: !(Maybe Text),
    _laName :: !Text,
    _laLambdaName :: !Text,
    _laBatchSize :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaActivity' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laNext' - The next activity in the pipeline.
--
-- * 'laName' - The name of the lambda activity.
--
-- * 'laLambdaName' - The name of the Lambda function that is run on the message.
--
-- * 'laBatchSize' - The number of messages passed to the Lambda function for processing. The Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
lambdaActivity ::
  -- | 'laName'
  Text ->
  -- | 'laLambdaName'
  Text ->
  -- | 'laBatchSize'
  Natural ->
  LambdaActivity
lambdaActivity pName_ pLambdaName_ pBatchSize_ =
  LambdaActivity'
    { _laNext = Nothing,
      _laName = pName_,
      _laLambdaName = pLambdaName_,
      _laBatchSize = _Nat # pBatchSize_
    }

-- | The next activity in the pipeline.
laNext :: Lens' LambdaActivity (Maybe Text)
laNext = lens _laNext (\s a -> s {_laNext = a})

-- | The name of the lambda activity.
laName :: Lens' LambdaActivity Text
laName = lens _laName (\s a -> s {_laName = a})

-- | The name of the Lambda function that is run on the message.
laLambdaName :: Lens' LambdaActivity Text
laLambdaName = lens _laLambdaName (\s a -> s {_laLambdaName = a})

-- | The number of messages passed to the Lambda function for processing. The Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
laBatchSize :: Lens' LambdaActivity Natural
laBatchSize = lens _laBatchSize (\s a -> s {_laBatchSize = a}) . _Nat

instance FromJSON LambdaActivity where
  parseJSON =
    withObject
      "LambdaActivity"
      ( \x ->
          LambdaActivity'
            <$> (x .:? "next")
            <*> (x .: "name")
            <*> (x .: "lambdaName")
            <*> (x .: "batchSize")
      )

instance Hashable LambdaActivity

instance NFData LambdaActivity

instance ToJSON LambdaActivity where
  toJSON LambdaActivity' {..} =
    object
      ( catMaybes
          [ ("next" .=) <$> _laNext,
            Just ("name" .= _laName),
            Just ("lambdaName" .= _laLambdaName),
            Just ("batchSize" .= _laBatchSize)
          ]
      )
