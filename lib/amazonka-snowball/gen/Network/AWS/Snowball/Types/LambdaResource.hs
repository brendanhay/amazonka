{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.LambdaResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.LambdaResource where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Snowball.Types.EventTriggerDefinition

-- | Identifies
--
--
--
-- /See:/ 'lambdaResource' smart constructor.
data LambdaResource = LambdaResource'
  { _lrEventTriggers ::
      !(Maybe [EventTriggerDefinition]),
    _lrLambdaARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaResource' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lrEventTriggers' - The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
--
-- * 'lrLambdaARN' - An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
lambdaResource ::
  LambdaResource
lambdaResource =
  LambdaResource'
    { _lrEventTriggers = Nothing,
      _lrLambdaARN = Nothing
    }

-- | The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
lrEventTriggers :: Lens' LambdaResource [EventTriggerDefinition]
lrEventTriggers = lens _lrEventTriggers (\s a -> s {_lrEventTriggers = a}) . _Default . _Coerce

-- | An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
lrLambdaARN :: Lens' LambdaResource (Maybe Text)
lrLambdaARN = lens _lrLambdaARN (\s a -> s {_lrLambdaARN = a})

instance FromJSON LambdaResource where
  parseJSON =
    withObject
      "LambdaResource"
      ( \x ->
          LambdaResource'
            <$> (x .:? "EventTriggers" .!= mempty) <*> (x .:? "LambdaArn")
      )

instance Hashable LambdaResource

instance NFData LambdaResource

instance ToJSON LambdaResource where
  toJSON LambdaResource' {..} =
    object
      ( catMaybes
          [ ("EventTriggers" .=) <$> _lrEventTriggers,
            ("LambdaArn" .=) <$> _lrLambdaARN
          ]
      )
