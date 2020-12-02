{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains the Amazon Resource Name (ARN) of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.
--
--
--
-- /See:/ 'inputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { _ilpdResourceARN ::
      !(Maybe Text),
    _ilpdRoleARN ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLambdaProcessorDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilpdResourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
--
-- * 'ilpdRoleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessorDescription ::
  InputLambdaProcessorDescription
inputLambdaProcessorDescription =
  InputLambdaProcessorDescription'
    { _ilpdResourceARN = Nothing,
      _ilpdRoleARN = Nothing
    }

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
ilpdResourceARN :: Lens' InputLambdaProcessorDescription (Maybe Text)
ilpdResourceARN = lens _ilpdResourceARN (\s a -> s {_ilpdResourceARN = a})

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
ilpdRoleARN :: Lens' InputLambdaProcessorDescription (Maybe Text)
ilpdRoleARN = lens _ilpdRoleARN (\s a -> s {_ilpdRoleARN = a})

instance FromJSON InputLambdaProcessorDescription where
  parseJSON =
    withObject
      "InputLambdaProcessorDescription"
      ( \x ->
          InputLambdaProcessorDescription'
            <$> (x .:? "ResourceARN") <*> (x .:? "RoleARN")
      )

instance Hashable InputLambdaProcessorDescription

instance NFData InputLambdaProcessorDescription
