{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | For an application output, describes the AWS Lambda function configured as its destination.
--
--
--
-- /See:/ 'lambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { _lodResourceARN ::
      !(Maybe Text),
    _lodRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaOutputDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lodResourceARN' - Amazon Resource Name (ARN) of the destination Lambda function.
--
-- * 'lodRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
lambdaOutputDescription ::
  LambdaOutputDescription
lambdaOutputDescription =
  LambdaOutputDescription'
    { _lodResourceARN = Nothing,
      _lodRoleARN = Nothing
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function.
lodResourceARN :: Lens' LambdaOutputDescription (Maybe Text)
lodResourceARN = lens _lodResourceARN (\s a -> s {_lodResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
lodRoleARN :: Lens' LambdaOutputDescription (Maybe Text)
lodRoleARN = lens _lodRoleARN (\s a -> s {_lodRoleARN = a})

instance FromJSON LambdaOutputDescription where
  parseJSON =
    withObject
      "LambdaOutputDescription"
      ( \x ->
          LambdaOutputDescription'
            <$> (x .:? "ResourceARN") <*> (x .:? "RoleARN")
      )

instance Hashable LambdaOutputDescription

instance NFData LambdaOutputDescription
