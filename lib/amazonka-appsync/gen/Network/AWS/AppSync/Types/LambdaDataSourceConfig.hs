{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LambdaDataSourceConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LambdaDataSourceConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an AWS Lambda data source configuration.
--
--
--
-- /See:/ 'lambdaDataSourceConfig' smart constructor.
newtype LambdaDataSourceConfig = LambdaDataSourceConfig'
  { _ldscLambdaFunctionARN ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaDataSourceConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ldscLambdaFunctionARN' - The ARN for the Lambda function.
lambdaDataSourceConfig ::
  -- | 'ldscLambdaFunctionARN'
  Text ->
  LambdaDataSourceConfig
lambdaDataSourceConfig pLambdaFunctionARN_ =
  LambdaDataSourceConfig'
    { _ldscLambdaFunctionARN =
        pLambdaFunctionARN_
    }

-- | The ARN for the Lambda function.
ldscLambdaFunctionARN :: Lens' LambdaDataSourceConfig Text
ldscLambdaFunctionARN = lens _ldscLambdaFunctionARN (\s a -> s {_ldscLambdaFunctionARN = a})

instance FromJSON LambdaDataSourceConfig where
  parseJSON =
    withObject
      "LambdaDataSourceConfig"
      (\x -> LambdaDataSourceConfig' <$> (x .: "lambdaFunctionArn"))

instance Hashable LambdaDataSourceConfig

instance NFData LambdaDataSourceConfig

instance ToJSON LambdaDataSourceConfig where
  toJSON LambdaDataSourceConfig' {..} =
    object
      (catMaybes [Just ("lambdaFunctionArn" .= _ldscLambdaFunctionARN)])
