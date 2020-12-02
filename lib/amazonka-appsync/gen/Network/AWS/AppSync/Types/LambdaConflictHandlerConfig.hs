{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.LambdaConflictHandlerConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The @LambdaConflictHandlerConfig@ object when configuring LAMBDA as the Conflict Handler.
--
--
--
-- /See:/ 'lambdaConflictHandlerConfig' smart constructor.
newtype LambdaConflictHandlerConfig = LambdaConflictHandlerConfig'
  { _lchcLambdaConflictHandlerARN ::
      Maybe Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaConflictHandlerConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lchcLambdaConflictHandlerARN' - The Arn for the Lambda function to use as the Conflict Handler.
lambdaConflictHandlerConfig ::
  LambdaConflictHandlerConfig
lambdaConflictHandlerConfig =
  LambdaConflictHandlerConfig'
    { _lchcLambdaConflictHandlerARN =
        Nothing
    }

-- | The Arn for the Lambda function to use as the Conflict Handler.
lchcLambdaConflictHandlerARN :: Lens' LambdaConflictHandlerConfig (Maybe Text)
lchcLambdaConflictHandlerARN = lens _lchcLambdaConflictHandlerARN (\s a -> s {_lchcLambdaConflictHandlerARN = a})

instance FromJSON LambdaConflictHandlerConfig where
  parseJSON =
    withObject
      "LambdaConflictHandlerConfig"
      ( \x ->
          LambdaConflictHandlerConfig'
            <$> (x .:? "lambdaConflictHandlerArn")
      )

instance Hashable LambdaConflictHandlerConfig

instance NFData LambdaConflictHandlerConfig

instance ToJSON LambdaConflictHandlerConfig where
  toJSON LambdaConflictHandlerConfig' {..} =
    object
      ( catMaybes
          [ ("lambdaConflictHandlerArn" .=)
              <$> _lchcLambdaConflictHandlerARN
          ]
      )
