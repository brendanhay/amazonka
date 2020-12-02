{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LambdaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LambdaAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an action to invoke a Lambda function.
--
--
--
-- /See:/ 'lambdaAction' smart constructor.
newtype LambdaAction = LambdaAction' {_laFunctionARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laFunctionARN' - The ARN of the Lambda function.
lambdaAction ::
  -- | 'laFunctionARN'
  Text ->
  LambdaAction
lambdaAction pFunctionARN_ =
  LambdaAction' {_laFunctionARN = pFunctionARN_}

-- | The ARN of the Lambda function.
laFunctionARN :: Lens' LambdaAction Text
laFunctionARN = lens _laFunctionARN (\s a -> s {_laFunctionARN = a})

instance FromJSON LambdaAction where
  parseJSON =
    withObject
      "LambdaAction"
      (\x -> LambdaAction' <$> (x .: "functionArn"))

instance Hashable LambdaAction

instance NFData LambdaAction

instance ToJSON LambdaAction where
  toJSON LambdaAction' {..} =
    object (catMaybes [Just ("functionArn" .= _laFunctionARN)])
