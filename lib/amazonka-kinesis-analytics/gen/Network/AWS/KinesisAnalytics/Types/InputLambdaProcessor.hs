{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object that contains the Amazon Resource Name (ARN) of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function.
--
--
--
-- /See:/ 'inputLambdaProcessor' smart constructor.
data InputLambdaProcessor = InputLambdaProcessor'
  { _ilpResourceARN ::
      !Text,
    _ilpRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLambdaProcessor' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilpResourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
--
-- * 'ilpRoleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
inputLambdaProcessor ::
  -- | 'ilpResourceARN'
  Text ->
  -- | 'ilpRoleARN'
  Text ->
  InputLambdaProcessor
inputLambdaProcessor pResourceARN_ pRoleARN_ =
  InputLambdaProcessor'
    { _ilpResourceARN = pResourceARN_,
      _ilpRoleARN = pRoleARN_
    }

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
ilpResourceARN :: Lens' InputLambdaProcessor Text
ilpResourceARN = lens _ilpResourceARN (\s a -> s {_ilpResourceARN = a})

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
ilpRoleARN :: Lens' InputLambdaProcessor Text
ilpRoleARN = lens _ilpRoleARN (\s a -> s {_ilpRoleARN = a})

instance Hashable InputLambdaProcessor

instance NFData InputLambdaProcessor

instance ToJSON InputLambdaProcessor where
  toJSON InputLambdaProcessor' {..} =
    object
      ( catMaybes
          [ Just ("ResourceARN" .= _ilpResourceARN),
            Just ("RoleARN" .= _ilpRoleARN)
          ]
      )
