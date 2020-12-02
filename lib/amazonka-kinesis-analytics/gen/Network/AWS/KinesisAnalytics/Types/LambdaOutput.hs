{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutput where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf.
--
--
--
-- /See:/ 'lambdaOutput' smart constructor.
data LambdaOutput = LambdaOutput'
  { _loResourceARN :: !Text,
    _loRoleARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaOutput' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'loResourceARN' - Amazon Resource Name (ARN) of the destination Lambda function to write to.
--
-- * 'loRoleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
lambdaOutput ::
  -- | 'loResourceARN'
  Text ->
  -- | 'loRoleARN'
  Text ->
  LambdaOutput
lambdaOutput pResourceARN_ pRoleARN_ =
  LambdaOutput'
    { _loResourceARN = pResourceARN_,
      _loRoleARN = pRoleARN_
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function to write to.
loResourceARN :: Lens' LambdaOutput Text
loResourceARN = lens _loResourceARN (\s a -> s {_loResourceARN = a})

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
loRoleARN :: Lens' LambdaOutput Text
loRoleARN = lens _loRoleARN (\s a -> s {_loRoleARN = a})

instance Hashable LambdaOutput

instance NFData LambdaOutput

instance ToJSON LambdaOutput where
  toJSON LambdaOutput' {..} =
    object
      ( catMaybes
          [ Just ("ResourceARN" .= _loResourceARN),
            Just ("RoleARN" .= _loRoleARN)
          ]
      )
