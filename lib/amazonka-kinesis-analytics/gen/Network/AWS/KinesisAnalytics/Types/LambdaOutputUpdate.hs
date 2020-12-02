{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an AWS Lambda function configured as the destination.
--
--
--
-- /See:/ 'lambdaOutputUpdate' smart constructor.
data LambdaOutputUpdate = LambdaOutputUpdate'
  { _louRoleARNUpdate ::
      !(Maybe Text),
    _louResourceARNUpdate :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaOutputUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'louRoleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
--
-- * 'louResourceARNUpdate' - Amazon Resource Name (ARN) of the destination Lambda function.
lambdaOutputUpdate ::
  LambdaOutputUpdate
lambdaOutputUpdate =
  LambdaOutputUpdate'
    { _louRoleARNUpdate = Nothing,
      _louResourceARNUpdate = Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
louRoleARNUpdate :: Lens' LambdaOutputUpdate (Maybe Text)
louRoleARNUpdate = lens _louRoleARNUpdate (\s a -> s {_louRoleARNUpdate = a})

-- | Amazon Resource Name (ARN) of the destination Lambda function.
louResourceARNUpdate :: Lens' LambdaOutputUpdate (Maybe Text)
louResourceARNUpdate = lens _louResourceARNUpdate (\s a -> s {_louResourceARNUpdate = a})

instance Hashable LambdaOutputUpdate

instance NFData LambdaOutputUpdate

instance ToJSON LambdaOutputUpdate where
  toJSON LambdaOutputUpdate' {..} =
    object
      ( catMaybes
          [ ("RoleARNUpdate" .=) <$> _louRoleARNUpdate,
            ("ResourceARNUpdate" .=) <$> _louResourceARNUpdate
          ]
      )
