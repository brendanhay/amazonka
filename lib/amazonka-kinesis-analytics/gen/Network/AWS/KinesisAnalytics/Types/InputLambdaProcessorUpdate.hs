{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents an update to the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream.
--
--
--
-- /See:/ 'inputLambdaProcessorUpdate' smart constructor.
data InputLambdaProcessorUpdate = InputLambdaProcessorUpdate'
  { _ilpuRoleARNUpdate ::
      !(Maybe Text),
    _ilpuResourceARNUpdate ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputLambdaProcessorUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ilpuRoleARNUpdate' - The ARN of the new IAM role that is used to access the AWS Lambda function.
--
-- * 'ilpuResourceARNUpdate' - The Amazon Resource Name (ARN) of the new <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
inputLambdaProcessorUpdate ::
  InputLambdaProcessorUpdate
inputLambdaProcessorUpdate =
  InputLambdaProcessorUpdate'
    { _ilpuRoleARNUpdate = Nothing,
      _ilpuResourceARNUpdate = Nothing
    }

-- | The ARN of the new IAM role that is used to access the AWS Lambda function.
ilpuRoleARNUpdate :: Lens' InputLambdaProcessorUpdate (Maybe Text)
ilpuRoleARNUpdate = lens _ilpuRoleARNUpdate (\s a -> s {_ilpuRoleARNUpdate = a})

-- | The Amazon Resource Name (ARN) of the new <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
ilpuResourceARNUpdate :: Lens' InputLambdaProcessorUpdate (Maybe Text)
ilpuResourceARNUpdate = lens _ilpuResourceARNUpdate (\s a -> s {_ilpuResourceARNUpdate = a})

instance Hashable InputLambdaProcessorUpdate

instance NFData InputLambdaProcessorUpdate

instance ToJSON InputLambdaProcessorUpdate where
  toJSON InputLambdaProcessorUpdate' {..} =
    object
      ( catMaybes
          [ ("RoleARNUpdate" .=) <$> _ilpuRoleARNUpdate,
            ("ResourceARNUpdate" .=) <$> _ilpuResourceARNUpdate
          ]
      )
