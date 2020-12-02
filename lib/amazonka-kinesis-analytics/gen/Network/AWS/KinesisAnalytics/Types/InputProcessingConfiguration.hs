{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfiguration where

import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides a description of a processor that is used to preprocess the records in the stream before being processed by your application code. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
--
--
--
-- /See:/ 'inputProcessingConfiguration' smart constructor.
newtype InputProcessingConfiguration = InputProcessingConfiguration'
  { _ipcInputLambdaProcessor ::
      InputLambdaProcessor
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputProcessingConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipcInputLambdaProcessor' - The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream before being processed by your application code.
inputProcessingConfiguration ::
  -- | 'ipcInputLambdaProcessor'
  InputLambdaProcessor ->
  InputProcessingConfiguration
inputProcessingConfiguration pInputLambdaProcessor_ =
  InputProcessingConfiguration'
    { _ipcInputLambdaProcessor =
        pInputLambdaProcessor_
    }

-- | The <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream before being processed by your application code.
ipcInputLambdaProcessor :: Lens' InputProcessingConfiguration InputLambdaProcessor
ipcInputLambdaProcessor = lens _ipcInputLambdaProcessor (\s a -> s {_ipcInputLambdaProcessor = a})

instance Hashable InputProcessingConfiguration

instance NFData InputProcessingConfiguration

instance ToJSON InputProcessingConfiguration where
  toJSON InputProcessingConfiguration' {..} =
    object
      ( catMaybes
          [Just ("InputLambdaProcessor" .= _ipcInputLambdaProcessor)]
      )
