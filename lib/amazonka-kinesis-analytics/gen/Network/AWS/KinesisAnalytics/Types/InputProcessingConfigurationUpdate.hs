{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationUpdate where

import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes updates to an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputProcessingConfiguration.html InputProcessingConfiguration> .
--
--
--
-- /See:/ 'inputProcessingConfigurationUpdate' smart constructor.
newtype InputProcessingConfigurationUpdate = InputProcessingConfigurationUpdate'
  { _ipcuInputLambdaProcessorUpdate ::
      InputLambdaProcessorUpdate
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InputProcessingConfigurationUpdate' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipcuInputLambdaProcessorUpdate' - Provides update information for an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
inputProcessingConfigurationUpdate ::
  -- | 'ipcuInputLambdaProcessorUpdate'
  InputLambdaProcessorUpdate ->
  InputProcessingConfigurationUpdate
inputProcessingConfigurationUpdate pInputLambdaProcessorUpdate_ =
  InputProcessingConfigurationUpdate'
    { _ipcuInputLambdaProcessorUpdate =
        pInputLambdaProcessorUpdate_
    }

-- | Provides update information for an <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> .
ipcuInputLambdaProcessorUpdate :: Lens' InputProcessingConfigurationUpdate InputLambdaProcessorUpdate
ipcuInputLambdaProcessorUpdate = lens _ipcuInputLambdaProcessorUpdate (\s a -> s {_ipcuInputLambdaProcessorUpdate = a})

instance Hashable InputProcessingConfigurationUpdate

instance NFData InputProcessingConfigurationUpdate

instance ToJSON InputProcessingConfigurationUpdate where
  toJSON InputProcessingConfigurationUpdate' {..} =
    object
      ( catMaybes
          [ Just
              ("InputLambdaProcessorUpdate" .= _ipcuInputLambdaProcessorUpdate)
          ]
      )
