{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputProcessingConfigurationDescription where

import Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides configuration information about an input processor. Currently, the only input processor available is <https://docs.aws.amazon.com/lambda/ AWS Lambda> .
--
--
--
-- /See:/ 'inputProcessingConfigurationDescription' smart constructor.
newtype InputProcessingConfigurationDescription = InputProcessingConfigurationDescription'
  { _ipcdInputLambdaProcessorDescription ::
      Maybe
        InputLambdaProcessorDescription
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'InputProcessingConfigurationDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ipcdInputLambdaProcessorDescription' - Provides configuration information about the associated <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription> .
inputProcessingConfigurationDescription ::
  InputProcessingConfigurationDescription
inputProcessingConfigurationDescription =
  InputProcessingConfigurationDescription'
    { _ipcdInputLambdaProcessorDescription =
        Nothing
    }

-- | Provides configuration information about the associated <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessorDescription.html InputLambdaProcessorDescription> .
ipcdInputLambdaProcessorDescription :: Lens' InputProcessingConfigurationDescription (Maybe InputLambdaProcessorDescription)
ipcdInputLambdaProcessorDescription = lens _ipcdInputLambdaProcessorDescription (\s a -> s {_ipcdInputLambdaProcessorDescription = a})

instance FromJSON InputProcessingConfigurationDescription where
  parseJSON =
    withObject
      "InputProcessingConfigurationDescription"
      ( \x ->
          InputProcessingConfigurationDescription'
            <$> (x .:? "InputLambdaProcessorDescription")
      )

instance Hashable InputProcessingConfigurationDescription

instance NFData InputProcessingConfigurationDescription
