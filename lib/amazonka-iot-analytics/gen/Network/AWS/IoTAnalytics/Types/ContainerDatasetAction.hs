{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.ContainerDatasetAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ContainerDatasetAction where

import Network.AWS.IoTAnalytics.Types.ResourceConfiguration
import Network.AWS.IoTAnalytics.Types.Variable
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information required to run the @containerAction@ to produce dataset contents.
--
--
--
-- /See:/ 'containerDatasetAction' smart constructor.
data ContainerDatasetAction = ContainerDatasetAction'
  { _cdaVariables ::
      !(Maybe [Variable]),
    _cdaImage :: !Text,
    _cdaExecutionRoleARN :: !Text,
    _cdaResourceConfiguration ::
      !ResourceConfiguration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ContainerDatasetAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdaVariables' - The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
--
-- * 'cdaImage' - The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
--
-- * 'cdaExecutionRoleARN' - The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
--
-- * 'cdaResourceConfiguration' - Configuration of the resource that executes the @containerAction@ .
containerDatasetAction ::
  -- | 'cdaImage'
  Text ->
  -- | 'cdaExecutionRoleARN'
  Text ->
  -- | 'cdaResourceConfiguration'
  ResourceConfiguration ->
  ContainerDatasetAction
containerDatasetAction
  pImage_
  pExecutionRoleARN_
  pResourceConfiguration_ =
    ContainerDatasetAction'
      { _cdaVariables = Nothing,
        _cdaImage = pImage_,
        _cdaExecutionRoleARN = pExecutionRoleARN_,
        _cdaResourceConfiguration = pResourceConfiguration_
      }

-- | The values of variables used in the context of the execution of the containerized application (basically, parameters passed to the application). Each variable must have a name and a value given by one of @stringValue@ , @datasetContentVersionValue@ , or @outputFileUriValue@ .
cdaVariables :: Lens' ContainerDatasetAction [Variable]
cdaVariables = lens _cdaVariables (\s a -> s {_cdaVariables = a}) . _Default . _Coerce

-- | The ARN of the Docker container stored in your account. The Docker container contains an application and required support libraries and is used to generate dataset contents.
cdaImage :: Lens' ContainerDatasetAction Text
cdaImage = lens _cdaImage (\s a -> s {_cdaImage = a})

-- | The ARN of the role that gives permission to the system to access required resources to run the @containerAction@ . This includes, at minimum, permission to retrieve the dataset contents that are the input to the containerized application.
cdaExecutionRoleARN :: Lens' ContainerDatasetAction Text
cdaExecutionRoleARN = lens _cdaExecutionRoleARN (\s a -> s {_cdaExecutionRoleARN = a})

-- | Configuration of the resource that executes the @containerAction@ .
cdaResourceConfiguration :: Lens' ContainerDatasetAction ResourceConfiguration
cdaResourceConfiguration = lens _cdaResourceConfiguration (\s a -> s {_cdaResourceConfiguration = a})

instance FromJSON ContainerDatasetAction where
  parseJSON =
    withObject
      "ContainerDatasetAction"
      ( \x ->
          ContainerDatasetAction'
            <$> (x .:? "variables" .!= mempty)
            <*> (x .: "image")
            <*> (x .: "executionRoleArn")
            <*> (x .: "resourceConfiguration")
      )

instance Hashable ContainerDatasetAction

instance NFData ContainerDatasetAction

instance ToJSON ContainerDatasetAction where
  toJSON ContainerDatasetAction' {..} =
    object
      ( catMaybes
          [ ("variables" .=) <$> _cdaVariables,
            Just ("image" .= _cdaImage),
            Just ("executionRoleArn" .= _cdaExecutionRoleARN),
            Just ("resourceConfiguration" .= _cdaResourceConfiguration)
          ]
      )
