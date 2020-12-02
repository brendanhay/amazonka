{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.FunctionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.FunctionConfiguration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A function is a reusable entity. Multiple functions can be used to compose the resolver logic.
--
--
--
-- /See:/ 'functionConfiguration' smart constructor.
data FunctionConfiguration = FunctionConfiguration'
  { _fcFunctionARN ::
      !(Maybe Text),
    _fcDataSourceName :: !(Maybe Text),
    _fcRequestMappingTemplate :: !(Maybe Text),
    _fcName :: !(Maybe Text),
    _fcFunctionId :: !(Maybe Text),
    _fcResponseMappingTemplate :: !(Maybe Text),
    _fcFunctionVersion :: !(Maybe Text),
    _fcDescription :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FunctionConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fcFunctionARN' - The ARN of the @Function@ object.
--
-- * 'fcDataSourceName' - The name of the @DataSource@ .
--
-- * 'fcRequestMappingTemplate' - The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
--
-- * 'fcName' - The name of the @Function@ object.
--
-- * 'fcFunctionId' - A unique ID representing the @Function@ object.
--
-- * 'fcResponseMappingTemplate' - The @Function@ response mapping template.
--
-- * 'fcFunctionVersion' - The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
--
-- * 'fcDescription' - The @Function@ description.
functionConfiguration ::
  FunctionConfiguration
functionConfiguration =
  FunctionConfiguration'
    { _fcFunctionARN = Nothing,
      _fcDataSourceName = Nothing,
      _fcRequestMappingTemplate = Nothing,
      _fcName = Nothing,
      _fcFunctionId = Nothing,
      _fcResponseMappingTemplate = Nothing,
      _fcFunctionVersion = Nothing,
      _fcDescription = Nothing
    }

-- | The ARN of the @Function@ object.
fcFunctionARN :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionARN = lens _fcFunctionARN (\s a -> s {_fcFunctionARN = a})

-- | The name of the @DataSource@ .
fcDataSourceName :: Lens' FunctionConfiguration (Maybe Text)
fcDataSourceName = lens _fcDataSourceName (\s a -> s {_fcDataSourceName = a})

-- | The @Function@ request mapping template. Functions support only the 2018-05-29 version of the request mapping template.
fcRequestMappingTemplate :: Lens' FunctionConfiguration (Maybe Text)
fcRequestMappingTemplate = lens _fcRequestMappingTemplate (\s a -> s {_fcRequestMappingTemplate = a})

-- | The name of the @Function@ object.
fcName :: Lens' FunctionConfiguration (Maybe Text)
fcName = lens _fcName (\s a -> s {_fcName = a})

-- | A unique ID representing the @Function@ object.
fcFunctionId :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionId = lens _fcFunctionId (\s a -> s {_fcFunctionId = a})

-- | The @Function@ response mapping template.
fcResponseMappingTemplate :: Lens' FunctionConfiguration (Maybe Text)
fcResponseMappingTemplate = lens _fcResponseMappingTemplate (\s a -> s {_fcResponseMappingTemplate = a})

-- | The version of the request mapping template. Currently only the 2018-05-29 version of the template is supported.
fcFunctionVersion :: Lens' FunctionConfiguration (Maybe Text)
fcFunctionVersion = lens _fcFunctionVersion (\s a -> s {_fcFunctionVersion = a})

-- | The @Function@ description.
fcDescription :: Lens' FunctionConfiguration (Maybe Text)
fcDescription = lens _fcDescription (\s a -> s {_fcDescription = a})

instance FromJSON FunctionConfiguration where
  parseJSON =
    withObject
      "FunctionConfiguration"
      ( \x ->
          FunctionConfiguration'
            <$> (x .:? "functionArn")
            <*> (x .:? "dataSourceName")
            <*> (x .:? "requestMappingTemplate")
            <*> (x .:? "name")
            <*> (x .:? "functionId")
            <*> (x .:? "responseMappingTemplate")
            <*> (x .:? "functionVersion")
            <*> (x .:? "description")
      )

instance Hashable FunctionConfiguration

instance NFData FunctionConfiguration
