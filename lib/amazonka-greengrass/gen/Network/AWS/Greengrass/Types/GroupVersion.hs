{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.Types.GroupVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Greengrass.Types.GroupVersion where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a group version.
--
-- /See:/ 'groupVersion' smart constructor.
data GroupVersion = GroupVersion'
  { _gvResourceDefinitionVersionARN ::
      !(Maybe Text),
    _gvSubscriptionDefinitionVersionARN :: !(Maybe Text),
    _gvCoreDefinitionVersionARN :: !(Maybe Text),
    _gvDeviceDefinitionVersionARN :: !(Maybe Text),
    _gvFunctionDefinitionVersionARN :: !(Maybe Text),
    _gvLoggerDefinitionVersionARN :: !(Maybe Text),
    _gvConnectorDefinitionVersionARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'GroupVersion' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'gvResourceDefinitionVersionARN' - The ARN of the resource definition version for this group.
--
-- * 'gvSubscriptionDefinitionVersionARN' - The ARN of the subscription definition version for this group.
--
-- * 'gvCoreDefinitionVersionARN' - The ARN of the core definition version for this group.
--
-- * 'gvDeviceDefinitionVersionARN' - The ARN of the device definition version for this group.
--
-- * 'gvFunctionDefinitionVersionARN' - The ARN of the function definition version for this group.
--
-- * 'gvLoggerDefinitionVersionARN' - The ARN of the logger definition version for this group.
--
-- * 'gvConnectorDefinitionVersionARN' - The ARN of the connector definition version for this group.
groupVersion ::
  GroupVersion
groupVersion =
  GroupVersion'
    { _gvResourceDefinitionVersionARN = Nothing,
      _gvSubscriptionDefinitionVersionARN = Nothing,
      _gvCoreDefinitionVersionARN = Nothing,
      _gvDeviceDefinitionVersionARN = Nothing,
      _gvFunctionDefinitionVersionARN = Nothing,
      _gvLoggerDefinitionVersionARN = Nothing,
      _gvConnectorDefinitionVersionARN = Nothing
    }

-- | The ARN of the resource definition version for this group.
gvResourceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvResourceDefinitionVersionARN = lens _gvResourceDefinitionVersionARN (\s a -> s {_gvResourceDefinitionVersionARN = a})

-- | The ARN of the subscription definition version for this group.
gvSubscriptionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvSubscriptionDefinitionVersionARN = lens _gvSubscriptionDefinitionVersionARN (\s a -> s {_gvSubscriptionDefinitionVersionARN = a})

-- | The ARN of the core definition version for this group.
gvCoreDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvCoreDefinitionVersionARN = lens _gvCoreDefinitionVersionARN (\s a -> s {_gvCoreDefinitionVersionARN = a})

-- | The ARN of the device definition version for this group.
gvDeviceDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvDeviceDefinitionVersionARN = lens _gvDeviceDefinitionVersionARN (\s a -> s {_gvDeviceDefinitionVersionARN = a})

-- | The ARN of the function definition version for this group.
gvFunctionDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvFunctionDefinitionVersionARN = lens _gvFunctionDefinitionVersionARN (\s a -> s {_gvFunctionDefinitionVersionARN = a})

-- | The ARN of the logger definition version for this group.
gvLoggerDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvLoggerDefinitionVersionARN = lens _gvLoggerDefinitionVersionARN (\s a -> s {_gvLoggerDefinitionVersionARN = a})

-- | The ARN of the connector definition version for this group.
gvConnectorDefinitionVersionARN :: Lens' GroupVersion (Maybe Text)
gvConnectorDefinitionVersionARN = lens _gvConnectorDefinitionVersionARN (\s a -> s {_gvConnectorDefinitionVersionARN = a})

instance FromJSON GroupVersion where
  parseJSON =
    withObject
      "GroupVersion"
      ( \x ->
          GroupVersion'
            <$> (x .:? "ResourceDefinitionVersionArn")
            <*> (x .:? "SubscriptionDefinitionVersionArn")
            <*> (x .:? "CoreDefinitionVersionArn")
            <*> (x .:? "DeviceDefinitionVersionArn")
            <*> (x .:? "FunctionDefinitionVersionArn")
            <*> (x .:? "LoggerDefinitionVersionArn")
            <*> (x .:? "ConnectorDefinitionVersionArn")
      )

instance Hashable GroupVersion

instance NFData GroupVersion

instance ToJSON GroupVersion where
  toJSON GroupVersion' {..} =
    object
      ( catMaybes
          [ ("ResourceDefinitionVersionArn" .=)
              <$> _gvResourceDefinitionVersionARN,
            ("SubscriptionDefinitionVersionArn" .=)
              <$> _gvSubscriptionDefinitionVersionARN,
            ("CoreDefinitionVersionArn" .=) <$> _gvCoreDefinitionVersionARN,
            ("DeviceDefinitionVersionArn" .=)
              <$> _gvDeviceDefinitionVersionARN,
            ("FunctionDefinitionVersionArn" .=)
              <$> _gvFunctionDefinitionVersionARN,
            ("LoggerDefinitionVersionArn" .=)
              <$> _gvLoggerDefinitionVersionARN,
            ("ConnectorDefinitionVersionArn" .=)
              <$> _gvConnectorDefinitionVersionARN
          ]
      )
