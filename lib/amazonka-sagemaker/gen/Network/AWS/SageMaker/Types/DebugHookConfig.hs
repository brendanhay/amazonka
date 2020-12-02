{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.DebugHookConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.DebugHookConfig where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.CollectionConfiguration

-- | Configuration information for the debug hook parameters, collection configuration, and storage paths.
--
--
--
-- /See:/ 'debugHookConfig' smart constructor.
data DebugHookConfig = DebugHookConfig'
  { _dhcLocalPath ::
      !(Maybe Text),
    _dhcCollectionConfigurations ::
      !(Maybe [CollectionConfiguration]),
    _dhcHookParameters :: !(Maybe (Map Text (Text))),
    _dhcS3OutputPath :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DebugHookConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dhcLocalPath' - Path to local storage location for tensors. Defaults to @/opt/ml/output/tensors/@ .
--
-- * 'dhcCollectionConfigurations' - Configuration information for tensor collections.
--
-- * 'dhcHookParameters' - Configuration information for the debug hook parameters.
--
-- * 'dhcS3OutputPath' - Path to Amazon S3 storage location for tensors.
debugHookConfig ::
  -- | 'dhcS3OutputPath'
  Text ->
  DebugHookConfig
debugHookConfig pS3OutputPath_ =
  DebugHookConfig'
    { _dhcLocalPath = Nothing,
      _dhcCollectionConfigurations = Nothing,
      _dhcHookParameters = Nothing,
      _dhcS3OutputPath = pS3OutputPath_
    }

-- | Path to local storage location for tensors. Defaults to @/opt/ml/output/tensors/@ .
dhcLocalPath :: Lens' DebugHookConfig (Maybe Text)
dhcLocalPath = lens _dhcLocalPath (\s a -> s {_dhcLocalPath = a})

-- | Configuration information for tensor collections.
dhcCollectionConfigurations :: Lens' DebugHookConfig [CollectionConfiguration]
dhcCollectionConfigurations = lens _dhcCollectionConfigurations (\s a -> s {_dhcCollectionConfigurations = a}) . _Default . _Coerce

-- | Configuration information for the debug hook parameters.
dhcHookParameters :: Lens' DebugHookConfig (HashMap Text (Text))
dhcHookParameters = lens _dhcHookParameters (\s a -> s {_dhcHookParameters = a}) . _Default . _Map

-- | Path to Amazon S3 storage location for tensors.
dhcS3OutputPath :: Lens' DebugHookConfig Text
dhcS3OutputPath = lens _dhcS3OutputPath (\s a -> s {_dhcS3OutputPath = a})

instance FromJSON DebugHookConfig where
  parseJSON =
    withObject
      "DebugHookConfig"
      ( \x ->
          DebugHookConfig'
            <$> (x .:? "LocalPath")
            <*> (x .:? "CollectionConfigurations" .!= mempty)
            <*> (x .:? "HookParameters" .!= mempty)
            <*> (x .: "S3OutputPath")
      )

instance Hashable DebugHookConfig

instance NFData DebugHookConfig

instance ToJSON DebugHookConfig where
  toJSON DebugHookConfig' {..} =
    object
      ( catMaybes
          [ ("LocalPath" .=) <$> _dhcLocalPath,
            ("CollectionConfigurations" .=) <$> _dhcCollectionConfigurations,
            ("HookParameters" .=) <$> _dhcHookParameters,
            Just ("S3OutputPath" .= _dhcS3OutputPath)
          ]
      )
