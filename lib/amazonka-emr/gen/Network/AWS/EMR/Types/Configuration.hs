{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Configuration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Configuration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An optional configuration specification to be used when provisioning cluster instances, which can include configurations for applications and software bundled with Amazon EMR. A configuration consists of a classification, properties, and optional nested configurations. A classification refers to an application-specific configuration file. Properties are the settings you want to change in that file. For more information, see <https://docs.aws.amazon.com/emr/latest/ReleaseGuide/emr-configure-apps.html Configuring Applications> .
--
--
--
-- /See:/ 'configuration' smart constructor.
data Configuration = Configuration'
  { _cConfigurations ::
      !(Maybe [Configuration]),
    _cClassification :: !(Maybe Text),
    _cProperties :: !(Maybe (Map Text (Text)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Configuration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cConfigurations' - A list of additional configurations to apply within a configuration object.
--
-- * 'cClassification' - The classification within a configuration.
--
-- * 'cProperties' - A set of properties specified within a configuration classification.
configuration ::
  Configuration
configuration =
  Configuration'
    { _cConfigurations = Nothing,
      _cClassification = Nothing,
      _cProperties = Nothing
    }

-- | A list of additional configurations to apply within a configuration object.
cConfigurations :: Lens' Configuration [Configuration]
cConfigurations = lens _cConfigurations (\s a -> s {_cConfigurations = a}) . _Default . _Coerce

-- | The classification within a configuration.
cClassification :: Lens' Configuration (Maybe Text)
cClassification = lens _cClassification (\s a -> s {_cClassification = a})

-- | A set of properties specified within a configuration classification.
cProperties :: Lens' Configuration (HashMap Text (Text))
cProperties = lens _cProperties (\s a -> s {_cProperties = a}) . _Default . _Map

instance FromJSON Configuration where
  parseJSON =
    withObject
      "Configuration"
      ( \x ->
          Configuration'
            <$> (x .:? "Configurations" .!= mempty)
            <*> (x .:? "Classification")
            <*> (x .:? "Properties" .!= mempty)
      )

instance Hashable Configuration

instance NFData Configuration

instance ToJSON Configuration where
  toJSON Configuration' {..} =
    object
      ( catMaybes
          [ ("Configurations" .=) <$> _cConfigurations,
            ("Classification" .=) <$> _cClassification,
            ("Properties" .=) <$> _cProperties
          ]
      )
