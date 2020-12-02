{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.Container
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Container where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types.ContainerServiceProtocol
import Network.AWS.Prelude

-- | Describes the settings of a container that will be launched, or that is launched, to an Amazon Lightsail container service.
--
--
--
-- /See:/ 'container' smart constructor.
data Container = Container'
  { _cImage :: !(Maybe Text),
    _cCommand :: !(Maybe [Text]),
    _cEnvironment :: !(Maybe (Map Text (Text))),
    _cPorts :: !(Maybe (Map Text (ContainerServiceProtocol)))
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Container' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cImage' - The name of the image used for the container. Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
--
-- * 'cCommand' - The launch command for the container.
--
-- * 'cEnvironment' - The environment variables of the container.
--
-- * 'cPorts' - The open firewall ports of the container.
container ::
  Container
container =
  Container'
    { _cImage = Nothing,
      _cCommand = Nothing,
      _cEnvironment = Nothing,
      _cPorts = Nothing
    }

-- | The name of the image used for the container. Container images sourced from your Lightsail container service, that are registered and stored on your service, start with a colon (@:@ ). For example, @:container-service-1.mystaticwebsite.1@ . Container images sourced from a public registry like Docker Hub don't start with a colon. For example, @nginx:latest@ or @nginx@ .
cImage :: Lens' Container (Maybe Text)
cImage = lens _cImage (\s a -> s {_cImage = a})

-- | The launch command for the container.
cCommand :: Lens' Container [Text]
cCommand = lens _cCommand (\s a -> s {_cCommand = a}) . _Default . _Coerce

-- | The environment variables of the container.
cEnvironment :: Lens' Container (HashMap Text (Text))
cEnvironment = lens _cEnvironment (\s a -> s {_cEnvironment = a}) . _Default . _Map

-- | The open firewall ports of the container.
cPorts :: Lens' Container (HashMap Text (ContainerServiceProtocol))
cPorts = lens _cPorts (\s a -> s {_cPorts = a}) . _Default . _Map

instance FromJSON Container where
  parseJSON =
    withObject
      "Container"
      ( \x ->
          Container'
            <$> (x .:? "image")
            <*> (x .:? "command" .!= mempty)
            <*> (x .:? "environment" .!= mempty)
            <*> (x .:? "ports" .!= mempty)
      )

instance Hashable Container

instance NFData Container

instance ToJSON Container where
  toJSON Container' {..} =
    object
      ( catMaybes
          [ ("image" .=) <$> _cImage,
            ("command" .=) <$> _cCommand,
            ("environment" .=) <$> _cEnvironment,
            ("ports" .=) <$> _cPorts
          ]
      )
