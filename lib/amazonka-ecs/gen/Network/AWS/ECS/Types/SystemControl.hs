{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.SystemControl
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.SystemControl where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | A list of namespaced kernel parameters to set in the container. This parameter maps to @Sysctls@ in the <https://docs.docker.com/engine/api/v1.35/#operation/ContainerCreate Create a container> section of the <https://docs.docker.com/engine/api/v1.35/ Docker Remote API> and the @--sysctl@ option to <https://docs.docker.com/engine/reference/run/#security-configuration docker run> .
--
--
-- It is not recommended that you specify network-related @systemControls@ parameters for multiple containers in a single task that also uses either the @awsvpc@ or @host@ network mode for the following reasons:
--
--     * For tasks that use the @awsvpc@ network mode, if you set @systemControls@ for any container, it applies to all containers in the task. If you set different @systemControls@ for multiple containers in a single task, the container that is started last determines which @systemControls@ take effect.
--
--     * For tasks that use the @host@ network mode, the @systemControls@ parameter applies to the container instance's kernel parameter as well as that of all containers of any tasks running on that container instance.
--
--
--
--
-- /See:/ 'systemControl' smart constructor.
data SystemControl = SystemControl'
  { _scValue :: !(Maybe Text),
    _scNamespace :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SystemControl' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scValue' - The value for the namespaced kernel parameter specified in @namespace@ .
--
-- * 'scNamespace' - The namespaced kernel parameter for which to set a @value@ .
systemControl ::
  SystemControl
systemControl =
  SystemControl' {_scValue = Nothing, _scNamespace = Nothing}

-- | The value for the namespaced kernel parameter specified in @namespace@ .
scValue :: Lens' SystemControl (Maybe Text)
scValue = lens _scValue (\s a -> s {_scValue = a})

-- | The namespaced kernel parameter for which to set a @value@ .
scNamespace :: Lens' SystemControl (Maybe Text)
scNamespace = lens _scNamespace (\s a -> s {_scNamespace = a})

instance FromJSON SystemControl where
  parseJSON =
    withObject
      "SystemControl"
      (\x -> SystemControl' <$> (x .:? "value") <*> (x .:? "namespace"))

instance Hashable SystemControl

instance NFData SystemControl

instance ToJSON SystemControl where
  toJSON SystemControl' {..} =
    object
      ( catMaybes
          [("value" .=) <$> _scValue, ("namespace" .=) <$> _scNamespace]
      )
