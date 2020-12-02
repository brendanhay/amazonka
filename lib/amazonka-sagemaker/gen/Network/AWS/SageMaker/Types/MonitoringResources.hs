{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.MonitoringResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.MonitoringResources where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.MonitoringClusterConfig

-- | Identifies the resources to deploy for a monitoring job.
--
--
--
-- /See:/ 'monitoringResources' smart constructor.
newtype MonitoringResources = MonitoringResources'
  { _mrClusterConfig ::
      MonitoringClusterConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MonitoringResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrClusterConfig' - The configuration for the cluster resources used to run the processing job.
monitoringResources ::
  -- | 'mrClusterConfig'
  MonitoringClusterConfig ->
  MonitoringResources
monitoringResources pClusterConfig_ =
  MonitoringResources' {_mrClusterConfig = pClusterConfig_}

-- | The configuration for the cluster resources used to run the processing job.
mrClusterConfig :: Lens' MonitoringResources MonitoringClusterConfig
mrClusterConfig = lens _mrClusterConfig (\s a -> s {_mrClusterConfig = a})

instance FromJSON MonitoringResources where
  parseJSON =
    withObject
      "MonitoringResources"
      (\x -> MonitoringResources' <$> (x .: "ClusterConfig"))

instance Hashable MonitoringResources

instance NFData MonitoringResources

instance ToJSON MonitoringResources where
  toJSON MonitoringResources' {..} =
    object (catMaybes [Just ("ClusterConfig" .= _mrClusterConfig)])
