{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ProcessingResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingResources where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.ProcessingClusterConfig

-- | Identifies the resources, ML compute instances, and ML storage volumes to deploy for a processing job. In distributed training, you specify more than one instance.
--
--
--
-- /See:/ 'processingResources' smart constructor.
newtype ProcessingResources = ProcessingResources'
  { _prClusterConfig ::
      ProcessingClusterConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ProcessingResources' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'prClusterConfig' - The configuration for the resources in a cluster used to run the processing job.
processingResources ::
  -- | 'prClusterConfig'
  ProcessingClusterConfig ->
  ProcessingResources
processingResources pClusterConfig_ =
  ProcessingResources' {_prClusterConfig = pClusterConfig_}

-- | The configuration for the resources in a cluster used to run the processing job.
prClusterConfig :: Lens' ProcessingResources ProcessingClusterConfig
prClusterConfig = lens _prClusterConfig (\s a -> s {_prClusterConfig = a})

instance FromJSON ProcessingResources where
  parseJSON =
    withObject
      "ProcessingResources"
      (\x -> ProcessingResources' <$> (x .: "ClusterConfig"))

instance Hashable ProcessingResources

instance NFData ProcessingResources

instance ToJSON ProcessingResources where
  toJSON ProcessingResources' {..} =
    object (catMaybes [Just ("ClusterConfig" .= _prClusterConfig)])
