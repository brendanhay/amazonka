{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ElasticGpuHealth
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ElasticGpuHealth where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ElasticGpuStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the status of an Elastic Graphics accelerator.
--
--
--
-- /See:/ 'elasticGpuHealth' smart constructor.
newtype ElasticGpuHealth = ElasticGpuHealth'
  { _eghStatus ::
      Maybe ElasticGpuStatus
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ElasticGpuHealth' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eghStatus' - The health status.
elasticGpuHealth ::
  ElasticGpuHealth
elasticGpuHealth = ElasticGpuHealth' {_eghStatus = Nothing}

-- | The health status.
eghStatus :: Lens' ElasticGpuHealth (Maybe ElasticGpuStatus)
eghStatus = lens _eghStatus (\s a -> s {_eghStatus = a})

instance FromXML ElasticGpuHealth where
  parseXML x = ElasticGpuHealth' <$> (x .@? "status")

instance Hashable ElasticGpuHealth

instance NFData ElasticGpuHealth
