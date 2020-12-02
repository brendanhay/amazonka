{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCPUOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCPUOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The CPU options for the instance. Both the core count and threads per core must be specified in the request.
--
--
--
-- /See:/ 'launchTemplateCPUOptionsRequest' smart constructor.
data LaunchTemplateCPUOptionsRequest = LaunchTemplateCPUOptionsRequest'
  { _ltcorCoreCount ::
      !(Maybe Int),
    _ltcorThreadsPerCore ::
      !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateCPUOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcorCoreCount' - The number of CPU cores for the instance.
--
-- * 'ltcorThreadsPerCore' - The number of threads per CPU core. To disable multithreading for the instance, specify a value of 1. Otherwise, specify the default value of 2.
launchTemplateCPUOptionsRequest ::
  LaunchTemplateCPUOptionsRequest
launchTemplateCPUOptionsRequest =
  LaunchTemplateCPUOptionsRequest'
    { _ltcorCoreCount = Nothing,
      _ltcorThreadsPerCore = Nothing
    }

-- | The number of CPU cores for the instance.
ltcorCoreCount :: Lens' LaunchTemplateCPUOptionsRequest (Maybe Int)
ltcorCoreCount = lens _ltcorCoreCount (\s a -> s {_ltcorCoreCount = a})

-- | The number of threads per CPU core. To disable multithreading for the instance, specify a value of 1. Otherwise, specify the default value of 2.
ltcorThreadsPerCore :: Lens' LaunchTemplateCPUOptionsRequest (Maybe Int)
ltcorThreadsPerCore = lens _ltcorThreadsPerCore (\s a -> s {_ltcorThreadsPerCore = a})

instance Hashable LaunchTemplateCPUOptionsRequest

instance NFData LaunchTemplateCPUOptionsRequest

instance ToQuery LaunchTemplateCPUOptionsRequest where
  toQuery LaunchTemplateCPUOptionsRequest' {..} =
    mconcat
      [ "CoreCount" =: _ltcorCoreCount,
        "ThreadsPerCore" =: _ltcorThreadsPerCore
      ]
