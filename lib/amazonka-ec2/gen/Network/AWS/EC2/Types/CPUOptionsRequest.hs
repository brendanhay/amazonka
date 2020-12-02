{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CPUOptionsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CPUOptionsRequest where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The CPU options for the instance. Both the core count and threads per core must be specified in the request.
--
--
--
-- /See:/ 'cpuOptionsRequest' smart constructor.
data CPUOptionsRequest = CPUOptionsRequest'
  { _corCoreCount ::
      !(Maybe Int),
    _corThreadsPerCore :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CPUOptionsRequest' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'corCoreCount' - The number of CPU cores for the instance.
--
-- * 'corThreadsPerCore' - The number of threads per CPU core. To disable multithreading for the instance, specify a value of @1@ . Otherwise, specify the default value of @2@ .
cpuOptionsRequest ::
  CPUOptionsRequest
cpuOptionsRequest =
  CPUOptionsRequest'
    { _corCoreCount = Nothing,
      _corThreadsPerCore = Nothing
    }

-- | The number of CPU cores for the instance.
corCoreCount :: Lens' CPUOptionsRequest (Maybe Int)
corCoreCount = lens _corCoreCount (\s a -> s {_corCoreCount = a})

-- | The number of threads per CPU core. To disable multithreading for the instance, specify a value of @1@ . Otherwise, specify the default value of @2@ .
corThreadsPerCore :: Lens' CPUOptionsRequest (Maybe Int)
corThreadsPerCore = lens _corThreadsPerCore (\s a -> s {_corThreadsPerCore = a})

instance Hashable CPUOptionsRequest

instance NFData CPUOptionsRequest

instance ToQuery CPUOptionsRequest where
  toQuery CPUOptionsRequest' {..} =
    mconcat
      [ "CoreCount" =: _corCoreCount,
        "ThreadsPerCore" =: _corThreadsPerCore
      ]
