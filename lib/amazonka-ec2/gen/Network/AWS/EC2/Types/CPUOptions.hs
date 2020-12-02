{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CPUOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CPUOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The CPU options for the instance.
--
--
--
-- /See:/ 'cpuOptions' smart constructor.
data CPUOptions = CPUOptions'
  { _coCoreCount :: !(Maybe Int),
    _coThreadsPerCore :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CPUOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'coCoreCount' - The number of CPU cores for the instance.
--
-- * 'coThreadsPerCore' - The number of threads per CPU core.
cpuOptions ::
  CPUOptions
cpuOptions =
  CPUOptions' {_coCoreCount = Nothing, _coThreadsPerCore = Nothing}

-- | The number of CPU cores for the instance.
coCoreCount :: Lens' CPUOptions (Maybe Int)
coCoreCount = lens _coCoreCount (\s a -> s {_coCoreCount = a})

-- | The number of threads per CPU core.
coThreadsPerCore :: Lens' CPUOptions (Maybe Int)
coThreadsPerCore = lens _coThreadsPerCore (\s a -> s {_coThreadsPerCore = a})

instance FromXML CPUOptions where
  parseXML x =
    CPUOptions' <$> (x .@? "coreCount") <*> (x .@? "threadsPerCore")

instance Hashable CPUOptions

instance NFData CPUOptions
