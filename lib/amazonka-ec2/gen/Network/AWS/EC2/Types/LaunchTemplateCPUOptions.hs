{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateCPUOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateCPUOptions where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The CPU options for the instance.
--
--
--
-- /See:/ 'launchTemplateCPUOptions' smart constructor.
data LaunchTemplateCPUOptions = LaunchTemplateCPUOptions'
  { _ltcoCoreCount ::
      !(Maybe Int),
    _ltcoThreadsPerCore :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LaunchTemplateCPUOptions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ltcoCoreCount' - The number of CPU cores for the instance.
--
-- * 'ltcoThreadsPerCore' - The number of threads per CPU core.
launchTemplateCPUOptions ::
  LaunchTemplateCPUOptions
launchTemplateCPUOptions =
  LaunchTemplateCPUOptions'
    { _ltcoCoreCount = Nothing,
      _ltcoThreadsPerCore = Nothing
    }

-- | The number of CPU cores for the instance.
ltcoCoreCount :: Lens' LaunchTemplateCPUOptions (Maybe Int)
ltcoCoreCount = lens _ltcoCoreCount (\s a -> s {_ltcoCoreCount = a})

-- | The number of threads per CPU core.
ltcoThreadsPerCore :: Lens' LaunchTemplateCPUOptions (Maybe Int)
ltcoThreadsPerCore = lens _ltcoThreadsPerCore (\s a -> s {_ltcoThreadsPerCore = a})

instance FromXML LaunchTemplateCPUOptions where
  parseXML x =
    LaunchTemplateCPUOptions'
      <$> (x .@? "coreCount") <*> (x .@? "threadsPerCore")

instance Hashable LaunchTemplateCPUOptions

instance NFData LaunchTemplateCPUOptions
