{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.VCPUInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VCPUInfo where

import Network.AWS.EC2.Internal
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes the vCPU configurations for the instance type.
--
--
--
-- /See:/ 'vCPUInfo' smart constructor.
data VCPUInfo = VCPUInfo'
  { _vciValidThreadsPerCore ::
      !(Maybe [Int]),
    _vciDefaultThreadsPerCore :: !(Maybe Int),
    _vciDefaultVCPUs :: !(Maybe Int),
    _vciDefaultCores :: !(Maybe Int),
    _vciValidCores :: !(Maybe [Int])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'VCPUInfo' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'vciValidThreadsPerCore' - The valid number of threads per core that can be configured for the instance type.
--
-- * 'vciDefaultThreadsPerCore' - The default number of threads per core for the instance type.
--
-- * 'vciDefaultVCPUs' - The default number of vCPUs for the instance type.
--
-- * 'vciDefaultCores' - The default number of cores for the instance type.
--
-- * 'vciValidCores' - The valid number of cores that can be configured for the instance type.
vCPUInfo ::
  VCPUInfo
vCPUInfo =
  VCPUInfo'
    { _vciValidThreadsPerCore = Nothing,
      _vciDefaultThreadsPerCore = Nothing,
      _vciDefaultVCPUs = Nothing,
      _vciDefaultCores = Nothing,
      _vciValidCores = Nothing
    }

-- | The valid number of threads per core that can be configured for the instance type.
vciValidThreadsPerCore :: Lens' VCPUInfo [Int]
vciValidThreadsPerCore = lens _vciValidThreadsPerCore (\s a -> s {_vciValidThreadsPerCore = a}) . _Default . _Coerce

-- | The default number of threads per core for the instance type.
vciDefaultThreadsPerCore :: Lens' VCPUInfo (Maybe Int)
vciDefaultThreadsPerCore = lens _vciDefaultThreadsPerCore (\s a -> s {_vciDefaultThreadsPerCore = a})

-- | The default number of vCPUs for the instance type.
vciDefaultVCPUs :: Lens' VCPUInfo (Maybe Int)
vciDefaultVCPUs = lens _vciDefaultVCPUs (\s a -> s {_vciDefaultVCPUs = a})

-- | The default number of cores for the instance type.
vciDefaultCores :: Lens' VCPUInfo (Maybe Int)
vciDefaultCores = lens _vciDefaultCores (\s a -> s {_vciDefaultCores = a})

-- | The valid number of cores that can be configured for the instance type.
vciValidCores :: Lens' VCPUInfo [Int]
vciValidCores = lens _vciValidCores (\s a -> s {_vciValidCores = a}) . _Default . _Coerce

instance FromXML VCPUInfo where
  parseXML x =
    VCPUInfo'
      <$> ( x .@? "validThreadsPerCore" .!@ mempty
              >>= may (parseXMLList "item")
          )
      <*> (x .@? "defaultThreadsPerCore")
      <*> (x .@? "defaultVCpus")
      <*> (x .@? "defaultCores")
      <*> (x .@? "validCores" .!@ mempty >>= may (parseXMLList "item"))

instance Hashable VCPUInfo

instance NFData VCPUInfo
