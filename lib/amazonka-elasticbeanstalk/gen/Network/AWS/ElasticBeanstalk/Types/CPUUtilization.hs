{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.CPUUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.CPUUtilization where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | CPU utilization metrics for an instance.
--
--
--
-- /See:/ 'cpuUtilization' smart constructor.
data CPUUtilization = CPUUtilization'
  { _cuSoftIRQ ::
      !(Maybe Double),
    _cuIdle :: !(Maybe Double),
    _cuIRQ :: !(Maybe Double),
    _cuSystem :: !(Maybe Double),
    _cuPrivileged :: !(Maybe Double),
    _cuUser :: !(Maybe Double),
    _cuIOWait :: !(Maybe Double),
    _cuNice :: !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CPUUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cuSoftIRQ' - Available on Linux environments only. Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
--
-- * 'cuIdle' - Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
--
-- * 'cuIRQ' - Available on Linux environments only. Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
--
-- * 'cuSystem' - Available on Linux environments only. Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
--
-- * 'cuPrivileged' - Available on Windows environments only. Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
--
-- * 'cuUser' - Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
--
-- * 'cuIOWait' - Available on Linux environments only. Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
--
-- * 'cuNice' - Available on Linux environments only. Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
cpuUtilization ::
  CPUUtilization
cpuUtilization =
  CPUUtilization'
    { _cuSoftIRQ = Nothing,
      _cuIdle = Nothing,
      _cuIRQ = Nothing,
      _cuSystem = Nothing,
      _cuPrivileged = Nothing,
      _cuUser = Nothing,
      _cuIOWait = Nothing,
      _cuNice = Nothing
    }

-- | Available on Linux environments only. Percentage of time that the CPU has spent in the @SoftIRQ@ state over the last 10 seconds.
cuSoftIRQ :: Lens' CPUUtilization (Maybe Double)
cuSoftIRQ = lens _cuSoftIRQ (\s a -> s {_cuSoftIRQ = a})

-- | Percentage of time that the CPU has spent in the @Idle@ state over the last 10 seconds.
cuIdle :: Lens' CPUUtilization (Maybe Double)
cuIdle = lens _cuIdle (\s a -> s {_cuIdle = a})

-- | Available on Linux environments only. Percentage of time that the CPU has spent in the @IRQ@ state over the last 10 seconds.
cuIRQ :: Lens' CPUUtilization (Maybe Double)
cuIRQ = lens _cuIRQ (\s a -> s {_cuIRQ = a})

-- | Available on Linux environments only. Percentage of time that the CPU has spent in the @System@ state over the last 10 seconds.
cuSystem :: Lens' CPUUtilization (Maybe Double)
cuSystem = lens _cuSystem (\s a -> s {_cuSystem = a})

-- | Available on Windows environments only. Percentage of time that the CPU has spent in the @Privileged@ state over the last 10 seconds.
cuPrivileged :: Lens' CPUUtilization (Maybe Double)
cuPrivileged = lens _cuPrivileged (\s a -> s {_cuPrivileged = a})

-- | Percentage of time that the CPU has spent in the @User@ state over the last 10 seconds.
cuUser :: Lens' CPUUtilization (Maybe Double)
cuUser = lens _cuUser (\s a -> s {_cuUser = a})

-- | Available on Linux environments only. Percentage of time that the CPU has spent in the @I/O Wait@ state over the last 10 seconds.
cuIOWait :: Lens' CPUUtilization (Maybe Double)
cuIOWait = lens _cuIOWait (\s a -> s {_cuIOWait = a})

-- | Available on Linux environments only. Percentage of time that the CPU has spent in the @Nice@ state over the last 10 seconds.
cuNice :: Lens' CPUUtilization (Maybe Double)
cuNice = lens _cuNice (\s a -> s {_cuNice = a})

instance FromXML CPUUtilization where
  parseXML x =
    CPUUtilization'
      <$> (x .@? "SoftIRQ")
      <*> (x .@? "Idle")
      <*> (x .@? "IRQ")
      <*> (x .@? "System")
      <*> (x .@? "Privileged")
      <*> (x .@? "User")
      <*> (x .@? "IOWait")
      <*> (x .@? "Nice")

instance Hashable CPUUtilization

instance NFData CPUUtilization
