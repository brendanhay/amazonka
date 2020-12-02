{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.EC2ResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.EC2ResourceUtilization where

import Network.AWS.CostExplorer.Types.EBSResourceUtilization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Utilization metrics of the instance.
--
--
--
-- /See:/ 'ec2ResourceUtilization' smart constructor.
data EC2ResourceUtilization = EC2ResourceUtilization'
  { _eruMaxCPUUtilizationPercentage ::
      !(Maybe Text),
    _eruEBSResourceUtilization ::
      !(Maybe EBSResourceUtilization),
    _eruMaxStorageUtilizationPercentage ::
      !(Maybe Text),
    _eruMaxMemoryUtilizationPercentage ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'EC2ResourceUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eruMaxCPUUtilizationPercentage' - Maximum observed or expected CPU utilization of the instance.
--
-- * 'eruEBSResourceUtilization' - The EBS field that contains a list of EBS metrics associated with the current instance.
--
-- * 'eruMaxStorageUtilizationPercentage' - Maximum observed or expected storage utilization of the instance (does not measure EBS storage).
--
-- * 'eruMaxMemoryUtilizationPercentage' - Maximum observed or expected memory utilization of the instance.
ec2ResourceUtilization ::
  EC2ResourceUtilization
ec2ResourceUtilization =
  EC2ResourceUtilization'
    { _eruMaxCPUUtilizationPercentage =
        Nothing,
      _eruEBSResourceUtilization = Nothing,
      _eruMaxStorageUtilizationPercentage = Nothing,
      _eruMaxMemoryUtilizationPercentage = Nothing
    }

-- | Maximum observed or expected CPU utilization of the instance.
eruMaxCPUUtilizationPercentage :: Lens' EC2ResourceUtilization (Maybe Text)
eruMaxCPUUtilizationPercentage = lens _eruMaxCPUUtilizationPercentage (\s a -> s {_eruMaxCPUUtilizationPercentage = a})

-- | The EBS field that contains a list of EBS metrics associated with the current instance.
eruEBSResourceUtilization :: Lens' EC2ResourceUtilization (Maybe EBSResourceUtilization)
eruEBSResourceUtilization = lens _eruEBSResourceUtilization (\s a -> s {_eruEBSResourceUtilization = a})

-- | Maximum observed or expected storage utilization of the instance (does not measure EBS storage).
eruMaxStorageUtilizationPercentage :: Lens' EC2ResourceUtilization (Maybe Text)
eruMaxStorageUtilizationPercentage = lens _eruMaxStorageUtilizationPercentage (\s a -> s {_eruMaxStorageUtilizationPercentage = a})

-- | Maximum observed or expected memory utilization of the instance.
eruMaxMemoryUtilizationPercentage :: Lens' EC2ResourceUtilization (Maybe Text)
eruMaxMemoryUtilizationPercentage = lens _eruMaxMemoryUtilizationPercentage (\s a -> s {_eruMaxMemoryUtilizationPercentage = a})

instance FromJSON EC2ResourceUtilization where
  parseJSON =
    withObject
      "EC2ResourceUtilization"
      ( \x ->
          EC2ResourceUtilization'
            <$> (x .:? "MaxCpuUtilizationPercentage")
            <*> (x .:? "EBSResourceUtilization")
            <*> (x .:? "MaxStorageUtilizationPercentage")
            <*> (x .:? "MaxMemoryUtilizationPercentage")
      )

instance Hashable EC2ResourceUtilization

instance NFData EC2ResourceUtilization
