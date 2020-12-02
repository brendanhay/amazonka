{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.Types.ResourceUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CostExplorer.Types.ResourceUtilization where

import Network.AWS.CostExplorer.Types.EC2ResourceUtilization
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Resource utilization of current resource.
--
--
--
-- /See:/ 'resourceUtilization' smart constructor.
newtype ResourceUtilization = ResourceUtilization'
  { _ruEC2ResourceUtilization ::
      Maybe EC2ResourceUtilization
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ResourceUtilization' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ruEC2ResourceUtilization' - Utilization of current Amazon EC2 instance.
resourceUtilization ::
  ResourceUtilization
resourceUtilization =
  ResourceUtilization' {_ruEC2ResourceUtilization = Nothing}

-- | Utilization of current Amazon EC2 instance.
ruEC2ResourceUtilization :: Lens' ResourceUtilization (Maybe EC2ResourceUtilization)
ruEC2ResourceUtilization = lens _ruEC2ResourceUtilization (\s a -> s {_ruEC2ResourceUtilization = a})

instance FromJSON ResourceUtilization where
  parseJSON =
    withObject
      "ResourceUtilization"
      (\x -> ResourceUtilization' <$> (x .:? "EC2ResourceUtilization"))

instance Hashable ResourceUtilization

instance NFData ResourceUtilization
