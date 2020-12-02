{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.NodeConfigurationOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.NodeConfigurationOption where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.Mode

-- | A list of node configurations.
--
--
--
-- /See:/ 'nodeConfigurationOption' smart constructor.
data NodeConfigurationOption = NodeConfigurationOption'
  { _ncoMode ::
      !(Maybe Mode),
    _ncoNumberOfNodes :: !(Maybe Int),
    _ncoNodeType :: !(Maybe Text),
    _ncoEstimatedDiskUtilizationPercent ::
      !(Maybe Double)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeConfigurationOption' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ncoMode' - The category of the node configuration recommendation.
--
-- * 'ncoNumberOfNodes' - The number of nodes.
--
-- * 'ncoNodeType' - The node type, such as, "ds2.8xlarge".
--
-- * 'ncoEstimatedDiskUtilizationPercent' - The estimated disk utilizaton percentage.
nodeConfigurationOption ::
  NodeConfigurationOption
nodeConfigurationOption =
  NodeConfigurationOption'
    { _ncoMode = Nothing,
      _ncoNumberOfNodes = Nothing,
      _ncoNodeType = Nothing,
      _ncoEstimatedDiskUtilizationPercent = Nothing
    }

-- | The category of the node configuration recommendation.
ncoMode :: Lens' NodeConfigurationOption (Maybe Mode)
ncoMode = lens _ncoMode (\s a -> s {_ncoMode = a})

-- | The number of nodes.
ncoNumberOfNodes :: Lens' NodeConfigurationOption (Maybe Int)
ncoNumberOfNodes = lens _ncoNumberOfNodes (\s a -> s {_ncoNumberOfNodes = a})

-- | The node type, such as, "ds2.8xlarge".
ncoNodeType :: Lens' NodeConfigurationOption (Maybe Text)
ncoNodeType = lens _ncoNodeType (\s a -> s {_ncoNodeType = a})

-- | The estimated disk utilizaton percentage.
ncoEstimatedDiskUtilizationPercent :: Lens' NodeConfigurationOption (Maybe Double)
ncoEstimatedDiskUtilizationPercent = lens _ncoEstimatedDiskUtilizationPercent (\s a -> s {_ncoEstimatedDiskUtilizationPercent = a})

instance FromXML NodeConfigurationOption where
  parseXML x =
    NodeConfigurationOption'
      <$> (x .@? "Mode")
      <*> (x .@? "NumberOfNodes")
      <*> (x .@? "NodeType")
      <*> (x .@? "EstimatedDiskUtilizationPercent")

instance Hashable NodeConfigurationOption

instance NFData NodeConfigurationOption
