{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeRangeProperty
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeRangeProperty where

import Network.AWS.Batch.Types.ContainerProperties
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the properties of the node range for a multi-node parallel job.
--
--
--
-- /See:/ 'nodeRangeProperty' smart constructor.
data NodeRangeProperty = NodeRangeProperty'
  { _nrpContainer ::
      !(Maybe ContainerProperties),
    _nrpTargetNodes :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeRangeProperty' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nrpContainer' - The container details for the node range.
--
-- * 'nrpTargetNodes' - The range of nodes, using node index values. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range. Your accumulative node ranges must account for all nodes (0:n). You may nest node ranges, for example 0:10 and 4:5, in which case the 4:5 range properties override the 0:10 properties.
nodeRangeProperty ::
  -- | 'nrpTargetNodes'
  Text ->
  NodeRangeProperty
nodeRangeProperty pTargetNodes_ =
  NodeRangeProperty'
    { _nrpContainer = Nothing,
      _nrpTargetNodes = pTargetNodes_
    }

-- | The container details for the node range.
nrpContainer :: Lens' NodeRangeProperty (Maybe ContainerProperties)
nrpContainer = lens _nrpContainer (\s a -> s {_nrpContainer = a})

-- | The range of nodes, using node index values. A range of @0:3@ indicates nodes with index values of @0@ through @3@ . If the starting range value is omitted (@:n@ ), then @0@ is used to start the range. If the ending range value is omitted (@n:@ ), then the highest possible node index is used to end the range. Your accumulative node ranges must account for all nodes (0:n). You may nest node ranges, for example 0:10 and 4:5, in which case the 4:5 range properties override the 0:10 properties.
nrpTargetNodes :: Lens' NodeRangeProperty Text
nrpTargetNodes = lens _nrpTargetNodes (\s a -> s {_nrpTargetNodes = a})

instance FromJSON NodeRangeProperty where
  parseJSON =
    withObject
      "NodeRangeProperty"
      ( \x ->
          NodeRangeProperty'
            <$> (x .:? "container") <*> (x .: "targetNodes")
      )

instance Hashable NodeRangeProperty

instance NFData NodeRangeProperty

instance ToJSON NodeRangeProperty where
  toJSON NodeRangeProperty' {..} =
    object
      ( catMaybes
          [ ("container" .=) <$> _nrpContainer,
            Just ("targetNodes" .= _nrpTargetNodes)
          ]
      )
