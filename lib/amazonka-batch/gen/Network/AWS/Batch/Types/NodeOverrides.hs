{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeOverrides
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeOverrides where

import Network.AWS.Batch.Types.NodePropertyOverride
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Object representing any node overrides to a job definition that is used in a 'SubmitJob' API operation.
--
--
--
-- /See:/ 'nodeOverrides' smart constructor.
data NodeOverrides = NodeOverrides'
  { _noNumNodes :: !(Maybe Int),
    _noNodePropertyOverrides :: !(Maybe [NodePropertyOverride])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeOverrides' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'noNumNodes' - The number of nodes to use with a multi-node parallel job. This value overrides the number of nodes that are specified in the job definition. To use this override:     * There must be at least one node range in your job definition that has an open upper boundary (such as @:@ or @n:@ ).     * The lower boundary of the node range specified in the job definition must be fewer than the number of nodes specified in the override.     * The main node index specified in the job definition must be fewer than the number of nodes specified in the override.
--
-- * 'noNodePropertyOverrides' - The node property overrides for the job.
nodeOverrides ::
  NodeOverrides
nodeOverrides =
  NodeOverrides'
    { _noNumNodes = Nothing,
      _noNodePropertyOverrides = Nothing
    }

-- | The number of nodes to use with a multi-node parallel job. This value overrides the number of nodes that are specified in the job definition. To use this override:     * There must be at least one node range in your job definition that has an open upper boundary (such as @:@ or @n:@ ).     * The lower boundary of the node range specified in the job definition must be fewer than the number of nodes specified in the override.     * The main node index specified in the job definition must be fewer than the number of nodes specified in the override.
noNumNodes :: Lens' NodeOverrides (Maybe Int)
noNumNodes = lens _noNumNodes (\s a -> s {_noNumNodes = a})

-- | The node property overrides for the job.
noNodePropertyOverrides :: Lens' NodeOverrides [NodePropertyOverride]
noNodePropertyOverrides = lens _noNodePropertyOverrides (\s a -> s {_noNodePropertyOverrides = a}) . _Default . _Coerce

instance Hashable NodeOverrides

instance NFData NodeOverrides

instance ToJSON NodeOverrides where
  toJSON NodeOverrides' {..} =
    object
      ( catMaybes
          [ ("numNodes" .=) <$> _noNumNodes,
            ("nodePropertyOverrides" .=) <$> _noNodePropertyOverrides
          ]
      )
