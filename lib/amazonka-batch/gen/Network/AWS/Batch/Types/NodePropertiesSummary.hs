{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodePropertiesSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodePropertiesSummary where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the properties of a node that is associated with a multi-node parallel job.
--
--
--
-- /See:/ 'nodePropertiesSummary' smart constructor.
data NodePropertiesSummary = NodePropertiesSummary'
  { _npsNumNodes ::
      !(Maybe Int),
    _npsNodeIndex :: !(Maybe Int),
    _npsIsMainNode :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodePropertiesSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npsNumNodes' - The number of nodes associated with a multi-node parallel job.
--
-- * 'npsNodeIndex' - The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
--
-- * 'npsIsMainNode' - Specifies whether the current node is the main node for a multi-node parallel job.
nodePropertiesSummary ::
  NodePropertiesSummary
nodePropertiesSummary =
  NodePropertiesSummary'
    { _npsNumNodes = Nothing,
      _npsNodeIndex = Nothing,
      _npsIsMainNode = Nothing
    }

-- | The number of nodes associated with a multi-node parallel job.
npsNumNodes :: Lens' NodePropertiesSummary (Maybe Int)
npsNumNodes = lens _npsNumNodes (\s a -> s {_npsNumNodes = a})

-- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
npsNodeIndex :: Lens' NodePropertiesSummary (Maybe Int)
npsNodeIndex = lens _npsNodeIndex (\s a -> s {_npsNodeIndex = a})

-- | Specifies whether the current node is the main node for a multi-node parallel job.
npsIsMainNode :: Lens' NodePropertiesSummary (Maybe Bool)
npsIsMainNode = lens _npsIsMainNode (\s a -> s {_npsIsMainNode = a})

instance FromJSON NodePropertiesSummary where
  parseJSON =
    withObject
      "NodePropertiesSummary"
      ( \x ->
          NodePropertiesSummary'
            <$> (x .:? "numNodes")
            <*> (x .:? "nodeIndex")
            <*> (x .:? "isMainNode")
      )

instance Hashable NodePropertiesSummary

instance NFData NodePropertiesSummary
