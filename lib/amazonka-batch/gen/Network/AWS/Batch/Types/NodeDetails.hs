{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeDetails where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the details of a multi-node parallel job node.
--
--
--
-- /See:/ 'nodeDetails' smart constructor.
data NodeDetails = NodeDetails'
  { _ndNodeIndex :: !(Maybe Int),
    _ndIsMainNode :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeDetails' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ndNodeIndex' - The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
--
-- * 'ndIsMainNode' - Specifies whether the current node is the main node for a multi-node parallel job.
nodeDetails ::
  NodeDetails
nodeDetails =
  NodeDetails' {_ndNodeIndex = Nothing, _ndIsMainNode = Nothing}

-- | The node index for the node. Node index numbering begins at zero. This index is also available on the node with the @AWS_BATCH_JOB_NODE_INDEX@ environment variable.
ndNodeIndex :: Lens' NodeDetails (Maybe Int)
ndNodeIndex = lens _ndNodeIndex (\s a -> s {_ndNodeIndex = a})

-- | Specifies whether the current node is the main node for a multi-node parallel job.
ndIsMainNode :: Lens' NodeDetails (Maybe Bool)
ndIsMainNode = lens _ndIsMainNode (\s a -> s {_ndIsMainNode = a})

instance FromJSON NodeDetails where
  parseJSON =
    withObject
      "NodeDetails"
      ( \x ->
          NodeDetails' <$> (x .:? "nodeIndex") <*> (x .:? "isMainNode")
      )

instance Hashable NodeDetails

instance NFData NodeDetails
