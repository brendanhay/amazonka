{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.NodeProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.NodeProperties where

import Network.AWS.Batch.Types.NodeRangeProperty
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the node properties of a multi-node parallel job.
--
--
--
-- /See:/ 'nodeProperties' smart constructor.
data NodeProperties = NodeProperties'
  { _npNumNodes :: !Int,
    _npMainNode :: !Int,
    _npNodeRangeProperties :: ![NodeRangeProperty]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'NodeProperties' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'npNumNodes' - The number of nodes associated with a multi-node parallel job.
--
-- * 'npMainNode' - Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
--
-- * 'npNodeRangeProperties' - A list of node ranges and their properties associated with a multi-node parallel job.
nodeProperties ::
  -- | 'npNumNodes'
  Int ->
  -- | 'npMainNode'
  Int ->
  NodeProperties
nodeProperties pNumNodes_ pMainNode_ =
  NodeProperties'
    { _npNumNodes = pNumNodes_,
      _npMainNode = pMainNode_,
      _npNodeRangeProperties = mempty
    }

-- | The number of nodes associated with a multi-node parallel job.
npNumNodes :: Lens' NodeProperties Int
npNumNodes = lens _npNumNodes (\s a -> s {_npNumNodes = a})

-- | Specifies the node index for the main node of a multi-node parallel job. This node index value must be fewer than the number of nodes.
npMainNode :: Lens' NodeProperties Int
npMainNode = lens _npMainNode (\s a -> s {_npMainNode = a})

-- | A list of node ranges and their properties associated with a multi-node parallel job.
npNodeRangeProperties :: Lens' NodeProperties [NodeRangeProperty]
npNodeRangeProperties = lens _npNodeRangeProperties (\s a -> s {_npNodeRangeProperties = a}) . _Coerce

instance FromJSON NodeProperties where
  parseJSON =
    withObject
      "NodeProperties"
      ( \x ->
          NodeProperties'
            <$> (x .: "numNodes")
            <*> (x .: "mainNode")
            <*> (x .:? "nodeRangeProperties" .!= mempty)
      )

instance Hashable NodeProperties

instance NFData NodeProperties

instance ToJSON NodeProperties where
  toJSON NodeProperties' {..} =
    object
      ( catMaybes
          [ Just ("numNodes" .= _npNumNodes),
            Just ("mainNode" .= _npMainNode),
            Just ("nodeRangeProperties" .= _npNodeRangeProperties)
          ]
      )
