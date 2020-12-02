{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.WorkflowGraph
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.WorkflowGraph where

import Network.AWS.Glue.Types.Edge
import Network.AWS.Glue.Types.Node
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A workflow graph represents the complete workflow containing all the AWS Glue components present in the workflow and all the directed connections between them.
--
--
--
-- /See:/ 'workflowGraph' smart constructor.
data WorkflowGraph = WorkflowGraph'
  { _wgEdges :: !(Maybe [Edge]),
    _wgNodes :: !(Maybe [Node])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'WorkflowGraph' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wgEdges' - A list of all the directed connections between the nodes belonging to the workflow.
--
-- * 'wgNodes' - A list of the the AWS Glue components belong to the workflow represented as nodes.
workflowGraph ::
  WorkflowGraph
workflowGraph =
  WorkflowGraph' {_wgEdges = Nothing, _wgNodes = Nothing}

-- | A list of all the directed connections between the nodes belonging to the workflow.
wgEdges :: Lens' WorkflowGraph [Edge]
wgEdges = lens _wgEdges (\s a -> s {_wgEdges = a}) . _Default . _Coerce

-- | A list of the the AWS Glue components belong to the workflow represented as nodes.
wgNodes :: Lens' WorkflowGraph [Node]
wgNodes = lens _wgNodes (\s a -> s {_wgNodes = a}) . _Default . _Coerce

instance FromJSON WorkflowGraph where
  parseJSON =
    withObject
      "WorkflowGraph"
      ( \x ->
          WorkflowGraph'
            <$> (x .:? "Edges" .!= mempty) <*> (x .:? "Nodes" .!= mempty)
      )

instance Hashable WorkflowGraph

instance NFData WorkflowGraph
