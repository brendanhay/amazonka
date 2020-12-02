{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Workflow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Workflow where

import Network.AWS.Glue.Types.WorkflowGraph
import Network.AWS.Glue.Types.WorkflowRun
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A workflow represents a flow in which AWS Glue components should be executed to complete a logical task.
--
--
--
-- /See:/ 'workflow' smart constructor.
data Workflow = Workflow'
  { _wGraph :: !(Maybe WorkflowGraph),
    _wLastModifiedOn :: !(Maybe POSIX),
    _wMaxConcurrentRuns :: !(Maybe Int),
    _wDefaultRunProperties :: !(Maybe (Map Text (Text))),
    _wName :: !(Maybe Text),
    _wLastRun :: !(Maybe WorkflowRun),
    _wDescription :: !(Maybe Text),
    _wCreatedOn :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Workflow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'wGraph' - The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
--
-- * 'wLastModifiedOn' - The date and time when the workflow was last modified.
--
-- * 'wMaxConcurrentRuns' - You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
--
-- * 'wDefaultRunProperties' - A collection of properties to be used as part of each execution of the workflow.
--
-- * 'wName' - The name of the workflow representing the flow.
--
-- * 'wLastRun' - The information about the last execution of the workflow.
--
-- * 'wDescription' - A description of the workflow.
--
-- * 'wCreatedOn' - The date and time when the workflow was created.
workflow ::
  Workflow
workflow =
  Workflow'
    { _wGraph = Nothing,
      _wLastModifiedOn = Nothing,
      _wMaxConcurrentRuns = Nothing,
      _wDefaultRunProperties = Nothing,
      _wName = Nothing,
      _wLastRun = Nothing,
      _wDescription = Nothing,
      _wCreatedOn = Nothing
    }

-- | The graph representing all the AWS Glue components that belong to the workflow as nodes and directed connections between them as edges.
wGraph :: Lens' Workflow (Maybe WorkflowGraph)
wGraph = lens _wGraph (\s a -> s {_wGraph = a})

-- | The date and time when the workflow was last modified.
wLastModifiedOn :: Lens' Workflow (Maybe UTCTime)
wLastModifiedOn = lens _wLastModifiedOn (\s a -> s {_wLastModifiedOn = a}) . mapping _Time

-- | You can use this parameter to prevent unwanted multiple updates to data, to control costs, or in some cases, to prevent exceeding the maximum number of concurrent runs of any of the component jobs. If you leave this parameter blank, there is no limit to the number of concurrent workflow runs.
wMaxConcurrentRuns :: Lens' Workflow (Maybe Int)
wMaxConcurrentRuns = lens _wMaxConcurrentRuns (\s a -> s {_wMaxConcurrentRuns = a})

-- | A collection of properties to be used as part of each execution of the workflow.
wDefaultRunProperties :: Lens' Workflow (HashMap Text (Text))
wDefaultRunProperties = lens _wDefaultRunProperties (\s a -> s {_wDefaultRunProperties = a}) . _Default . _Map

-- | The name of the workflow representing the flow.
wName :: Lens' Workflow (Maybe Text)
wName = lens _wName (\s a -> s {_wName = a})

-- | The information about the last execution of the workflow.
wLastRun :: Lens' Workflow (Maybe WorkflowRun)
wLastRun = lens _wLastRun (\s a -> s {_wLastRun = a})

-- | A description of the workflow.
wDescription :: Lens' Workflow (Maybe Text)
wDescription = lens _wDescription (\s a -> s {_wDescription = a})

-- | The date and time when the workflow was created.
wCreatedOn :: Lens' Workflow (Maybe UTCTime)
wCreatedOn = lens _wCreatedOn (\s a -> s {_wCreatedOn = a}) . mapping _Time

instance FromJSON Workflow where
  parseJSON =
    withObject
      "Workflow"
      ( \x ->
          Workflow'
            <$> (x .:? "Graph")
            <*> (x .:? "LastModifiedOn")
            <*> (x .:? "MaxConcurrentRuns")
            <*> (x .:? "DefaultRunProperties" .!= mempty)
            <*> (x .:? "Name")
            <*> (x .:? "LastRun")
            <*> (x .:? "Description")
            <*> (x .:? "CreatedOn")
      )

instance Hashable Workflow

instance NFData Workflow
