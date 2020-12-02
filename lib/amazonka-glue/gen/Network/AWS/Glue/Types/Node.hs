{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Node
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Node where

import Network.AWS.Glue.Types.CrawlerNodeDetails
import Network.AWS.Glue.Types.JobNodeDetails
import Network.AWS.Glue.Types.NodeType
import Network.AWS.Glue.Types.TriggerNodeDetails
import Network.AWS.Lens
import Network.AWS.Prelude

-- | A node represents an AWS Glue component such as a trigger, or job, etc., that is part of a workflow.
--
--
--
-- /See:/ 'node' smart constructor.
data Node = Node'
  { _nTriggerDetails :: !(Maybe TriggerNodeDetails),
    _nUniqueId :: !(Maybe Text),
    _nCrawlerDetails :: !(Maybe CrawlerNodeDetails),
    _nName :: !(Maybe Text),
    _nJobDetails :: !(Maybe JobNodeDetails),
    _nType :: !(Maybe NodeType)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Node' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'nTriggerDetails' - Details of the Trigger when the node represents a Trigger.
--
-- * 'nUniqueId' - The unique Id assigned to the node within the workflow.
--
-- * 'nCrawlerDetails' - Details of the crawler when the node represents a crawler.
--
-- * 'nName' - The name of the AWS Glue component represented by the node.
--
-- * 'nJobDetails' - Details of the Job when the node represents a Job.
--
-- * 'nType' - The type of AWS Glue component represented by the node.
node ::
  Node
node =
  Node'
    { _nTriggerDetails = Nothing,
      _nUniqueId = Nothing,
      _nCrawlerDetails = Nothing,
      _nName = Nothing,
      _nJobDetails = Nothing,
      _nType = Nothing
    }

-- | Details of the Trigger when the node represents a Trigger.
nTriggerDetails :: Lens' Node (Maybe TriggerNodeDetails)
nTriggerDetails = lens _nTriggerDetails (\s a -> s {_nTriggerDetails = a})

-- | The unique Id assigned to the node within the workflow.
nUniqueId :: Lens' Node (Maybe Text)
nUniqueId = lens _nUniqueId (\s a -> s {_nUniqueId = a})

-- | Details of the crawler when the node represents a crawler.
nCrawlerDetails :: Lens' Node (Maybe CrawlerNodeDetails)
nCrawlerDetails = lens _nCrawlerDetails (\s a -> s {_nCrawlerDetails = a})

-- | The name of the AWS Glue component represented by the node.
nName :: Lens' Node (Maybe Text)
nName = lens _nName (\s a -> s {_nName = a})

-- | Details of the Job when the node represents a Job.
nJobDetails :: Lens' Node (Maybe JobNodeDetails)
nJobDetails = lens _nJobDetails (\s a -> s {_nJobDetails = a})

-- | The type of AWS Glue component represented by the node.
nType :: Lens' Node (Maybe NodeType)
nType = lens _nType (\s a -> s {_nType = a})

instance FromJSON Node where
  parseJSON =
    withObject
      "Node"
      ( \x ->
          Node'
            <$> (x .:? "TriggerDetails")
            <*> (x .:? "UniqueId")
            <*> (x .:? "CrawlerDetails")
            <*> (x .:? "Name")
            <*> (x .:? "JobDetails")
            <*> (x .:? "Type")
      )

instance Hashable Node

instance NFData Node
