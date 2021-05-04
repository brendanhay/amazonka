{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Node
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Node where

import Network.AWS.Glue.Types.CrawlerNodeDetails
import Network.AWS.Glue.Types.JobNodeDetails
import Network.AWS.Glue.Types.NodeType
import Network.AWS.Glue.Types.TriggerNodeDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A node represents an AWS Glue component such as a trigger, or job, etc.,
-- that is part of a workflow.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | Details of the Job when the node represents a Job.
    jobDetails :: Prelude.Maybe JobNodeDetails,
    -- | Details of the Trigger when the node represents a Trigger.
    triggerDetails :: Prelude.Maybe TriggerNodeDetails,
    -- | The name of the AWS Glue component represented by the node.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique Id assigned to the node within the workflow.
    uniqueId :: Prelude.Maybe Prelude.Text,
    -- | Details of the crawler when the node represents a crawler.
    crawlerDetails :: Prelude.Maybe CrawlerNodeDetails,
    -- | The type of AWS Glue component represented by the node.
    type' :: Prelude.Maybe NodeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Node' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobDetails', 'node_jobDetails' - Details of the Job when the node represents a Job.
--
-- 'triggerDetails', 'node_triggerDetails' - Details of the Trigger when the node represents a Trigger.
--
-- 'name', 'node_name' - The name of the AWS Glue component represented by the node.
--
-- 'uniqueId', 'node_uniqueId' - The unique Id assigned to the node within the workflow.
--
-- 'crawlerDetails', 'node_crawlerDetails' - Details of the crawler when the node represents a crawler.
--
-- 'type'', 'node_type' - The type of AWS Glue component represented by the node.
newNode ::
  Node
newNode =
  Node'
    { jobDetails = Prelude.Nothing,
      triggerDetails = Prelude.Nothing,
      name = Prelude.Nothing,
      uniqueId = Prelude.Nothing,
      crawlerDetails = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Details of the Job when the node represents a Job.
node_jobDetails :: Lens.Lens' Node (Prelude.Maybe JobNodeDetails)
node_jobDetails = Lens.lens (\Node' {jobDetails} -> jobDetails) (\s@Node' {} a -> s {jobDetails = a} :: Node)

-- | Details of the Trigger when the node represents a Trigger.
node_triggerDetails :: Lens.Lens' Node (Prelude.Maybe TriggerNodeDetails)
node_triggerDetails = Lens.lens (\Node' {triggerDetails} -> triggerDetails) (\s@Node' {} a -> s {triggerDetails = a} :: Node)

-- | The name of the AWS Glue component represented by the node.
node_name :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_name = Lens.lens (\Node' {name} -> name) (\s@Node' {} a -> s {name = a} :: Node)

-- | The unique Id assigned to the node within the workflow.
node_uniqueId :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_uniqueId = Lens.lens (\Node' {uniqueId} -> uniqueId) (\s@Node' {} a -> s {uniqueId = a} :: Node)

-- | Details of the crawler when the node represents a crawler.
node_crawlerDetails :: Lens.Lens' Node (Prelude.Maybe CrawlerNodeDetails)
node_crawlerDetails = Lens.lens (\Node' {crawlerDetails} -> crawlerDetails) (\s@Node' {} a -> s {crawlerDetails = a} :: Node)

-- | The type of AWS Glue component represented by the node.
node_type :: Lens.Lens' Node (Prelude.Maybe NodeType)
node_type = Lens.lens (\Node' {type'} -> type') (\s@Node' {} a -> s {type' = a} :: Node)

instance Prelude.FromJSON Node where
  parseJSON =
    Prelude.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Prelude..:? "JobDetails")
            Prelude.<*> (x Prelude..:? "TriggerDetails")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "UniqueId")
            Prelude.<*> (x Prelude..:? "CrawlerDetails")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable Node

instance Prelude.NFData Node
