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
-- Module      : Amazonka.Glue.Types.Node
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.Node where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.CrawlerNodeDetails
import Amazonka.Glue.Types.JobNodeDetails
import Amazonka.Glue.Types.NodeType
import Amazonka.Glue.Types.TriggerNodeDetails
import qualified Amazonka.Prelude as Prelude

-- | A node represents an Glue component (trigger, crawler, or job) on a
-- workflow graph.
--
-- /See:/ 'newNode' smart constructor.
data Node = Node'
  { -- | The name of the Glue component represented by the node.
    name :: Prelude.Maybe Prelude.Text,
    -- | The type of Glue component represented by the node.
    type' :: Prelude.Maybe NodeType,
    -- | Details of the Job when the node represents a Job.
    jobDetails :: Prelude.Maybe JobNodeDetails,
    -- | Details of the Trigger when the node represents a Trigger.
    triggerDetails :: Prelude.Maybe TriggerNodeDetails,
    -- | The unique Id assigned to the node within the workflow.
    uniqueId :: Prelude.Maybe Prelude.Text,
    -- | Details of the crawler when the node represents a crawler.
    crawlerDetails :: Prelude.Maybe CrawlerNodeDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Node' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'node_name' - The name of the Glue component represented by the node.
--
-- 'type'', 'node_type' - The type of Glue component represented by the node.
--
-- 'jobDetails', 'node_jobDetails' - Details of the Job when the node represents a Job.
--
-- 'triggerDetails', 'node_triggerDetails' - Details of the Trigger when the node represents a Trigger.
--
-- 'uniqueId', 'node_uniqueId' - The unique Id assigned to the node within the workflow.
--
-- 'crawlerDetails', 'node_crawlerDetails' - Details of the crawler when the node represents a crawler.
newNode ::
  Node
newNode =
  Node'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      triggerDetails = Prelude.Nothing,
      uniqueId = Prelude.Nothing,
      crawlerDetails = Prelude.Nothing
    }

-- | The name of the Glue component represented by the node.
node_name :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_name = Lens.lens (\Node' {name} -> name) (\s@Node' {} a -> s {name = a} :: Node)

-- | The type of Glue component represented by the node.
node_type :: Lens.Lens' Node (Prelude.Maybe NodeType)
node_type = Lens.lens (\Node' {type'} -> type') (\s@Node' {} a -> s {type' = a} :: Node)

-- | Details of the Job when the node represents a Job.
node_jobDetails :: Lens.Lens' Node (Prelude.Maybe JobNodeDetails)
node_jobDetails = Lens.lens (\Node' {jobDetails} -> jobDetails) (\s@Node' {} a -> s {jobDetails = a} :: Node)

-- | Details of the Trigger when the node represents a Trigger.
node_triggerDetails :: Lens.Lens' Node (Prelude.Maybe TriggerNodeDetails)
node_triggerDetails = Lens.lens (\Node' {triggerDetails} -> triggerDetails) (\s@Node' {} a -> s {triggerDetails = a} :: Node)

-- | The unique Id assigned to the node within the workflow.
node_uniqueId :: Lens.Lens' Node (Prelude.Maybe Prelude.Text)
node_uniqueId = Lens.lens (\Node' {uniqueId} -> uniqueId) (\s@Node' {} a -> s {uniqueId = a} :: Node)

-- | Details of the crawler when the node represents a crawler.
node_crawlerDetails :: Lens.Lens' Node (Prelude.Maybe CrawlerNodeDetails)
node_crawlerDetails = Lens.lens (\Node' {crawlerDetails} -> crawlerDetails) (\s@Node' {} a -> s {crawlerDetails = a} :: Node)

instance Core.FromJSON Node where
  parseJSON =
    Core.withObject
      "Node"
      ( \x ->
          Node'
            Prelude.<$> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "JobDetails")
            Prelude.<*> (x Core..:? "TriggerDetails")
            Prelude.<*> (x Core..:? "UniqueId")
            Prelude.<*> (x Core..:? "CrawlerDetails")
      )

instance Prelude.Hashable Node where
  hashWithSalt _salt Node' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` triggerDetails
      `Prelude.hashWithSalt` uniqueId
      `Prelude.hashWithSalt` crawlerDetails

instance Prelude.NFData Node where
  rnf Node' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf triggerDetails
      `Prelude.seq` Prelude.rnf uniqueId
      `Prelude.seq` Prelude.rnf crawlerDetails
