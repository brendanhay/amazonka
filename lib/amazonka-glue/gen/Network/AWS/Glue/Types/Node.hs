{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.Node
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.Node
  ( Node (..),

    -- * Smart constructor
    mkNode,

    -- * Lenses
    nCrawlerDetails,
    nJobDetails,
    nName,
    nTriggerDetails,
    nType,
    nUniqueId,
  )
where

import qualified Network.AWS.Glue.Types.CrawlerNodeDetails as Types
import qualified Network.AWS.Glue.Types.JobNodeDetails as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Glue.Types.NodeType as Types
import qualified Network.AWS.Glue.Types.TriggerNodeDetails as Types
import qualified Network.AWS.Glue.Types.UniqueId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A node represents an AWS Glue component such as a trigger, or job, etc., that is part of a workflow.
--
-- /See:/ 'mkNode' smart constructor.
data Node = Node'
  { -- | Details of the crawler when the node represents a crawler.
    crawlerDetails :: Core.Maybe Types.CrawlerNodeDetails,
    -- | Details of the Job when the node represents a Job.
    jobDetails :: Core.Maybe Types.JobNodeDetails,
    -- | The name of the AWS Glue component represented by the node.
    name :: Core.Maybe Types.Name,
    -- | Details of the Trigger when the node represents a Trigger.
    triggerDetails :: Core.Maybe Types.TriggerNodeDetails,
    -- | The type of AWS Glue component represented by the node.
    type' :: Core.Maybe Types.NodeType,
    -- | The unique Id assigned to the node within the workflow.
    uniqueId :: Core.Maybe Types.UniqueId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Node' value with any optional fields omitted.
mkNode ::
  Node
mkNode =
  Node'
    { crawlerDetails = Core.Nothing,
      jobDetails = Core.Nothing,
      name = Core.Nothing,
      triggerDetails = Core.Nothing,
      type' = Core.Nothing,
      uniqueId = Core.Nothing
    }

-- | Details of the crawler when the node represents a crawler.
--
-- /Note:/ Consider using 'crawlerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCrawlerDetails :: Lens.Lens' Node (Core.Maybe Types.CrawlerNodeDetails)
nCrawlerDetails = Lens.field @"crawlerDetails"
{-# DEPRECATED nCrawlerDetails "Use generic-lens or generic-optics with 'crawlerDetails' instead." #-}

-- | Details of the Job when the node represents a Job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nJobDetails :: Lens.Lens' Node (Core.Maybe Types.JobNodeDetails)
nJobDetails = Lens.field @"jobDetails"
{-# DEPRECATED nJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The name of the AWS Glue component represented by the node.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nName :: Lens.Lens' Node (Core.Maybe Types.Name)
nName = Lens.field @"name"
{-# DEPRECATED nName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Details of the Trigger when the node represents a Trigger.
--
-- /Note:/ Consider using 'triggerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nTriggerDetails :: Lens.Lens' Node (Core.Maybe Types.TriggerNodeDetails)
nTriggerDetails = Lens.field @"triggerDetails"
{-# DEPRECATED nTriggerDetails "Use generic-lens or generic-optics with 'triggerDetails' instead." #-}

-- | The type of AWS Glue component represented by the node.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nType :: Lens.Lens' Node (Core.Maybe Types.NodeType)
nType = Lens.field @"type'"
{-# DEPRECATED nType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The unique Id assigned to the node within the workflow.
--
-- /Note:/ Consider using 'uniqueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nUniqueId :: Lens.Lens' Node (Core.Maybe Types.UniqueId)
nUniqueId = Lens.field @"uniqueId"
{-# DEPRECATED nUniqueId "Use generic-lens or generic-optics with 'uniqueId' instead." #-}

instance Core.FromJSON Node where
  parseJSON =
    Core.withObject "Node" Core.$
      \x ->
        Node'
          Core.<$> (x Core..:? "CrawlerDetails")
          Core.<*> (x Core..:? "JobDetails")
          Core.<*> (x Core..:? "Name")
          Core.<*> (x Core..:? "TriggerDetails")
          Core.<*> (x Core..:? "Type")
          Core.<*> (x Core..:? "UniqueId")
