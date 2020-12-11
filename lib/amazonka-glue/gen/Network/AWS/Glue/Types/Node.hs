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
    nTriggerDetails,
    nUniqueId,
    nCrawlerDetails,
    nName,
    nJobDetails,
    nType,
  )
where

import Network.AWS.Glue.Types.CrawlerNodeDetails
import Network.AWS.Glue.Types.JobNodeDetails
import Network.AWS.Glue.Types.NodeType
import Network.AWS.Glue.Types.TriggerNodeDetails
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A node represents an AWS Glue component such as a trigger, or job, etc., that is part of a workflow.
--
-- /See:/ 'mkNode' smart constructor.
data Node = Node'
  { triggerDetails :: Lude.Maybe TriggerNodeDetails,
    uniqueId :: Lude.Maybe Lude.Text,
    crawlerDetails :: Lude.Maybe CrawlerNodeDetails,
    name :: Lude.Maybe Lude.Text,
    jobDetails :: Lude.Maybe JobNodeDetails,
    type' :: Lude.Maybe NodeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Node' with the minimum fields required to make a request.
--
-- * 'crawlerDetails' - Details of the crawler when the node represents a crawler.
-- * 'jobDetails' - Details of the Job when the node represents a Job.
-- * 'name' - The name of the AWS Glue component represented by the node.
-- * 'triggerDetails' - Details of the Trigger when the node represents a Trigger.
-- * 'type'' - The type of AWS Glue component represented by the node.
-- * 'uniqueId' - The unique Id assigned to the node within the workflow.
mkNode ::
  Node
mkNode =
  Node'
    { triggerDetails = Lude.Nothing,
      uniqueId = Lude.Nothing,
      crawlerDetails = Lude.Nothing,
      name = Lude.Nothing,
      jobDetails = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | Details of the Trigger when the node represents a Trigger.
--
-- /Note:/ Consider using 'triggerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nTriggerDetails :: Lens.Lens' Node (Lude.Maybe TriggerNodeDetails)
nTriggerDetails = Lens.lens (triggerDetails :: Node -> Lude.Maybe TriggerNodeDetails) (\s a -> s {triggerDetails = a} :: Node)
{-# DEPRECATED nTriggerDetails "Use generic-lens or generic-optics with 'triggerDetails' instead." #-}

-- | The unique Id assigned to the node within the workflow.
--
-- /Note:/ Consider using 'uniqueId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nUniqueId :: Lens.Lens' Node (Lude.Maybe Lude.Text)
nUniqueId = Lens.lens (uniqueId :: Node -> Lude.Maybe Lude.Text) (\s a -> s {uniqueId = a} :: Node)
{-# DEPRECATED nUniqueId "Use generic-lens or generic-optics with 'uniqueId' instead." #-}

-- | Details of the crawler when the node represents a crawler.
--
-- /Note:/ Consider using 'crawlerDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nCrawlerDetails :: Lens.Lens' Node (Lude.Maybe CrawlerNodeDetails)
nCrawlerDetails = Lens.lens (crawlerDetails :: Node -> Lude.Maybe CrawlerNodeDetails) (\s a -> s {crawlerDetails = a} :: Node)
{-# DEPRECATED nCrawlerDetails "Use generic-lens or generic-optics with 'crawlerDetails' instead." #-}

-- | The name of the AWS Glue component represented by the node.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nName :: Lens.Lens' Node (Lude.Maybe Lude.Text)
nName = Lens.lens (name :: Node -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Node)
{-# DEPRECATED nName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Details of the Job when the node represents a Job.
--
-- /Note:/ Consider using 'jobDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nJobDetails :: Lens.Lens' Node (Lude.Maybe JobNodeDetails)
nJobDetails = Lens.lens (jobDetails :: Node -> Lude.Maybe JobNodeDetails) (\s a -> s {jobDetails = a} :: Node)
{-# DEPRECATED nJobDetails "Use generic-lens or generic-optics with 'jobDetails' instead." #-}

-- | The type of AWS Glue component represented by the node.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nType :: Lens.Lens' Node (Lude.Maybe NodeType)
nType = Lens.lens (type' :: Node -> Lude.Maybe NodeType) (\s a -> s {type' = a} :: Node)
{-# DEPRECATED nType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Node where
  parseJSON =
    Lude.withObject
      "Node"
      ( \x ->
          Node'
            Lude.<$> (x Lude..:? "TriggerDetails")
            Lude.<*> (x Lude..:? "UniqueId")
            Lude.<*> (x Lude..:? "CrawlerDetails")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "JobDetails")
            Lude.<*> (x Lude..:? "Type")
      )
