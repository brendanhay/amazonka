{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchLogs.Types.QueryDefinition
  ( QueryDefinition (..),

    -- * Smart constructor
    mkQueryDefinition,

    -- * Lenses
    qdLogGroupNames,
    qdQueryDefinitionId,
    qdName,
    qdQueryString,
    qdLastModified,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This structure contains details about a saved CloudWatch Logs Insights query definition.
--
-- /See:/ 'mkQueryDefinition' smart constructor.
data QueryDefinition = QueryDefinition'
  { logGroupNames ::
      Lude.Maybe [Lude.Text],
    queryDefinitionId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    queryString :: Lude.Maybe Lude.Text,
    lastModified :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'QueryDefinition' with the minimum fields required to make a request.
--
-- * 'lastModified' - The date that the query definition was most recently modified.
-- * 'logGroupNames' - If this query definition contains a list of log groups that it is limited to, that list appears here.
-- * 'name' - The name of the query definition.
-- * 'queryDefinitionId' - The unique ID of the query definition.
-- * 'queryString' - The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
mkQueryDefinition ::
  QueryDefinition
mkQueryDefinition =
  QueryDefinition'
    { logGroupNames = Lude.Nothing,
      queryDefinitionId = Lude.Nothing,
      name = Lude.Nothing,
      queryString = Lude.Nothing,
      lastModified = Lude.Nothing
    }

-- | If this query definition contains a list of log groups that it is limited to, that list appears here.
--
-- /Note:/ Consider using 'logGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdLogGroupNames :: Lens.Lens' QueryDefinition (Lude.Maybe [Lude.Text])
qdLogGroupNames = Lens.lens (logGroupNames :: QueryDefinition -> Lude.Maybe [Lude.Text]) (\s a -> s {logGroupNames = a} :: QueryDefinition)
{-# DEPRECATED qdLogGroupNames "Use generic-lens or generic-optics with 'logGroupNames' instead." #-}

-- | The unique ID of the query definition.
--
-- /Note:/ Consider using 'queryDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdQueryDefinitionId :: Lens.Lens' QueryDefinition (Lude.Maybe Lude.Text)
qdQueryDefinitionId = Lens.lens (queryDefinitionId :: QueryDefinition -> Lude.Maybe Lude.Text) (\s a -> s {queryDefinitionId = a} :: QueryDefinition)
{-# DEPRECATED qdQueryDefinitionId "Use generic-lens or generic-optics with 'queryDefinitionId' instead." #-}

-- | The name of the query definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdName :: Lens.Lens' QueryDefinition (Lude.Maybe Lude.Text)
qdName = Lens.lens (name :: QueryDefinition -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: QueryDefinition)
{-# DEPRECATED qdName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdQueryString :: Lens.Lens' QueryDefinition (Lude.Maybe Lude.Text)
qdQueryString = Lens.lens (queryString :: QueryDefinition -> Lude.Maybe Lude.Text) (\s a -> s {queryString = a} :: QueryDefinition)
{-# DEPRECATED qdQueryString "Use generic-lens or generic-optics with 'queryString' instead." #-}

-- | The date that the query definition was most recently modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdLastModified :: Lens.Lens' QueryDefinition (Lude.Maybe Lude.Natural)
qdLastModified = Lens.lens (lastModified :: QueryDefinition -> Lude.Maybe Lude.Natural) (\s a -> s {lastModified = a} :: QueryDefinition)
{-# DEPRECATED qdLastModified "Use generic-lens or generic-optics with 'lastModified' instead." #-}

instance Lude.FromJSON QueryDefinition where
  parseJSON =
    Lude.withObject
      "QueryDefinition"
      ( \x ->
          QueryDefinition'
            Lude.<$> (x Lude..:? "logGroupNames" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "queryDefinitionId")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "queryString")
            Lude.<*> (x Lude..:? "lastModified")
      )
