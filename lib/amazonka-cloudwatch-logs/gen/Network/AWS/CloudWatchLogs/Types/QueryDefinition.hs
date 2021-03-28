{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.Types.QueryDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchLogs.Types.QueryDefinition
  ( QueryDefinition (..)
  -- * Smart constructor
  , mkQueryDefinition
  -- * Lenses
  , qdLastModified
  , qdLogGroupNames
  , qdName
  , qdQueryDefinitionId
  , qdQueryString
  ) where

import qualified Network.AWS.CloudWatchLogs.Types.LogGroupName as Types
import qualified Network.AWS.CloudWatchLogs.Types.QueryDefinitionName as Types
import qualified Network.AWS.CloudWatchLogs.Types.QueryId as Types
import qualified Network.AWS.CloudWatchLogs.Types.QueryString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This structure contains details about a saved CloudWatch Logs Insights query definition.
--
-- /See:/ 'mkQueryDefinition' smart constructor.
data QueryDefinition = QueryDefinition'
  { lastModified :: Core.Maybe Core.Natural
    -- ^ The date that the query definition was most recently modified.
  , logGroupNames :: Core.Maybe [Types.LogGroupName]
    -- ^ If this query definition contains a list of log groups that it is limited to, that list appears here.
  , name :: Core.Maybe Types.QueryDefinitionName
    -- ^ The name of the query definition.
  , queryDefinitionId :: Core.Maybe Types.QueryId
    -- ^ The unique ID of the query definition.
  , queryString :: Core.Maybe Types.QueryString
    -- ^ The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'QueryDefinition' value with any optional fields omitted.
mkQueryDefinition
    :: QueryDefinition
mkQueryDefinition
  = QueryDefinition'{lastModified = Core.Nothing,
                     logGroupNames = Core.Nothing, name = Core.Nothing,
                     queryDefinitionId = Core.Nothing, queryString = Core.Nothing}

-- | The date that the query definition was most recently modified.
--
-- /Note:/ Consider using 'lastModified' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdLastModified :: Lens.Lens' QueryDefinition (Core.Maybe Core.Natural)
qdLastModified = Lens.field @"lastModified"
{-# INLINEABLE qdLastModified #-}
{-# DEPRECATED lastModified "Use generic-lens or generic-optics with 'lastModified' instead"  #-}

-- | If this query definition contains a list of log groups that it is limited to, that list appears here.
--
-- /Note:/ Consider using 'logGroupNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdLogGroupNames :: Lens.Lens' QueryDefinition (Core.Maybe [Types.LogGroupName])
qdLogGroupNames = Lens.field @"logGroupNames"
{-# INLINEABLE qdLogGroupNames #-}
{-# DEPRECATED logGroupNames "Use generic-lens or generic-optics with 'logGroupNames' instead"  #-}

-- | The name of the query definition.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdName :: Lens.Lens' QueryDefinition (Core.Maybe Types.QueryDefinitionName)
qdName = Lens.field @"name"
{-# INLINEABLE qdName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The unique ID of the query definition.
--
-- /Note:/ Consider using 'queryDefinitionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdQueryDefinitionId :: Lens.Lens' QueryDefinition (Core.Maybe Types.QueryId)
qdQueryDefinitionId = Lens.field @"queryDefinitionId"
{-# INLINEABLE qdQueryDefinitionId #-}
{-# DEPRECATED queryDefinitionId "Use generic-lens or generic-optics with 'queryDefinitionId' instead"  #-}

-- | The query string to use for this definition. For more information, see <https://docs.aws.amazon.com/AmazonCloudWatch/latest/logs/CWL_QuerySyntax.html CloudWatch Logs Insights Query Syntax> .
--
-- /Note:/ Consider using 'queryString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
qdQueryString :: Lens.Lens' QueryDefinition (Core.Maybe Types.QueryString)
qdQueryString = Lens.field @"queryString"
{-# INLINEABLE qdQueryString #-}
{-# DEPRECATED queryString "Use generic-lens or generic-optics with 'queryString' instead"  #-}

instance Core.FromJSON QueryDefinition where
        parseJSON
          = Core.withObject "QueryDefinition" Core.$
              \ x ->
                QueryDefinition' Core.<$>
                  (x Core..:? "lastModified") Core.<*> x Core..:? "logGroupNames"
                    Core.<*> x Core..:? "name"
                    Core.<*> x Core..:? "queryDefinitionId"
                    Core.<*> x Core..:? "queryString"
