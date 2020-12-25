{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.RefreshSchemasStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.RefreshSchemasStatus
  ( RefreshSchemasStatus (..),

    -- * Smart constructor
    mkRefreshSchemasStatus,

    -- * Lenses
    rssEndpointArn,
    rssLastFailureMessage,
    rssLastRefreshDate,
    rssReplicationInstanceArn,
    rssStatus,
  )
where

import qualified Network.AWS.DMS.Types.RefreshSchemasStatusTypeValue as Types
import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides information that describes status of a schema at an endpoint specified by the @DescribeRefreshSchemaStatus@ operation.
--
-- /See:/ 'mkRefreshSchemasStatus' smart constructor.
data RefreshSchemasStatus = RefreshSchemasStatus'
  { -- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
    endpointArn :: Core.Maybe Types.String,
    -- | The last failure message for the schema.
    lastFailureMessage :: Core.Maybe Types.String,
    -- | The date the schema was last refreshed.
    lastRefreshDate :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the replication instance.
    replicationInstanceArn :: Core.Maybe Types.String,
    -- | The status of the schema.
    status :: Core.Maybe Types.RefreshSchemasStatusTypeValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RefreshSchemasStatus' value with any optional fields omitted.
mkRefreshSchemasStatus ::
  RefreshSchemasStatus
mkRefreshSchemasStatus =
  RefreshSchemasStatus'
    { endpointArn = Core.Nothing,
      lastFailureMessage = Core.Nothing,
      lastRefreshDate = Core.Nothing,
      replicationInstanceArn = Core.Nothing,
      status = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) string that uniquely identifies the endpoint.
--
-- /Note:/ Consider using 'endpointArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssEndpointArn :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Types.String)
rssEndpointArn = Lens.field @"endpointArn"
{-# DEPRECATED rssEndpointArn "Use generic-lens or generic-optics with 'endpointArn' instead." #-}

-- | The last failure message for the schema.
--
-- /Note:/ Consider using 'lastFailureMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssLastFailureMessage :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Types.String)
rssLastFailureMessage = Lens.field @"lastFailureMessage"
{-# DEPRECATED rssLastFailureMessage "Use generic-lens or generic-optics with 'lastFailureMessage' instead." #-}

-- | The date the schema was last refreshed.
--
-- /Note:/ Consider using 'lastRefreshDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssLastRefreshDate :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Core.NominalDiffTime)
rssLastRefreshDate = Lens.field @"lastRefreshDate"
{-# DEPRECATED rssLastRefreshDate "Use generic-lens or generic-optics with 'lastRefreshDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication instance.
--
-- /Note:/ Consider using 'replicationInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssReplicationInstanceArn :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Types.String)
rssReplicationInstanceArn = Lens.field @"replicationInstanceArn"
{-# DEPRECATED rssReplicationInstanceArn "Use generic-lens or generic-optics with 'replicationInstanceArn' instead." #-}

-- | The status of the schema.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rssStatus :: Lens.Lens' RefreshSchemasStatus (Core.Maybe Types.RefreshSchemasStatusTypeValue)
rssStatus = Lens.field @"status"
{-# DEPRECATED rssStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Core.FromJSON RefreshSchemasStatus where
  parseJSON =
    Core.withObject "RefreshSchemasStatus" Core.$
      \x ->
        RefreshSchemasStatus'
          Core.<$> (x Core..:? "EndpointArn")
          Core.<*> (x Core..:? "LastFailureMessage")
          Core.<*> (x Core..:? "LastRefreshDate")
          Core.<*> (x Core..:? "ReplicationInstanceArn")
          Core.<*> (x Core..:? "Status")
