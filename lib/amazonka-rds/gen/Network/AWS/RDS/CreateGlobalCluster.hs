{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.CreateGlobalCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Aurora global database spread across multiple AWS Regions. The global database contains a single primary cluster with read-write capability, and a read-only secondary cluster that receives data from the primary cluster through high-speed replication performed by the Aurora storage subsystem.
--
-- You can create a global database that is initially empty, and then add a primary cluster and a secondary cluster to it. Or you can specify an existing Aurora cluster during the create operation, and this cluster becomes the primary cluster of the global database.
module Network.AWS.RDS.CreateGlobalCluster
  ( -- * Creating a request
    CreateGlobalCluster (..),
    mkCreateGlobalCluster,

    -- ** Request lenses
    cgcDatabaseName,
    cgcDeletionProtection,
    cgcEngine,
    cgcEngineVersion,
    cgcGlobalClusterIdentifier,
    cgcSourceDBClusterIdentifier,
    cgcStorageEncrypted,

    -- * Destructuring the response
    CreateGlobalClusterResponse (..),
    mkCreateGlobalClusterResponse,

    -- ** Response lenses
    cgcrrsGlobalCluster,
    cgcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.RDS.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateGlobalCluster' smart constructor.
data CreateGlobalCluster = CreateGlobalCluster'
  { -- | The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
    databaseName :: Core.Maybe Types.String,
    -- | The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
    deletionProtection :: Core.Maybe Core.Bool,
    -- | The name of the database engine to be used for this DB cluster.
    engine :: Core.Maybe Types.String,
    -- | The engine version of the Aurora global database.
    engineVersion :: Core.Maybe Types.String,
    -- | The cluster identifier of the new global database cluster.
    globalClusterIdentifier :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
    sourceDBClusterIdentifier :: Core.Maybe Types.String,
    -- | The storage encryption setting for the new global database cluster.
    storageEncrypted :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGlobalCluster' value with any optional fields omitted.
mkCreateGlobalCluster ::
  CreateGlobalCluster
mkCreateGlobalCluster =
  CreateGlobalCluster'
    { databaseName = Core.Nothing,
      deletionProtection = Core.Nothing,
      engine = Core.Nothing,
      engineVersion = Core.Nothing,
      globalClusterIdentifier = Core.Nothing,
      sourceDBClusterIdentifier = Core.Nothing,
      storageEncrypted = Core.Nothing
    }

-- | The name for your database of up to 64 alpha-numeric characters. If you do not provide a name, Amazon Aurora will not create a database in the global database cluster you are creating.
--
-- /Note:/ Consider using 'databaseName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcDatabaseName :: Lens.Lens' CreateGlobalCluster (Core.Maybe Types.String)
cgcDatabaseName = Lens.field @"databaseName"
{-# DEPRECATED cgcDatabaseName "Use generic-lens or generic-optics with 'databaseName' instead." #-}

-- | The deletion protection setting for the new global database. The global database can't be deleted when deletion protection is enabled.
--
-- /Note:/ Consider using 'deletionProtection' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcDeletionProtection :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Bool)
cgcDeletionProtection = Lens.field @"deletionProtection"
{-# DEPRECATED cgcDeletionProtection "Use generic-lens or generic-optics with 'deletionProtection' instead." #-}

-- | The name of the database engine to be used for this DB cluster.
--
-- /Note:/ Consider using 'engine' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcEngine :: Lens.Lens' CreateGlobalCluster (Core.Maybe Types.String)
cgcEngine = Lens.field @"engine"
{-# DEPRECATED cgcEngine "Use generic-lens or generic-optics with 'engine' instead." #-}

-- | The engine version of the Aurora global database.
--
-- /Note:/ Consider using 'engineVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcEngineVersion :: Lens.Lens' CreateGlobalCluster (Core.Maybe Types.String)
cgcEngineVersion = Lens.field @"engineVersion"
{-# DEPRECATED cgcEngineVersion "Use generic-lens or generic-optics with 'engineVersion' instead." #-}

-- | The cluster identifier of the new global database cluster.
--
-- /Note:/ Consider using 'globalClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcGlobalClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Core.Maybe Types.String)
cgcGlobalClusterIdentifier = Lens.field @"globalClusterIdentifier"
{-# DEPRECATED cgcGlobalClusterIdentifier "Use generic-lens or generic-optics with 'globalClusterIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) to use as the primary cluster of the global database. This parameter is optional.
--
-- /Note:/ Consider using 'sourceDBClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcSourceDBClusterIdentifier :: Lens.Lens' CreateGlobalCluster (Core.Maybe Types.String)
cgcSourceDBClusterIdentifier = Lens.field @"sourceDBClusterIdentifier"
{-# DEPRECATED cgcSourceDBClusterIdentifier "Use generic-lens or generic-optics with 'sourceDBClusterIdentifier' instead." #-}

-- | The storage encryption setting for the new global database cluster.
--
-- /Note:/ Consider using 'storageEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcStorageEncrypted :: Lens.Lens' CreateGlobalCluster (Core.Maybe Core.Bool)
cgcStorageEncrypted = Lens.field @"storageEncrypted"
{-# DEPRECATED cgcStorageEncrypted "Use generic-lens or generic-optics with 'storageEncrypted' instead." #-}

instance Core.AWSRequest CreateGlobalCluster where
  type Rs CreateGlobalCluster = CreateGlobalClusterResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "CreateGlobalCluster")
                Core.<> (Core.pure ("Version", "2014-10-31"))
                Core.<> (Core.toQueryValue "DatabaseName" Core.<$> databaseName)
                Core.<> ( Core.toQueryValue "DeletionProtection"
                            Core.<$> deletionProtection
                        )
                Core.<> (Core.toQueryValue "Engine" Core.<$> engine)
                Core.<> (Core.toQueryValue "EngineVersion" Core.<$> engineVersion)
                Core.<> ( Core.toQueryValue "GlobalClusterIdentifier"
                            Core.<$> globalClusterIdentifier
                        )
                Core.<> ( Core.toQueryValue "SourceDBClusterIdentifier"
                            Core.<$> sourceDBClusterIdentifier
                        )
                Core.<> (Core.toQueryValue "StorageEncrypted" Core.<$> storageEncrypted)
            )
      }
  response =
    Response.receiveXMLWrapper
      "CreateGlobalClusterResult"
      ( \s h x ->
          CreateGlobalClusterResponse'
            Core.<$> (x Core..@? "GlobalCluster")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateGlobalClusterResponse' smart constructor.
data CreateGlobalClusterResponse = CreateGlobalClusterResponse'
  { globalCluster :: Core.Maybe Types.GlobalCluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGlobalClusterResponse' value with any optional fields omitted.
mkCreateGlobalClusterResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateGlobalClusterResponse
mkCreateGlobalClusterResponse responseStatus =
  CreateGlobalClusterResponse'
    { globalCluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'globalCluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrrsGlobalCluster :: Lens.Lens' CreateGlobalClusterResponse (Core.Maybe Types.GlobalCluster)
cgcrrsGlobalCluster = Lens.field @"globalCluster"
{-# DEPRECATED cgcrrsGlobalCluster "Use generic-lens or generic-optics with 'globalCluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrrsResponseStatus :: Lens.Lens' CreateGlobalClusterResponse Core.Int
cgcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cgcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
