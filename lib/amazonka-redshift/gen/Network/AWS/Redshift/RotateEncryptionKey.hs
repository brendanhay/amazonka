{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RotateEncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rotates the encryption keys for a cluster.
module Network.AWS.Redshift.RotateEncryptionKey
  ( -- * Creating a request
    RotateEncryptionKey (..),
    mkRotateEncryptionKey,

    -- ** Request lenses
    rekClusterIdentifier,

    -- * Destructuring the response
    RotateEncryptionKeyResponse (..),
    mkRotateEncryptionKeyResponse,

    -- ** Response lenses
    rekrrsCluster,
    rekrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRotateEncryptionKey' smart constructor.
newtype RotateEncryptionKey = RotateEncryptionKey'
  { -- | The unique identifier of the cluster that you want to rotate the encryption keys for.
    --
    -- Constraints: Must be the name of valid cluster that has encryption enabled.
    clusterIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'RotateEncryptionKey' value with any optional fields omitted.
mkRotateEncryptionKey ::
  -- | 'clusterIdentifier'
  Types.String ->
  RotateEncryptionKey
mkRotateEncryptionKey clusterIdentifier =
  RotateEncryptionKey' {clusterIdentifier}

-- | The unique identifier of the cluster that you want to rotate the encryption keys for.
--
-- Constraints: Must be the name of valid cluster that has encryption enabled.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekClusterIdentifier :: Lens.Lens' RotateEncryptionKey Types.String
rekClusterIdentifier = Lens.field @"clusterIdentifier"
{-# DEPRECATED rekClusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead." #-}

instance Core.AWSRequest RotateEncryptionKey where
  type Rs RotateEncryptionKey = RotateEncryptionKeyResponse
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
            ( Core.pure ("Action", "RotateEncryptionKey")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "ClusterIdentifier" clusterIdentifier)
            )
      }
  response =
    Response.receiveXMLWrapper
      "RotateEncryptionKeyResult"
      ( \s h x ->
          RotateEncryptionKeyResponse'
            Core.<$> (x Core..@? "Cluster") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRotateEncryptionKeyResponse' smart constructor.
data RotateEncryptionKeyResponse = RotateEncryptionKeyResponse'
  { cluster :: Core.Maybe Types.Cluster,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RotateEncryptionKeyResponse' value with any optional fields omitted.
mkRotateEncryptionKeyResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RotateEncryptionKeyResponse
mkRotateEncryptionKeyResponse responseStatus =
  RotateEncryptionKeyResponse'
    { cluster = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekrrsCluster :: Lens.Lens' RotateEncryptionKeyResponse (Core.Maybe Types.Cluster)
rekrrsCluster = Lens.field @"cluster"
{-# DEPRECATED rekrrsCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rekrrsResponseStatus :: Lens.Lens' RotateEncryptionKeyResponse Core.Int
rekrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rekrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
