{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.RevokeSnapshotAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the ability of the specified AWS customer account to restore the specified snapshot. If the account is currently restoring the snapshot, the restore will run to completion.
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.RevokeSnapshotAccess
  ( -- * Creating a request
    RevokeSnapshotAccess (..),
    mkRevokeSnapshotAccess,

    -- ** Request lenses
    rsaSnapshotIdentifier,
    rsaAccountWithRestoreAccess,
    rsaSnapshotClusterIdentifier,

    -- * Destructuring the response
    RevokeSnapshotAccessResponse (..),
    mkRevokeSnapshotAccessResponse,

    -- ** Response lenses
    rsarrsSnapshot,
    rsarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkRevokeSnapshotAccess' smart constructor.
data RevokeSnapshotAccess = RevokeSnapshotAccess'
  { -- | The identifier of the snapshot that the account can no longer access.
    snapshotIdentifier :: Types.String,
    -- | The identifier of the AWS customer account that can no longer restore the specified snapshot.
    accountWithRestoreAccess :: Types.String,
    -- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    snapshotClusterIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RevokeSnapshotAccess' value with any optional fields omitted.
mkRevokeSnapshotAccess ::
  -- | 'snapshotIdentifier'
  Types.String ->
  -- | 'accountWithRestoreAccess'
  Types.String ->
  RevokeSnapshotAccess
mkRevokeSnapshotAccess snapshotIdentifier accountWithRestoreAccess =
  RevokeSnapshotAccess'
    { snapshotIdentifier,
      accountWithRestoreAccess,
      snapshotClusterIdentifier = Core.Nothing
    }

-- | The identifier of the snapshot that the account can no longer access.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaSnapshotIdentifier :: Lens.Lens' RevokeSnapshotAccess Types.String
rsaSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED rsaSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The identifier of the AWS customer account that can no longer restore the specified snapshot.
--
-- /Note:/ Consider using 'accountWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaAccountWithRestoreAccess :: Lens.Lens' RevokeSnapshotAccess Types.String
rsaAccountWithRestoreAccess = Lens.field @"accountWithRestoreAccess"
{-# DEPRECATED rsaAccountWithRestoreAccess "Use generic-lens or generic-optics with 'accountWithRestoreAccess' instead." #-}

-- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsaSnapshotClusterIdentifier :: Lens.Lens' RevokeSnapshotAccess (Core.Maybe Types.String)
rsaSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# DEPRECATED rsaSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

instance Core.AWSRequest RevokeSnapshotAccess where
  type Rs RevokeSnapshotAccess = RevokeSnapshotAccessResponse
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
            ( Core.pure ("Action", "RevokeSnapshotAccess")
                Core.<> (Core.pure ("Version", "2012-12-01"))
                Core.<> (Core.toQueryValue "SnapshotIdentifier" snapshotIdentifier)
                Core.<> ( Core.toQueryValue
                            "AccountWithRestoreAccess"
                            accountWithRestoreAccess
                        )
                Core.<> ( Core.toQueryValue "SnapshotClusterIdentifier"
                            Core.<$> snapshotClusterIdentifier
                        )
            )
      }
  response =
    Response.receiveXMLWrapper
      "RevokeSnapshotAccessResult"
      ( \s h x ->
          RevokeSnapshotAccessResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkRevokeSnapshotAccessResponse' smart constructor.
data RevokeSnapshotAccessResponse = RevokeSnapshotAccessResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RevokeSnapshotAccessResponse' value with any optional fields omitted.
mkRevokeSnapshotAccessResponse ::
  -- | 'responseStatus'
  Core.Int ->
  RevokeSnapshotAccessResponse
mkRevokeSnapshotAccessResponse responseStatus =
  RevokeSnapshotAccessResponse'
    { snapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsarrsSnapshot :: Lens.Lens' RevokeSnapshotAccessResponse (Core.Maybe Types.Snapshot)
rsarrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED rsarrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rsarrsResponseStatus :: Lens.Lens' RevokeSnapshotAccessResponse Core.Int
rsarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED rsarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
