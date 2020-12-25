{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.AuthorizeSnapshotAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the specified AWS customer account to restore the specified snapshot.
--
-- For more information about working with snapshots, go to <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-snapshots.html Amazon Redshift Snapshots> in the /Amazon Redshift Cluster Management Guide/ .
module Network.AWS.Redshift.AuthorizeSnapshotAccess
  ( -- * Creating a request
    AuthorizeSnapshotAccess (..),
    mkAuthorizeSnapshotAccess,

    -- ** Request lenses
    asaSnapshotIdentifier,
    asaAccountWithRestoreAccess,
    asaSnapshotClusterIdentifier,

    -- * Destructuring the response
    AuthorizeSnapshotAccessResponse (..),
    mkAuthorizeSnapshotAccessResponse,

    -- ** Response lenses
    asarrsSnapshot,
    asarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Types as Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkAuthorizeSnapshotAccess' smart constructor.
data AuthorizeSnapshotAccess = AuthorizeSnapshotAccess'
  { -- | The identifier of the snapshot the account is authorized to restore.
    snapshotIdentifier :: Types.String,
    -- | The identifier of the AWS customer account authorized to restore the specified snapshot.
    --
    -- To share a snapshot with AWS support, specify amazon-redshift-support.
    accountWithRestoreAccess :: Types.String,
    -- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
    snapshotClusterIdentifier :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AuthorizeSnapshotAccess' value with any optional fields omitted.
mkAuthorizeSnapshotAccess ::
  -- | 'snapshotIdentifier'
  Types.String ->
  -- | 'accountWithRestoreAccess'
  Types.String ->
  AuthorizeSnapshotAccess
mkAuthorizeSnapshotAccess
  snapshotIdentifier
  accountWithRestoreAccess =
    AuthorizeSnapshotAccess'
      { snapshotIdentifier,
        accountWithRestoreAccess,
        snapshotClusterIdentifier = Core.Nothing
      }

-- | The identifier of the snapshot the account is authorized to restore.
--
-- /Note:/ Consider using 'snapshotIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaSnapshotIdentifier :: Lens.Lens' AuthorizeSnapshotAccess Types.String
asaSnapshotIdentifier = Lens.field @"snapshotIdentifier"
{-# DEPRECATED asaSnapshotIdentifier "Use generic-lens or generic-optics with 'snapshotIdentifier' instead." #-}

-- | The identifier of the AWS customer account authorized to restore the specified snapshot.
--
-- To share a snapshot with AWS support, specify amazon-redshift-support.
--
-- /Note:/ Consider using 'accountWithRestoreAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaAccountWithRestoreAccess :: Lens.Lens' AuthorizeSnapshotAccess Types.String
asaAccountWithRestoreAccess = Lens.field @"accountWithRestoreAccess"
{-# DEPRECATED asaAccountWithRestoreAccess "Use generic-lens or generic-optics with 'accountWithRestoreAccess' instead." #-}

-- | The identifier of the cluster the snapshot was created from. This parameter is required if your IAM user has a policy containing a snapshot resource element that specifies anything other than * for the cluster name.
--
-- /Note:/ Consider using 'snapshotClusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asaSnapshotClusterIdentifier :: Lens.Lens' AuthorizeSnapshotAccess (Core.Maybe Types.String)
asaSnapshotClusterIdentifier = Lens.field @"snapshotClusterIdentifier"
{-# DEPRECATED asaSnapshotClusterIdentifier "Use generic-lens or generic-optics with 'snapshotClusterIdentifier' instead." #-}

instance Core.AWSRequest AuthorizeSnapshotAccess where
  type Rs AuthorizeSnapshotAccess = AuthorizeSnapshotAccessResponse
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
            ( Core.pure ("Action", "AuthorizeSnapshotAccess")
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
      "AuthorizeSnapshotAccessResult"
      ( \s h x ->
          AuthorizeSnapshotAccessResponse'
            Core.<$> (x Core..@? "Snapshot") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkAuthorizeSnapshotAccessResponse' smart constructor.
data AuthorizeSnapshotAccessResponse = AuthorizeSnapshotAccessResponse'
  { snapshot :: Core.Maybe Types.Snapshot,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'AuthorizeSnapshotAccessResponse' value with any optional fields omitted.
mkAuthorizeSnapshotAccessResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AuthorizeSnapshotAccessResponse
mkAuthorizeSnapshotAccessResponse responseStatus =
  AuthorizeSnapshotAccessResponse'
    { snapshot = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'snapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asarrsSnapshot :: Lens.Lens' AuthorizeSnapshotAccessResponse (Core.Maybe Types.Snapshot)
asarrsSnapshot = Lens.field @"snapshot"
{-# DEPRECATED asarrsSnapshot "Use generic-lens or generic-optics with 'snapshot' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asarrsResponseStatus :: Lens.Lens' AuthorizeSnapshotAccessResponse Core.Int
asarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED asarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
