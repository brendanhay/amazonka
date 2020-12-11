{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.BatchDeleteClusterSnapshots
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a set of cluster snapshots.
module Network.AWS.Redshift.BatchDeleteClusterSnapshots
  ( -- * Creating a request
    BatchDeleteClusterSnapshots (..),
    mkBatchDeleteClusterSnapshots,

    -- ** Request lenses
    bdcsIdentifiers,

    -- * Destructuring the response
    BatchDeleteClusterSnapshotsResponse (..),
    mkBatchDeleteClusterSnapshotsResponse,

    -- ** Response lenses
    bdcsrsResources,
    bdcsrsErrors,
    bdcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkBatchDeleteClusterSnapshots' smart constructor.
newtype BatchDeleteClusterSnapshots = BatchDeleteClusterSnapshots'
  { identifiers ::
      [DeleteClusterSnapshotMessage]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteClusterSnapshots' with the minimum fields required to make a request.
--
-- * 'identifiers' - A list of identifiers for the snapshots that you want to delete.
mkBatchDeleteClusterSnapshots ::
  BatchDeleteClusterSnapshots
mkBatchDeleteClusterSnapshots =
  BatchDeleteClusterSnapshots' {identifiers = Lude.mempty}

-- | A list of identifiers for the snapshots that you want to delete.
--
-- /Note:/ Consider using 'identifiers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsIdentifiers :: Lens.Lens' BatchDeleteClusterSnapshots [DeleteClusterSnapshotMessage]
bdcsIdentifiers = Lens.lens (identifiers :: BatchDeleteClusterSnapshots -> [DeleteClusterSnapshotMessage]) (\s a -> s {identifiers = a} :: BatchDeleteClusterSnapshots)
{-# DEPRECATED bdcsIdentifiers "Use generic-lens or generic-optics with 'identifiers' instead." #-}

instance Lude.AWSRequest BatchDeleteClusterSnapshots where
  type
    Rs BatchDeleteClusterSnapshots =
      BatchDeleteClusterSnapshotsResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "BatchDeleteClusterSnapshotsResult"
      ( \s h x ->
          BatchDeleteClusterSnapshotsResponse'
            Lude.<$> ( x Lude..@? "Resources" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "String")
                     )
            Lude.<*> ( x Lude..@? "Errors" Lude..!@ Lude.mempty
                         Lude.>>= Lude.may (Lude.parseXMLList "SnapshotErrorMessage")
                     )
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders BatchDeleteClusterSnapshots where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath BatchDeleteClusterSnapshots where
  toPath = Lude.const "/"

instance Lude.ToQuery BatchDeleteClusterSnapshots where
  toQuery BatchDeleteClusterSnapshots' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("BatchDeleteClusterSnapshots" :: Lude.ByteString),
        "Version" Lude.=: ("2012-12-01" :: Lude.ByteString),
        "Identifiers"
          Lude.=: Lude.toQueryList "DeleteClusterSnapshotMessage" identifiers
      ]

-- | /See:/ 'mkBatchDeleteClusterSnapshotsResponse' smart constructor.
data BatchDeleteClusterSnapshotsResponse = BatchDeleteClusterSnapshotsResponse'
  { resources ::
      Lude.Maybe
        [Lude.Text],
    errors ::
      Lude.Maybe
        [SnapshotErrorMessage],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchDeleteClusterSnapshotsResponse' with the minimum fields required to make a request.
--
-- * 'errors' - A list of any errors returned.
-- * 'resources' - A list of the snapshot identifiers that were deleted.
-- * 'responseStatus' - The response status code.
mkBatchDeleteClusterSnapshotsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  BatchDeleteClusterSnapshotsResponse
mkBatchDeleteClusterSnapshotsResponse pResponseStatus_ =
  BatchDeleteClusterSnapshotsResponse'
    { resources = Lude.Nothing,
      errors = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of the snapshot identifiers that were deleted.
--
-- /Note:/ Consider using 'resources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsrsResources :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Lude.Maybe [Lude.Text])
bdcsrsResources = Lens.lens (resources :: BatchDeleteClusterSnapshotsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {resources = a} :: BatchDeleteClusterSnapshotsResponse)
{-# DEPRECATED bdcsrsResources "Use generic-lens or generic-optics with 'resources' instead." #-}

-- | A list of any errors returned.
--
-- /Note:/ Consider using 'errors' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsrsErrors :: Lens.Lens' BatchDeleteClusterSnapshotsResponse (Lude.Maybe [SnapshotErrorMessage])
bdcsrsErrors = Lens.lens (errors :: BatchDeleteClusterSnapshotsResponse -> Lude.Maybe [SnapshotErrorMessage]) (\s a -> s {errors = a} :: BatchDeleteClusterSnapshotsResponse)
{-# DEPRECATED bdcsrsErrors "Use generic-lens or generic-optics with 'errors' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bdcsrsResponseStatus :: Lens.Lens' BatchDeleteClusterSnapshotsResponse Lude.Int
bdcsrsResponseStatus = Lens.lens (responseStatus :: BatchDeleteClusterSnapshotsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: BatchDeleteClusterSnapshotsResponse)
{-# DEPRECATED bdcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
