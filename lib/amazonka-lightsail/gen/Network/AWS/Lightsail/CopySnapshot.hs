{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CopySnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a manual snapshot of an instance or disk as another manual snapshot, or copies an automatic snapshot of an instance or disk as a manual snapshot. This operation can also be used to copy a manual or automatic snapshot of an instance or a disk from one AWS Region to another in Amazon Lightsail.
--
-- When copying a /manual snapshot/ , be sure to define the @source region@ , @source snapshot name@ , and @target snapshot name@ parameters.
-- When copying an /automatic snapshot/ , be sure to define the @source region@ , @source resource name@ , @target snapshot name@ , and either the @restore date@ or the @use latest restorable auto snapshot@ parameters.
module Network.AWS.Lightsail.CopySnapshot
  ( -- * Creating a request
    CopySnapshot (..),
    mkCopySnapshot,

    -- ** Request lenses
    csUseLatestRestorableAutoSnapshot,
    csRestoreDate,
    csSourceResourceName,
    csSourceSnapshotName,
    csTargetSnapshotName,
    csSourceRegion,

    -- * Destructuring the response
    CopySnapshotResponse (..),
    mkCopySnapshotResponse,

    -- ** Response lenses
    csrsOperations,
    csrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { useLatestRestorableAutoSnapshot ::
      Lude.Maybe Lude.Bool,
    restoreDate :: Lude.Maybe Lude.Text,
    sourceResourceName :: Lude.Maybe Lude.Text,
    sourceSnapshotName :: Lude.Maybe Lude.Text,
    targetSnapshotName :: Lude.Text,
    sourceRegion :: RegionName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopySnapshot' with the minimum fields required to make a request.
--
-- * 'restoreDate' - The date of the source automatic snapshot to copy. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'sourceRegion' - The AWS Region where the source manual or automatic snapshot is located.
-- * 'sourceResourceName' - The name of the source instance or disk from which the source automatic snapshot was created.
--
-- Constraint:
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
-- * 'sourceSnapshotName' - The name of the source manual snapshot to copy.
--
-- Constraint:
--
--     * Define this parameter only when copying a manual snapshot as another manual snapshot.
--
--
-- * 'targetSnapshotName' - The name of the new manual snapshot to be created as a copy.
-- * 'useLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available automatic snapshot of the specified source instance or disk.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
mkCopySnapshot ::
  -- | 'targetSnapshotName'
  Lude.Text ->
  -- | 'sourceRegion'
  RegionName ->
  CopySnapshot
mkCopySnapshot pTargetSnapshotName_ pSourceRegion_ =
  CopySnapshot'
    { useLatestRestorableAutoSnapshot = Lude.Nothing,
      restoreDate = Lude.Nothing,
      sourceResourceName = Lude.Nothing,
      sourceSnapshotName = Lude.Nothing,
      targetSnapshotName = pTargetSnapshotName_,
      sourceRegion = pSourceRegion_
    }

-- | A Boolean value to indicate whether to use the latest available automatic snapshot of the specified source instance or disk.
--
-- Constraints:
--
--     * This parameter cannot be defined together with the @restore date@ parameter. The @use latest restorable auto snapshot@ and @restore date@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'useLatestRestorableAutoSnapshot' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csUseLatestRestorableAutoSnapshot :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Bool)
csUseLatestRestorableAutoSnapshot = Lens.lens (useLatestRestorableAutoSnapshot :: CopySnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {useLatestRestorableAutoSnapshot = a} :: CopySnapshot)
{-# DEPRECATED csUseLatestRestorableAutoSnapshot "Use generic-lens or generic-optics with 'useLatestRestorableAutoSnapshot' instead." #-}

-- | The date of the source automatic snapshot to copy. Use the @get auto snapshots@ operation to identify the dates of the available automatic snapshots.
--
-- Constraints:
--
--     * Must be specified in @YYYY-MM-DD@ format.
--
--
--     * This parameter cannot be defined together with the @use latest restorable auto snapshot@ parameter. The @restore date@ and @use latest restorable auto snapshot@ parameters are mutually exclusive.
--
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'restoreDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csRestoreDate :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csRestoreDate = Lens.lens (restoreDate :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {restoreDate = a} :: CopySnapshot)
{-# DEPRECATED csRestoreDate "Use generic-lens or generic-optics with 'restoreDate' instead." #-}

-- | The name of the source instance or disk from which the source automatic snapshot was created.
--
-- Constraint:
--
--     * Define this parameter only when copying an automatic snapshot as a manual snapshot. For more information, see the <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide> .
--
--
--
-- /Note:/ Consider using 'sourceResourceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceResourceName :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csSourceResourceName = Lens.lens (sourceResourceName :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceResourceName = a} :: CopySnapshot)
{-# DEPRECATED csSourceResourceName "Use generic-lens or generic-optics with 'sourceResourceName' instead." #-}

-- | The name of the source manual snapshot to copy.
--
-- Constraint:
--
--     * Define this parameter only when copying a manual snapshot as another manual snapshot.
--
--
--
-- /Note:/ Consider using 'sourceSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceSnapshotName :: Lens.Lens' CopySnapshot (Lude.Maybe Lude.Text)
csSourceSnapshotName = Lens.lens (sourceSnapshotName :: CopySnapshot -> Lude.Maybe Lude.Text) (\s a -> s {sourceSnapshotName = a} :: CopySnapshot)
{-# DEPRECATED csSourceSnapshotName "Use generic-lens or generic-optics with 'sourceSnapshotName' instead." #-}

-- | The name of the new manual snapshot to be created as a copy.
--
-- /Note:/ Consider using 'targetSnapshotName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csTargetSnapshotName :: Lens.Lens' CopySnapshot Lude.Text
csTargetSnapshotName = Lens.lens (targetSnapshotName :: CopySnapshot -> Lude.Text) (\s a -> s {targetSnapshotName = a} :: CopySnapshot)
{-# DEPRECATED csTargetSnapshotName "Use generic-lens or generic-optics with 'targetSnapshotName' instead." #-}

-- | The AWS Region where the source manual or automatic snapshot is located.
--
-- /Note:/ Consider using 'sourceRegion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csSourceRegion :: Lens.Lens' CopySnapshot RegionName
csSourceRegion = Lens.lens (sourceRegion :: CopySnapshot -> RegionName) (\s a -> s {sourceRegion = a} :: CopySnapshot)
{-# DEPRECATED csSourceRegion "Use generic-lens or generic-optics with 'sourceRegion' instead." #-}

instance Lude.AWSRequest CopySnapshot where
  type Rs CopySnapshot = CopySnapshotResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          CopySnapshotResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CopySnapshot where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.CopySnapshot" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CopySnapshot where
  toJSON CopySnapshot' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("useLatestRestorableAutoSnapshot" Lude..=)
              Lude.<$> useLatestRestorableAutoSnapshot,
            ("restoreDate" Lude..=) Lude.<$> restoreDate,
            ("sourceResourceName" Lude..=) Lude.<$> sourceResourceName,
            ("sourceSnapshotName" Lude..=) Lude.<$> sourceSnapshotName,
            Lude.Just ("targetSnapshotName" Lude..= targetSnapshotName),
            Lude.Just ("sourceRegion" Lude..= sourceRegion)
          ]
      )

instance Lude.ToPath CopySnapshot where
  toPath = Lude.const "/"

instance Lude.ToQuery CopySnapshot where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { operations ::
      Lude.Maybe [Operation],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CopySnapshotResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkCopySnapshotResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CopySnapshotResponse
mkCopySnapshotResponse pResponseStatus_ =
  CopySnapshotResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsOperations :: Lens.Lens' CopySnapshotResponse (Lude.Maybe [Operation])
csrsOperations = Lens.lens (operations :: CopySnapshotResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: CopySnapshotResponse)
{-# DEPRECATED csrsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csrsResponseStatus :: Lens.Lens' CopySnapshotResponse Lude.Int
csrsResponseStatus = Lens.lens (responseStatus :: CopySnapshotResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CopySnapshotResponse)
{-# DEPRECATED csrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
