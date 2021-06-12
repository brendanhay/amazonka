{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.CopySnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a manual snapshot of an instance or disk as another manual
-- snapshot, or copies an automatic snapshot of an instance or disk as a
-- manual snapshot. This operation can also be used to copy a manual or
-- automatic snapshot of an instance or a disk from one AWS Region to
-- another in Amazon Lightsail.
--
-- When copying a /manual snapshot/, be sure to define the @source region@,
-- @source snapshot name@, and @target snapshot name@ parameters.
--
-- When copying an /automatic snapshot/, be sure to define the
-- @source region@, @source resource name@, @target snapshot name@, and
-- either the @restore date@ or the @use latest restorable auto snapshot@
-- parameters.
module Network.AWS.Lightsail.CopySnapshot
  ( -- * Creating a Request
    CopySnapshot (..),
    newCopySnapshot,

    -- * Request Lenses
    copySnapshot_restoreDate,
    copySnapshot_sourceSnapshotName,
    copySnapshot_useLatestRestorableAutoSnapshot,
    copySnapshot_sourceResourceName,
    copySnapshot_targetSnapshotName,
    copySnapshot_sourceRegion,

    -- * Destructuring the Response
    CopySnapshotResponse (..),
    newCopySnapshotResponse,

    -- * Response Lenses
    copySnapshotResponse_operations,
    copySnapshotResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCopySnapshot' smart constructor.
data CopySnapshot = CopySnapshot'
  { -- | The date of the source automatic snapshot to copy. Use the
    -- @get auto snapshots@ operation to identify the dates of the available
    -- automatic snapshots.
    --
    -- Constraints:
    --
    -- -   Must be specified in @YYYY-MM-DD@ format.
    --
    -- -   This parameter cannot be defined together with the
    --     @use latest restorable auto snapshot@ parameter. The @restore date@
    --     and @use latest restorable auto snapshot@ parameters are mutually
    --     exclusive.
    --
    -- -   Define this parameter only when copying an automatic snapshot as a
    --     manual snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
    restoreDate :: Core.Maybe Core.Text,
    -- | The name of the source manual snapshot to copy.
    --
    -- Constraint:
    --
    -- -   Define this parameter only when copying a manual snapshot as another
    --     manual snapshot.
    sourceSnapshotName :: Core.Maybe Core.Text,
    -- | A Boolean value to indicate whether to use the latest available
    -- automatic snapshot of the specified source instance or disk.
    --
    -- Constraints:
    --
    -- -   This parameter cannot be defined together with the @restore date@
    --     parameter. The @use latest restorable auto snapshot@ and
    --     @restore date@ parameters are mutually exclusive.
    --
    -- -   Define this parameter only when copying an automatic snapshot as a
    --     manual snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
    useLatestRestorableAutoSnapshot :: Core.Maybe Core.Bool,
    -- | The name of the source instance or disk from which the source automatic
    -- snapshot was created.
    --
    -- Constraint:
    --
    -- -   Define this parameter only when copying an automatic snapshot as a
    --     manual snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
    sourceResourceName :: Core.Maybe Core.Text,
    -- | The name of the new manual snapshot to be created as a copy.
    targetSnapshotName :: Core.Text,
    -- | The AWS Region where the source manual or automatic snapshot is located.
    sourceRegion :: RegionName
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopySnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'restoreDate', 'copySnapshot_restoreDate' - The date of the source automatic snapshot to copy. Use the
-- @get auto snapshots@ operation to identify the dates of the available
-- automatic snapshots.
--
-- Constraints:
--
-- -   Must be specified in @YYYY-MM-DD@ format.
--
-- -   This parameter cannot be defined together with the
--     @use latest restorable auto snapshot@ parameter. The @restore date@
--     and @use latest restorable auto snapshot@ parameters are mutually
--     exclusive.
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
--
-- 'sourceSnapshotName', 'copySnapshot_sourceSnapshotName' - The name of the source manual snapshot to copy.
--
-- Constraint:
--
-- -   Define this parameter only when copying a manual snapshot as another
--     manual snapshot.
--
-- 'useLatestRestorableAutoSnapshot', 'copySnapshot_useLatestRestorableAutoSnapshot' - A Boolean value to indicate whether to use the latest available
-- automatic snapshot of the specified source instance or disk.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the @restore date@
--     parameter. The @use latest restorable auto snapshot@ and
--     @restore date@ parameters are mutually exclusive.
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
--
-- 'sourceResourceName', 'copySnapshot_sourceResourceName' - The name of the source instance or disk from which the source automatic
-- snapshot was created.
--
-- Constraint:
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
--
-- 'targetSnapshotName', 'copySnapshot_targetSnapshotName' - The name of the new manual snapshot to be created as a copy.
--
-- 'sourceRegion', 'copySnapshot_sourceRegion' - The AWS Region where the source manual or automatic snapshot is located.
newCopySnapshot ::
  -- | 'targetSnapshotName'
  Core.Text ->
  -- | 'sourceRegion'
  RegionName ->
  CopySnapshot
newCopySnapshot pTargetSnapshotName_ pSourceRegion_ =
  CopySnapshot'
    { restoreDate = Core.Nothing,
      sourceSnapshotName = Core.Nothing,
      useLatestRestorableAutoSnapshot = Core.Nothing,
      sourceResourceName = Core.Nothing,
      targetSnapshotName = pTargetSnapshotName_,
      sourceRegion = pSourceRegion_
    }

-- | The date of the source automatic snapshot to copy. Use the
-- @get auto snapshots@ operation to identify the dates of the available
-- automatic snapshots.
--
-- Constraints:
--
-- -   Must be specified in @YYYY-MM-DD@ format.
--
-- -   This parameter cannot be defined together with the
--     @use latest restorable auto snapshot@ parameter. The @restore date@
--     and @use latest restorable auto snapshot@ parameters are mutually
--     exclusive.
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
copySnapshot_restoreDate :: Lens.Lens' CopySnapshot (Core.Maybe Core.Text)
copySnapshot_restoreDate = Lens.lens (\CopySnapshot' {restoreDate} -> restoreDate) (\s@CopySnapshot' {} a -> s {restoreDate = a} :: CopySnapshot)

-- | The name of the source manual snapshot to copy.
--
-- Constraint:
--
-- -   Define this parameter only when copying a manual snapshot as another
--     manual snapshot.
copySnapshot_sourceSnapshotName :: Lens.Lens' CopySnapshot (Core.Maybe Core.Text)
copySnapshot_sourceSnapshotName = Lens.lens (\CopySnapshot' {sourceSnapshotName} -> sourceSnapshotName) (\s@CopySnapshot' {} a -> s {sourceSnapshotName = a} :: CopySnapshot)

-- | A Boolean value to indicate whether to use the latest available
-- automatic snapshot of the specified source instance or disk.
--
-- Constraints:
--
-- -   This parameter cannot be defined together with the @restore date@
--     parameter. The @use latest restorable auto snapshot@ and
--     @restore date@ parameters are mutually exclusive.
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
copySnapshot_useLatestRestorableAutoSnapshot :: Lens.Lens' CopySnapshot (Core.Maybe Core.Bool)
copySnapshot_useLatestRestorableAutoSnapshot = Lens.lens (\CopySnapshot' {useLatestRestorableAutoSnapshot} -> useLatestRestorableAutoSnapshot) (\s@CopySnapshot' {} a -> s {useLatestRestorableAutoSnapshot = a} :: CopySnapshot)

-- | The name of the source instance or disk from which the source automatic
-- snapshot was created.
--
-- Constraint:
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Lightsail Dev Guide>.
copySnapshot_sourceResourceName :: Lens.Lens' CopySnapshot (Core.Maybe Core.Text)
copySnapshot_sourceResourceName = Lens.lens (\CopySnapshot' {sourceResourceName} -> sourceResourceName) (\s@CopySnapshot' {} a -> s {sourceResourceName = a} :: CopySnapshot)

-- | The name of the new manual snapshot to be created as a copy.
copySnapshot_targetSnapshotName :: Lens.Lens' CopySnapshot Core.Text
copySnapshot_targetSnapshotName = Lens.lens (\CopySnapshot' {targetSnapshotName} -> targetSnapshotName) (\s@CopySnapshot' {} a -> s {targetSnapshotName = a} :: CopySnapshot)

-- | The AWS Region where the source manual or automatic snapshot is located.
copySnapshot_sourceRegion :: Lens.Lens' CopySnapshot RegionName
copySnapshot_sourceRegion = Lens.lens (\CopySnapshot' {sourceRegion} -> sourceRegion) (\s@CopySnapshot' {} a -> s {sourceRegion = a} :: CopySnapshot)

instance Core.AWSRequest CopySnapshot where
  type AWSResponse CopySnapshot = CopySnapshotResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CopySnapshotResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CopySnapshot

instance Core.NFData CopySnapshot

instance Core.ToHeaders CopySnapshot where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CopySnapshot" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CopySnapshot where
  toJSON CopySnapshot' {..} =
    Core.object
      ( Core.catMaybes
          [ ("restoreDate" Core..=) Core.<$> restoreDate,
            ("sourceSnapshotName" Core..=)
              Core.<$> sourceSnapshotName,
            ("useLatestRestorableAutoSnapshot" Core..=)
              Core.<$> useLatestRestorableAutoSnapshot,
            ("sourceResourceName" Core..=)
              Core.<$> sourceResourceName,
            Core.Just
              ("targetSnapshotName" Core..= targetSnapshotName),
            Core.Just ("sourceRegion" Core..= sourceRegion)
          ]
      )

instance Core.ToPath CopySnapshot where
  toPath = Core.const "/"

instance Core.ToQuery CopySnapshot where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CopySnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'copySnapshotResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'copySnapshotResponse_httpStatus' - The response's http status code.
newCopySnapshotResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CopySnapshotResponse
newCopySnapshotResponse pHttpStatus_ =
  CopySnapshotResponse'
    { operations = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
copySnapshotResponse_operations :: Lens.Lens' CopySnapshotResponse (Core.Maybe [Operation])
copySnapshotResponse_operations = Lens.lens (\CopySnapshotResponse' {operations} -> operations) (\s@CopySnapshotResponse' {} a -> s {operations = a} :: CopySnapshotResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
copySnapshotResponse_httpStatus :: Lens.Lens' CopySnapshotResponse Core.Int
copySnapshotResponse_httpStatus = Lens.lens (\CopySnapshotResponse' {httpStatus} -> httpStatus) (\s@CopySnapshotResponse' {} a -> s {httpStatus = a} :: CopySnapshotResponse)

instance Core.NFData CopySnapshotResponse
