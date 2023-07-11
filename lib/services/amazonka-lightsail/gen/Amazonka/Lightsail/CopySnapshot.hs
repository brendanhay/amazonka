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
-- Module      : Amazonka.Lightsail.CopySnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Copies a manual snapshot of an instance or disk as another manual
-- snapshot, or copies an automatic snapshot of an instance or disk as a
-- manual snapshot. This operation can also be used to copy a manual or
-- automatic snapshot of an instance or a disk from one Amazon Web Services
-- Region to another in Amazon Lightsail.
--
-- When copying a /manual snapshot/, be sure to define the @source region@,
-- @source snapshot name@, and @target snapshot name@ parameters.
--
-- When copying an /automatic snapshot/, be sure to define the
-- @source region@, @source resource name@, @target snapshot name@, and
-- either the @restore date@ or the @use latest restorable auto snapshot@
-- parameters.
module Amazonka.Lightsail.CopySnapshot
  ( -- * Creating a Request
    CopySnapshot (..),
    newCopySnapshot,

    -- * Request Lenses
    copySnapshot_restoreDate,
    copySnapshot_sourceResourceName,
    copySnapshot_sourceSnapshotName,
    copySnapshot_useLatestRestorableAutoSnapshot,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
    restoreDate :: Prelude.Maybe Prelude.Text,
    -- | The name of the source instance or disk from which the source automatic
    -- snapshot was created.
    --
    -- Constraint:
    --
    -- -   Define this parameter only when copying an automatic snapshot as a
    --     manual snapshot. For more information, see the
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
    sourceResourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source manual snapshot to copy.
    --
    -- Constraint:
    --
    -- -   Define this parameter only when copying a manual snapshot as another
    --     manual snapshot.
    sourceSnapshotName :: Prelude.Maybe Prelude.Text,
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
    --     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
    useLatestRestorableAutoSnapshot :: Prelude.Maybe Prelude.Bool,
    -- | The name of the new manual snapshot to be created as a copy.
    targetSnapshotName :: Prelude.Text,
    -- | The Amazon Web Services Region where the source manual or automatic
    -- snapshot is located.
    sourceRegion :: RegionName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
--
-- 'sourceResourceName', 'copySnapshot_sourceResourceName' - The name of the source instance or disk from which the source automatic
-- snapshot was created.
--
-- Constraint:
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
--
-- 'targetSnapshotName', 'copySnapshot_targetSnapshotName' - The name of the new manual snapshot to be created as a copy.
--
-- 'sourceRegion', 'copySnapshot_sourceRegion' - The Amazon Web Services Region where the source manual or automatic
-- snapshot is located.
newCopySnapshot ::
  -- | 'targetSnapshotName'
  Prelude.Text ->
  -- | 'sourceRegion'
  RegionName ->
  CopySnapshot
newCopySnapshot pTargetSnapshotName_ pSourceRegion_ =
  CopySnapshot'
    { restoreDate = Prelude.Nothing,
      sourceResourceName = Prelude.Nothing,
      sourceSnapshotName = Prelude.Nothing,
      useLatestRestorableAutoSnapshot = Prelude.Nothing,
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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
copySnapshot_restoreDate :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_restoreDate = Lens.lens (\CopySnapshot' {restoreDate} -> restoreDate) (\s@CopySnapshot' {} a -> s {restoreDate = a} :: CopySnapshot)

-- | The name of the source instance or disk from which the source automatic
-- snapshot was created.
--
-- Constraint:
--
-- -   Define this parameter only when copying an automatic snapshot as a
--     manual snapshot. For more information, see the
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
copySnapshot_sourceResourceName :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
copySnapshot_sourceResourceName = Lens.lens (\CopySnapshot' {sourceResourceName} -> sourceResourceName) (\s@CopySnapshot' {} a -> s {sourceResourceName = a} :: CopySnapshot)

-- | The name of the source manual snapshot to copy.
--
-- Constraint:
--
-- -   Define this parameter only when copying a manual snapshot as another
--     manual snapshot.
copySnapshot_sourceSnapshotName :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Text)
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
--     <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-keeping-automatic-snapshots Amazon Lightsail Developer Guide>.
copySnapshot_useLatestRestorableAutoSnapshot :: Lens.Lens' CopySnapshot (Prelude.Maybe Prelude.Bool)
copySnapshot_useLatestRestorableAutoSnapshot = Lens.lens (\CopySnapshot' {useLatestRestorableAutoSnapshot} -> useLatestRestorableAutoSnapshot) (\s@CopySnapshot' {} a -> s {useLatestRestorableAutoSnapshot = a} :: CopySnapshot)

-- | The name of the new manual snapshot to be created as a copy.
copySnapshot_targetSnapshotName :: Lens.Lens' CopySnapshot Prelude.Text
copySnapshot_targetSnapshotName = Lens.lens (\CopySnapshot' {targetSnapshotName} -> targetSnapshotName) (\s@CopySnapshot' {} a -> s {targetSnapshotName = a} :: CopySnapshot)

-- | The Amazon Web Services Region where the source manual or automatic
-- snapshot is located.
copySnapshot_sourceRegion :: Lens.Lens' CopySnapshot RegionName
copySnapshot_sourceRegion = Lens.lens (\CopySnapshot' {sourceRegion} -> sourceRegion) (\s@CopySnapshot' {} a -> s {sourceRegion = a} :: CopySnapshot)

instance Core.AWSRequest CopySnapshot where
  type AWSResponse CopySnapshot = CopySnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CopySnapshotResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CopySnapshot where
  hashWithSalt _salt CopySnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` restoreDate
      `Prelude.hashWithSalt` sourceResourceName
      `Prelude.hashWithSalt` sourceSnapshotName
      `Prelude.hashWithSalt` useLatestRestorableAutoSnapshot
      `Prelude.hashWithSalt` targetSnapshotName
      `Prelude.hashWithSalt` sourceRegion

instance Prelude.NFData CopySnapshot where
  rnf CopySnapshot' {..} =
    Prelude.rnf restoreDate
      `Prelude.seq` Prelude.rnf sourceResourceName
      `Prelude.seq` Prelude.rnf sourceSnapshotName
      `Prelude.seq` Prelude.rnf useLatestRestorableAutoSnapshot
      `Prelude.seq` Prelude.rnf targetSnapshotName
      `Prelude.seq` Prelude.rnf sourceRegion

instance Data.ToHeaders CopySnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.CopySnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CopySnapshot where
  toJSON CopySnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("restoreDate" Data..=) Prelude.<$> restoreDate,
            ("sourceResourceName" Data..=)
              Prelude.<$> sourceResourceName,
            ("sourceSnapshotName" Data..=)
              Prelude.<$> sourceSnapshotName,
            ("useLatestRestorableAutoSnapshot" Data..=)
              Prelude.<$> useLatestRestorableAutoSnapshot,
            Prelude.Just
              ("targetSnapshotName" Data..= targetSnapshotName),
            Prelude.Just ("sourceRegion" Data..= sourceRegion)
          ]
      )

instance Data.ToPath CopySnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery CopySnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCopySnapshotResponse' smart constructor.
data CopySnapshotResponse = CopySnapshotResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  CopySnapshotResponse
newCopySnapshotResponse pHttpStatus_ =
  CopySnapshotResponse'
    { operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
copySnapshotResponse_operations :: Lens.Lens' CopySnapshotResponse (Prelude.Maybe [Operation])
copySnapshotResponse_operations = Lens.lens (\CopySnapshotResponse' {operations} -> operations) (\s@CopySnapshotResponse' {} a -> s {operations = a} :: CopySnapshotResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
copySnapshotResponse_httpStatus :: Lens.Lens' CopySnapshotResponse Prelude.Int
copySnapshotResponse_httpStatus = Lens.lens (\CopySnapshotResponse' {httpStatus} -> httpStatus) (\s@CopySnapshotResponse' {} a -> s {httpStatus = a} :: CopySnapshotResponse)

instance Prelude.NFData CopySnapshotResponse where
  rnf CopySnapshotResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
