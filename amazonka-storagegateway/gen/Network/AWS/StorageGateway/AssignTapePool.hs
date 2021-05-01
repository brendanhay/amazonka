{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.StorageGateway.AssignTapePool
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a tape to a tape pool for archiving. The tape assigned to a pool
-- is archived in the S3 storage class that is associated with the pool.
-- When you use your backup application to eject the tape, the tape is
-- archived directly into the S3 storage class (S3 Glacier or S3 Glacier
-- Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
module Network.AWS.StorageGateway.AssignTapePool
  ( -- * Creating a Request
    AssignTapePool (..),
    newAssignTapePool,

    -- * Request Lenses
    assignTapePool_bypassGovernanceRetention,
    assignTapePool_tapeARN,
    assignTapePool_poolId,

    -- * Destructuring the Response
    AssignTapePoolResponse (..),
    newAssignTapePoolResponse,

    -- * Response Lenses
    assignTapePoolResponse_tapeARN,
    assignTapePoolResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newAssignTapePool' smart constructor.
data AssignTapePool = AssignTapePool'
  { -- | Set permissions to bypass governance retention. If the lock type of the
    -- archived tape is @Governance@, the tape\'s archived age is not older
    -- than @RetentionLockInDays@, and the user does not already have
    -- @BypassGovernanceRetention@, setting this to TRUE enables the user to
    -- bypass the retention lock. This parameter is set to true by default for
    -- calls from the console.
    --
    -- Valid values: @TRUE@ | @FALSE@
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
    -- | The unique Amazon Resource Name (ARN) of the virtual tape that you want
    -- to add to the tape pool.
    tapeARN :: Prelude.Text,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignTapePool' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassGovernanceRetention', 'assignTapePool_bypassGovernanceRetention' - Set permissions to bypass governance retention. If the lock type of the
-- archived tape is @Governance@, the tape\'s archived age is not older
-- than @RetentionLockInDays@, and the user does not already have
-- @BypassGovernanceRetention@, setting this to TRUE enables the user to
-- bypass the retention lock. This parameter is set to true by default for
-- calls from the console.
--
-- Valid values: @TRUE@ | @FALSE@
--
-- 'tapeARN', 'assignTapePool_tapeARN' - The unique Amazon Resource Name (ARN) of the virtual tape that you want
-- to add to the tape pool.
--
-- 'poolId', 'assignTapePool_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
newAssignTapePool ::
  -- | 'tapeARN'
  Prelude.Text ->
  -- | 'poolId'
  Prelude.Text ->
  AssignTapePool
newAssignTapePool pTapeARN_ pPoolId_ =
  AssignTapePool'
    { bypassGovernanceRetention =
        Prelude.Nothing,
      tapeARN = pTapeARN_,
      poolId = pPoolId_
    }

-- | Set permissions to bypass governance retention. If the lock type of the
-- archived tape is @Governance@, the tape\'s archived age is not older
-- than @RetentionLockInDays@, and the user does not already have
-- @BypassGovernanceRetention@, setting this to TRUE enables the user to
-- bypass the retention lock. This parameter is set to true by default for
-- calls from the console.
--
-- Valid values: @TRUE@ | @FALSE@
assignTapePool_bypassGovernanceRetention :: Lens.Lens' AssignTapePool (Prelude.Maybe Prelude.Bool)
assignTapePool_bypassGovernanceRetention = Lens.lens (\AssignTapePool' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@AssignTapePool' {} a -> s {bypassGovernanceRetention = a} :: AssignTapePool)

-- | The unique Amazon Resource Name (ARN) of the virtual tape that you want
-- to add to the tape pool.
assignTapePool_tapeARN :: Lens.Lens' AssignTapePool Prelude.Text
assignTapePool_tapeARN = Lens.lens (\AssignTapePool' {tapeARN} -> tapeARN) (\s@AssignTapePool' {} a -> s {tapeARN = a} :: AssignTapePool)

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
assignTapePool_poolId :: Lens.Lens' AssignTapePool Prelude.Text
assignTapePool_poolId = Lens.lens (\AssignTapePool' {poolId} -> poolId) (\s@AssignTapePool' {} a -> s {poolId = a} :: AssignTapePool)

instance Prelude.AWSRequest AssignTapePool where
  type Rs AssignTapePool = AssignTapePoolResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          AssignTapePoolResponse'
            Prelude.<$> (x Prelude..?> "TapeARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssignTapePool

instance Prelude.NFData AssignTapePool

instance Prelude.ToHeaders AssignTapePool where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.AssignTapePool" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON AssignTapePool where
  toJSON AssignTapePool' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("BypassGovernanceRetention" Prelude..=)
              Prelude.<$> bypassGovernanceRetention,
            Prelude.Just ("TapeARN" Prelude..= tapeARN),
            Prelude.Just ("PoolId" Prelude..= poolId)
          ]
      )

instance Prelude.ToPath AssignTapePool where
  toPath = Prelude.const "/"

instance Prelude.ToQuery AssignTapePool where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssignTapePoolResponse' smart constructor.
data AssignTapePoolResponse = AssignTapePoolResponse'
  { -- | The unique Amazon Resource Names (ARN) of the virtual tape that was
    -- added to the tape pool.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssignTapePoolResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'assignTapePoolResponse_tapeARN' - The unique Amazon Resource Names (ARN) of the virtual tape that was
-- added to the tape pool.
--
-- 'httpStatus', 'assignTapePoolResponse_httpStatus' - The response's http status code.
newAssignTapePoolResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssignTapePoolResponse
newAssignTapePoolResponse pHttpStatus_ =
  AssignTapePoolResponse'
    { tapeARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique Amazon Resource Names (ARN) of the virtual tape that was
-- added to the tape pool.
assignTapePoolResponse_tapeARN :: Lens.Lens' AssignTapePoolResponse (Prelude.Maybe Prelude.Text)
assignTapePoolResponse_tapeARN = Lens.lens (\AssignTapePoolResponse' {tapeARN} -> tapeARN) (\s@AssignTapePoolResponse' {} a -> s {tapeARN = a} :: AssignTapePoolResponse)

-- | The response's http status code.
assignTapePoolResponse_httpStatus :: Lens.Lens' AssignTapePoolResponse Prelude.Int
assignTapePoolResponse_httpStatus = Lens.lens (\AssignTapePoolResponse' {httpStatus} -> httpStatus) (\s@AssignTapePoolResponse' {} a -> s {httpStatus = a} :: AssignTapePoolResponse)

instance Prelude.NFData AssignTapePoolResponse
