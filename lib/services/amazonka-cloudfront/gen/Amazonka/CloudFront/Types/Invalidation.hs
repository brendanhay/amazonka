{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFront.Types.Invalidation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.Invalidation where

import Amazonka.CloudFront.Types.InvalidationBatch
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An invalidation.
--
-- /See:/ 'newInvalidation' smart constructor.
data Invalidation = Invalidation'
  { -- | The identifier for the invalidation request. For example:
    -- @IDFDVBD632BHDS5@.
    id :: Prelude.Text,
    -- | The status of the invalidation request. When the invalidation batch is
    -- finished, the status is @Completed@.
    status :: Prelude.Text,
    -- | The date and time the invalidation request was first made.
    createTime :: Data.ISO8601,
    -- | The current invalidation information for the batch request.
    invalidationBatch :: InvalidationBatch
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Invalidation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'invalidation_id' - The identifier for the invalidation request. For example:
-- @IDFDVBD632BHDS5@.
--
-- 'status', 'invalidation_status' - The status of the invalidation request. When the invalidation batch is
-- finished, the status is @Completed@.
--
-- 'createTime', 'invalidation_createTime' - The date and time the invalidation request was first made.
--
-- 'invalidationBatch', 'invalidation_invalidationBatch' - The current invalidation information for the batch request.
newInvalidation ::
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'invalidationBatch'
  InvalidationBatch ->
  Invalidation
newInvalidation
  pId_
  pStatus_
  pCreateTime_
  pInvalidationBatch_ =
    Invalidation'
      { id = pId_,
        status = pStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        invalidationBatch = pInvalidationBatch_
      }

-- | The identifier for the invalidation request. For example:
-- @IDFDVBD632BHDS5@.
invalidation_id :: Lens.Lens' Invalidation Prelude.Text
invalidation_id = Lens.lens (\Invalidation' {id} -> id) (\s@Invalidation' {} a -> s {id = a} :: Invalidation)

-- | The status of the invalidation request. When the invalidation batch is
-- finished, the status is @Completed@.
invalidation_status :: Lens.Lens' Invalidation Prelude.Text
invalidation_status = Lens.lens (\Invalidation' {status} -> status) (\s@Invalidation' {} a -> s {status = a} :: Invalidation)

-- | The date and time the invalidation request was first made.
invalidation_createTime :: Lens.Lens' Invalidation Prelude.UTCTime
invalidation_createTime = Lens.lens (\Invalidation' {createTime} -> createTime) (\s@Invalidation' {} a -> s {createTime = a} :: Invalidation) Prelude.. Data._Time

-- | The current invalidation information for the batch request.
invalidation_invalidationBatch :: Lens.Lens' Invalidation InvalidationBatch
invalidation_invalidationBatch = Lens.lens (\Invalidation' {invalidationBatch} -> invalidationBatch) (\s@Invalidation' {} a -> s {invalidationBatch = a} :: Invalidation)

instance Data.FromXML Invalidation where
  parseXML x =
    Invalidation'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "Status")
      Prelude.<*> (x Data..@ "CreateTime")
      Prelude.<*> (x Data..@ "InvalidationBatch")

instance Prelude.Hashable Invalidation where
  hashWithSalt _salt Invalidation' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` invalidationBatch

instance Prelude.NFData Invalidation where
  rnf Invalidation' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf invalidationBatch
