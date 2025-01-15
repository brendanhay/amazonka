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
-- Module      : Amazonka.CloudFront.Types.InvalidationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFront.Types.InvalidationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of an invalidation request.
--
-- /See:/ 'newInvalidationSummary' smart constructor.
data InvalidationSummary = InvalidationSummary'
  { -- | The unique ID for an invalidation request.
    id :: Prelude.Text,
    -- | The time that an invalidation request was created.
    createTime :: Data.ISO8601,
    -- | The status of an invalidation request.
    status :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InvalidationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'invalidationSummary_id' - The unique ID for an invalidation request.
--
-- 'createTime', 'invalidationSummary_createTime' - The time that an invalidation request was created.
--
-- 'status', 'invalidationSummary_status' - The status of an invalidation request.
newInvalidationSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'status'
  Prelude.Text ->
  InvalidationSummary
newInvalidationSummary pId_ pCreateTime_ pStatus_ =
  InvalidationSummary'
    { id = pId_,
      createTime = Data._Time Lens.# pCreateTime_,
      status = pStatus_
    }

-- | The unique ID for an invalidation request.
invalidationSummary_id :: Lens.Lens' InvalidationSummary Prelude.Text
invalidationSummary_id = Lens.lens (\InvalidationSummary' {id} -> id) (\s@InvalidationSummary' {} a -> s {id = a} :: InvalidationSummary)

-- | The time that an invalidation request was created.
invalidationSummary_createTime :: Lens.Lens' InvalidationSummary Prelude.UTCTime
invalidationSummary_createTime = Lens.lens (\InvalidationSummary' {createTime} -> createTime) (\s@InvalidationSummary' {} a -> s {createTime = a} :: InvalidationSummary) Prelude.. Data._Time

-- | The status of an invalidation request.
invalidationSummary_status :: Lens.Lens' InvalidationSummary Prelude.Text
invalidationSummary_status = Lens.lens (\InvalidationSummary' {status} -> status) (\s@InvalidationSummary' {} a -> s {status = a} :: InvalidationSummary)

instance Data.FromXML InvalidationSummary where
  parseXML x =
    InvalidationSummary'
      Prelude.<$> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "CreateTime")
      Prelude.<*> (x Data..@ "Status")

instance Prelude.Hashable InvalidationSummary where
  hashWithSalt _salt InvalidationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createTime
      `Prelude.hashWithSalt` status

instance Prelude.NFData InvalidationSummary where
  rnf InvalidationSummary' {..} =
    Prelude.rnf id `Prelude.seq`
      Prelude.rnf createTime `Prelude.seq`
        Prelude.rnf status
