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
-- Module      : Amazonka.IoT.Types.Bucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.Bucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A count of documents that meets a specific aggregation criteria.
--
-- /See:/ 'newBucket' smart constructor.
data Bucket = Bucket'
  { -- | The number of documents that have the value counted for the particular
    -- bucket.
    count :: Prelude.Maybe Prelude.Int,
    -- | The value counted for the particular bucket.
    keyValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'count', 'bucket_count' - The number of documents that have the value counted for the particular
-- bucket.
--
-- 'keyValue', 'bucket_keyValue' - The value counted for the particular bucket.
newBucket ::
  Bucket
newBucket =
  Bucket'
    { count = Prelude.Nothing,
      keyValue = Prelude.Nothing
    }

-- | The number of documents that have the value counted for the particular
-- bucket.
bucket_count :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Int)
bucket_count = Lens.lens (\Bucket' {count} -> count) (\s@Bucket' {} a -> s {count = a} :: Bucket)

-- | The value counted for the particular bucket.
bucket_keyValue :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_keyValue = Lens.lens (\Bucket' {keyValue} -> keyValue) (\s@Bucket' {} a -> s {keyValue = a} :: Bucket)

instance Data.FromJSON Bucket where
  parseJSON =
    Data.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "keyValue")
      )

instance Prelude.Hashable Bucket where
  hashWithSalt _salt Bucket' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` keyValue

instance Prelude.NFData Bucket where
  rnf Bucket' {..} =
    Prelude.rnf count
      `Prelude.seq` Prelude.rnf keyValue
