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
-- Module      : Amazonka.CloudSearchDomains.Types.Bucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.Bucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A container for facet information.
--
-- /See:/ 'newBucket' smart constructor.
data Bucket = Bucket'
  { -- | The number of hits that contain the facet value in the specified facet
    -- field.
    count :: Prelude.Maybe Prelude.Integer,
    -- | The facet value being counted.
    value :: Prelude.Maybe Prelude.Text
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
-- 'count', 'bucket_count' - The number of hits that contain the facet value in the specified facet
-- field.
--
-- 'value', 'bucket_value' - The facet value being counted.
newBucket ::
  Bucket
newBucket =
  Bucket'
    { count = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The number of hits that contain the facet value in the specified facet
-- field.
bucket_count :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Integer)
bucket_count = Lens.lens (\Bucket' {count} -> count) (\s@Bucket' {} a -> s {count = a} :: Bucket)

-- | The facet value being counted.
bucket_value :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_value = Lens.lens (\Bucket' {value} -> value) (\s@Bucket' {} a -> s {value = a} :: Bucket)

instance Data.FromJSON Bucket where
  parseJSON =
    Data.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Prelude.<$> (x Data..:? "count")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable Bucket where
  hashWithSalt _salt Bucket' {..} =
    _salt
      `Prelude.hashWithSalt` count
      `Prelude.hashWithSalt` value

instance Prelude.NFData Bucket where
  rnf Bucket' {..} =
    Prelude.rnf count `Prelude.seq` Prelude.rnf value
