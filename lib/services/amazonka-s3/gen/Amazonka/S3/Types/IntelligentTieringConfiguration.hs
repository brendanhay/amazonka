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
-- Module      : Amazonka.S3.Types.IntelligentTieringConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.IntelligentTieringConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.IntelligentTieringFilter
import Amazonka.S3.Types.IntelligentTieringStatus
import Amazonka.S3.Types.Tiering

-- | Specifies the S3 Intelligent-Tiering configuration for an Amazon S3
-- bucket.
--
-- For information about the S3 Intelligent-Tiering storage class, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects>.
--
-- /See:/ 'newIntelligentTieringConfiguration' smart constructor.
data IntelligentTieringConfiguration = IntelligentTieringConfiguration'
  { -- | Specifies a bucket filter. The configuration only includes objects that
    -- meet the filter\'s criteria.
    filter' :: Prelude.Maybe IntelligentTieringFilter,
    -- | The ID used to identify the S3 Intelligent-Tiering configuration.
    id :: Prelude.Text,
    -- | Specifies the status of the configuration.
    status :: IntelligentTieringStatus,
    -- | Specifies the S3 Intelligent-Tiering storage class tier of the
    -- configuration.
    tierings :: [Tiering]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IntelligentTieringConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'intelligentTieringConfiguration_filter' - Specifies a bucket filter. The configuration only includes objects that
-- meet the filter\'s criteria.
--
-- 'id', 'intelligentTieringConfiguration_id' - The ID used to identify the S3 Intelligent-Tiering configuration.
--
-- 'status', 'intelligentTieringConfiguration_status' - Specifies the status of the configuration.
--
-- 'tierings', 'intelligentTieringConfiguration_tierings' - Specifies the S3 Intelligent-Tiering storage class tier of the
-- configuration.
newIntelligentTieringConfiguration ::
  -- | 'id'
  Prelude.Text ->
  -- | 'status'
  IntelligentTieringStatus ->
  IntelligentTieringConfiguration
newIntelligentTieringConfiguration pId_ pStatus_ =
  IntelligentTieringConfiguration'
    { filter' =
        Prelude.Nothing,
      id = pId_,
      status = pStatus_,
      tierings = Prelude.mempty
    }

-- | Specifies a bucket filter. The configuration only includes objects that
-- meet the filter\'s criteria.
intelligentTieringConfiguration_filter :: Lens.Lens' IntelligentTieringConfiguration (Prelude.Maybe IntelligentTieringFilter)
intelligentTieringConfiguration_filter = Lens.lens (\IntelligentTieringConfiguration' {filter'} -> filter') (\s@IntelligentTieringConfiguration' {} a -> s {filter' = a} :: IntelligentTieringConfiguration)

-- | The ID used to identify the S3 Intelligent-Tiering configuration.
intelligentTieringConfiguration_id :: Lens.Lens' IntelligentTieringConfiguration Prelude.Text
intelligentTieringConfiguration_id = Lens.lens (\IntelligentTieringConfiguration' {id} -> id) (\s@IntelligentTieringConfiguration' {} a -> s {id = a} :: IntelligentTieringConfiguration)

-- | Specifies the status of the configuration.
intelligentTieringConfiguration_status :: Lens.Lens' IntelligentTieringConfiguration IntelligentTieringStatus
intelligentTieringConfiguration_status = Lens.lens (\IntelligentTieringConfiguration' {status} -> status) (\s@IntelligentTieringConfiguration' {} a -> s {status = a} :: IntelligentTieringConfiguration)

-- | Specifies the S3 Intelligent-Tiering storage class tier of the
-- configuration.
intelligentTieringConfiguration_tierings :: Lens.Lens' IntelligentTieringConfiguration [Tiering]
intelligentTieringConfiguration_tierings = Lens.lens (\IntelligentTieringConfiguration' {tierings} -> tierings) (\s@IntelligentTieringConfiguration' {} a -> s {tierings = a} :: IntelligentTieringConfiguration) Prelude.. Lens.coerced

instance Data.FromXML IntelligentTieringConfiguration where
  parseXML x =
    IntelligentTieringConfiguration'
      Prelude.<$> (x Data..@? "Filter")
      Prelude.<*> (x Data..@ "Id")
      Prelude.<*> (x Data..@ "Status")
      Prelude.<*> (Data.parseXMLList "Tiering" x)

instance
  Prelude.Hashable
    IntelligentTieringConfiguration
  where
  hashWithSalt
    _salt
    IntelligentTieringConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` id
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` tierings

instance
  Prelude.NFData
    IntelligentTieringConfiguration
  where
  rnf IntelligentTieringConfiguration' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tierings

instance Data.ToXML IntelligentTieringConfiguration where
  toXML IntelligentTieringConfiguration' {..} =
    Prelude.mconcat
      [ "Filter" Data.@= filter',
        "Id" Data.@= id,
        "Status" Data.@= status,
        Data.toXMLList "Tiering" tierings
      ]
