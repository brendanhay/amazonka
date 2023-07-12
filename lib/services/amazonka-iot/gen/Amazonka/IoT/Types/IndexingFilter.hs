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
-- Module      : Amazonka.IoT.Types.IndexingFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.IndexingFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides additional filters for specific data sources. Named shadow is
-- the only data source that currently supports and requires a filter. To
-- add named shadows to your fleet indexing configuration, set
-- @namedShadowIndexingMode@ to be @ON@ and specify your shadow names in
-- @filter@.
--
-- /See:/ 'newIndexingFilter' smart constructor.
data IndexingFilter = IndexingFilter'
  { -- | The shadow names that you select to index. The default maximum number of
    -- shadow names for indexing is 10. To increase the limit, see
    -- <https://docs.aws.amazon.com/general/latest/gr/iot_device_management.html#fleet-indexing-limits Amazon Web Services IoT Device Management Quotas>
    -- in the /Amazon Web Services General Reference/.
    namedShadowNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexingFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedShadowNames', 'indexingFilter_namedShadowNames' - The shadow names that you select to index. The default maximum number of
-- shadow names for indexing is 10. To increase the limit, see
-- <https://docs.aws.amazon.com/general/latest/gr/iot_device_management.html#fleet-indexing-limits Amazon Web Services IoT Device Management Quotas>
-- in the /Amazon Web Services General Reference/.
newIndexingFilter ::
  IndexingFilter
newIndexingFilter =
  IndexingFilter' {namedShadowNames = Prelude.Nothing}

-- | The shadow names that you select to index. The default maximum number of
-- shadow names for indexing is 10. To increase the limit, see
-- <https://docs.aws.amazon.com/general/latest/gr/iot_device_management.html#fleet-indexing-limits Amazon Web Services IoT Device Management Quotas>
-- in the /Amazon Web Services General Reference/.
indexingFilter_namedShadowNames :: Lens.Lens' IndexingFilter (Prelude.Maybe [Prelude.Text])
indexingFilter_namedShadowNames = Lens.lens (\IndexingFilter' {namedShadowNames} -> namedShadowNames) (\s@IndexingFilter' {} a -> s {namedShadowNames = a} :: IndexingFilter) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON IndexingFilter where
  parseJSON =
    Data.withObject
      "IndexingFilter"
      ( \x ->
          IndexingFilter'
            Prelude.<$> ( x
                            Data..:? "namedShadowNames"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable IndexingFilter where
  hashWithSalt _salt IndexingFilter' {..} =
    _salt `Prelude.hashWithSalt` namedShadowNames

instance Prelude.NFData IndexingFilter where
  rnf IndexingFilter' {..} =
    Prelude.rnf namedShadowNames

instance Data.ToJSON IndexingFilter where
  toJSON IndexingFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("namedShadowNames" Data..=)
              Prelude.<$> namedShadowNames
          ]
      )
