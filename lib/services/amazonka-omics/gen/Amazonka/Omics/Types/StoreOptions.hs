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
-- Module      : Amazonka.Omics.Types.StoreOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.StoreOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.TsvStoreOptions
import qualified Amazonka.Prelude as Prelude

-- | Settings for a store.
--
-- /See:/ 'newStoreOptions' smart constructor.
data StoreOptions = StoreOptions'
  { -- | File settings for a TSV store.
    tsvStoreOptions :: Prelude.Maybe TsvStoreOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StoreOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tsvStoreOptions', 'storeOptions_tsvStoreOptions' - File settings for a TSV store.
newStoreOptions ::
  StoreOptions
newStoreOptions =
  StoreOptions' {tsvStoreOptions = Prelude.Nothing}

-- | File settings for a TSV store.
storeOptions_tsvStoreOptions :: Lens.Lens' StoreOptions (Prelude.Maybe TsvStoreOptions)
storeOptions_tsvStoreOptions = Lens.lens (\StoreOptions' {tsvStoreOptions} -> tsvStoreOptions) (\s@StoreOptions' {} a -> s {tsvStoreOptions = a} :: StoreOptions)

instance Data.FromJSON StoreOptions where
  parseJSON =
    Data.withObject
      "StoreOptions"
      ( \x ->
          StoreOptions'
            Prelude.<$> (x Data..:? "tsvStoreOptions")
      )

instance Prelude.Hashable StoreOptions where
  hashWithSalt _salt StoreOptions' {..} =
    _salt `Prelude.hashWithSalt` tsvStoreOptions

instance Prelude.NFData StoreOptions where
  rnf StoreOptions' {..} = Prelude.rnf tsvStoreOptions

instance Data.ToJSON StoreOptions where
  toJSON StoreOptions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("tsvStoreOptions" Data..=)
              Prelude.<$> tsvStoreOptions
          ]
      )
