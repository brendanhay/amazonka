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
-- Module      : Amazonka.Glue.Types.CatalogSchemaChangePolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.CatalogSchemaChangePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Glue.Types.UpdateCatalogBehavior
import qualified Amazonka.Prelude as Prelude

-- | A policy that specifies update behavior for the crawler.
--
-- /See:/ 'newCatalogSchemaChangePolicy' smart constructor.
data CatalogSchemaChangePolicy = CatalogSchemaChangePolicy'
  { -- | The update behavior when the crawler finds a changed schema.
    updateBehavior :: Prelude.Maybe UpdateCatalogBehavior,
    -- | Whether to use the specified update behavior when the crawler finds a
    -- changed schema.
    enableUpdateCatalog :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CatalogSchemaChangePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'updateBehavior', 'catalogSchemaChangePolicy_updateBehavior' - The update behavior when the crawler finds a changed schema.
--
-- 'enableUpdateCatalog', 'catalogSchemaChangePolicy_enableUpdateCatalog' - Whether to use the specified update behavior when the crawler finds a
-- changed schema.
newCatalogSchemaChangePolicy ::
  CatalogSchemaChangePolicy
newCatalogSchemaChangePolicy =
  CatalogSchemaChangePolicy'
    { updateBehavior =
        Prelude.Nothing,
      enableUpdateCatalog = Prelude.Nothing
    }

-- | The update behavior when the crawler finds a changed schema.
catalogSchemaChangePolicy_updateBehavior :: Lens.Lens' CatalogSchemaChangePolicy (Prelude.Maybe UpdateCatalogBehavior)
catalogSchemaChangePolicy_updateBehavior = Lens.lens (\CatalogSchemaChangePolicy' {updateBehavior} -> updateBehavior) (\s@CatalogSchemaChangePolicy' {} a -> s {updateBehavior = a} :: CatalogSchemaChangePolicy)

-- | Whether to use the specified update behavior when the crawler finds a
-- changed schema.
catalogSchemaChangePolicy_enableUpdateCatalog :: Lens.Lens' CatalogSchemaChangePolicy (Prelude.Maybe Prelude.Bool)
catalogSchemaChangePolicy_enableUpdateCatalog = Lens.lens (\CatalogSchemaChangePolicy' {enableUpdateCatalog} -> enableUpdateCatalog) (\s@CatalogSchemaChangePolicy' {} a -> s {enableUpdateCatalog = a} :: CatalogSchemaChangePolicy)

instance Core.FromJSON CatalogSchemaChangePolicy where
  parseJSON =
    Core.withObject
      "CatalogSchemaChangePolicy"
      ( \x ->
          CatalogSchemaChangePolicy'
            Prelude.<$> (x Core..:? "UpdateBehavior")
            Prelude.<*> (x Core..:? "EnableUpdateCatalog")
      )

instance Prelude.Hashable CatalogSchemaChangePolicy where
  hashWithSalt _salt CatalogSchemaChangePolicy' {..} =
    _salt `Prelude.hashWithSalt` updateBehavior
      `Prelude.hashWithSalt` enableUpdateCatalog

instance Prelude.NFData CatalogSchemaChangePolicy where
  rnf CatalogSchemaChangePolicy' {..} =
    Prelude.rnf updateBehavior
      `Prelude.seq` Prelude.rnf enableUpdateCatalog

instance Core.ToJSON CatalogSchemaChangePolicy where
  toJSON CatalogSchemaChangePolicy' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("UpdateBehavior" Core..=)
              Prelude.<$> updateBehavior,
            ("EnableUpdateCatalog" Core..=)
              Prelude.<$> enableUpdateCatalog
          ]
      )
