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
-- Module      : Amazonka.CodeArtifact.Types.AssetSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeArtifact.Types.AssetSummary where

import Amazonka.CodeArtifact.Types.HashAlgorithm
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains details about a package version asset.
--
-- /See:/ 'newAssetSummary' smart constructor.
data AssetSummary = AssetSummary'
  { -- | The hashes of the asset.
    hashes :: Prelude.Maybe (Prelude.HashMap HashAlgorithm Prelude.Text),
    -- | The size of the asset.
    size :: Prelude.Maybe Prelude.Integer,
    -- | The name of the asset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hashes', 'assetSummary_hashes' - The hashes of the asset.
--
-- 'size', 'assetSummary_size' - The size of the asset.
--
-- 'name', 'assetSummary_name' - The name of the asset.
newAssetSummary ::
  -- | 'name'
  Prelude.Text ->
  AssetSummary
newAssetSummary pName_ =
  AssetSummary'
    { hashes = Prelude.Nothing,
      size = Prelude.Nothing,
      name = pName_
    }

-- | The hashes of the asset.
assetSummary_hashes :: Lens.Lens' AssetSummary (Prelude.Maybe (Prelude.HashMap HashAlgorithm Prelude.Text))
assetSummary_hashes = Lens.lens (\AssetSummary' {hashes} -> hashes) (\s@AssetSummary' {} a -> s {hashes = a} :: AssetSummary) Prelude.. Lens.mapping Lens.coerced

-- | The size of the asset.
assetSummary_size :: Lens.Lens' AssetSummary (Prelude.Maybe Prelude.Integer)
assetSummary_size = Lens.lens (\AssetSummary' {size} -> size) (\s@AssetSummary' {} a -> s {size = a} :: AssetSummary)

-- | The name of the asset.
assetSummary_name :: Lens.Lens' AssetSummary Prelude.Text
assetSummary_name = Lens.lens (\AssetSummary' {name} -> name) (\s@AssetSummary' {} a -> s {name = a} :: AssetSummary)

instance Data.FromJSON AssetSummary where
  parseJSON =
    Data.withObject
      "AssetSummary"
      ( \x ->
          AssetSummary'
            Prelude.<$> (x Data..:? "hashes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "size")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable AssetSummary where
  hashWithSalt _salt AssetSummary' {..} =
    _salt
      `Prelude.hashWithSalt` hashes
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` name

instance Prelude.NFData AssetSummary where
  rnf AssetSummary' {..} =
    Prelude.rnf hashes `Prelude.seq`
      Prelude.rnf size `Prelude.seq`
        Prelude.rnf name
