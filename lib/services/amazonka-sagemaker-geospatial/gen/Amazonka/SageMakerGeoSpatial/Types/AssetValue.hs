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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.AssetValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.AssetValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The structure containing the asset properties.
--
-- /See:/ 'newAssetValue' smart constructor.
data AssetValue = AssetValue'
  { -- | Link to the asset object.
    href :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'href', 'assetValue_href' - Link to the asset object.
newAssetValue ::
  AssetValue
newAssetValue = AssetValue' {href = Prelude.Nothing}

-- | Link to the asset object.
assetValue_href :: Lens.Lens' AssetValue (Prelude.Maybe Prelude.Text)
assetValue_href = Lens.lens (\AssetValue' {href} -> href) (\s@AssetValue' {} a -> s {href = a} :: AssetValue)

instance Data.FromJSON AssetValue where
  parseJSON =
    Data.withObject
      "AssetValue"
      (\x -> AssetValue' Prelude.<$> (x Data..:? "Href"))

instance Prelude.Hashable AssetValue where
  hashWithSalt _salt AssetValue' {..} =
    _salt `Prelude.hashWithSalt` href

instance Prelude.NFData AssetValue where
  rnf AssetValue' {..} = Prelude.rnf href
