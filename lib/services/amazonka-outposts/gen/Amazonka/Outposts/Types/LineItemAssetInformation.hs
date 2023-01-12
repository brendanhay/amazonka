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
-- Module      : Amazonka.Outposts.Types.LineItemAssetInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Outposts.Types.LineItemAssetInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a line item asset.
--
-- /See:/ 'newLineItemAssetInformation' smart constructor.
data LineItemAssetInformation = LineItemAssetInformation'
  { -- | The ID of the asset.
    assetId :: Prelude.Maybe Prelude.Text,
    -- | The MAC addresses of the asset.
    macAddressList :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LineItemAssetInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assetId', 'lineItemAssetInformation_assetId' - The ID of the asset.
--
-- 'macAddressList', 'lineItemAssetInformation_macAddressList' - The MAC addresses of the asset.
newLineItemAssetInformation ::
  LineItemAssetInformation
newLineItemAssetInformation =
  LineItemAssetInformation'
    { assetId =
        Prelude.Nothing,
      macAddressList = Prelude.Nothing
    }

-- | The ID of the asset.
lineItemAssetInformation_assetId :: Lens.Lens' LineItemAssetInformation (Prelude.Maybe Prelude.Text)
lineItemAssetInformation_assetId = Lens.lens (\LineItemAssetInformation' {assetId} -> assetId) (\s@LineItemAssetInformation' {} a -> s {assetId = a} :: LineItemAssetInformation)

-- | The MAC addresses of the asset.
lineItemAssetInformation_macAddressList :: Lens.Lens' LineItemAssetInformation (Prelude.Maybe [Prelude.Text])
lineItemAssetInformation_macAddressList = Lens.lens (\LineItemAssetInformation' {macAddressList} -> macAddressList) (\s@LineItemAssetInformation' {} a -> s {macAddressList = a} :: LineItemAssetInformation) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON LineItemAssetInformation where
  parseJSON =
    Data.withObject
      "LineItemAssetInformation"
      ( \x ->
          LineItemAssetInformation'
            Prelude.<$> (x Data..:? "AssetId")
            Prelude.<*> ( x Data..:? "MacAddressList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable LineItemAssetInformation where
  hashWithSalt _salt LineItemAssetInformation' {..} =
    _salt `Prelude.hashWithSalt` assetId
      `Prelude.hashWithSalt` macAddressList

instance Prelude.NFData LineItemAssetInformation where
  rnf LineItemAssetInformation' {..} =
    Prelude.rnf assetId
      `Prelude.seq` Prelude.rnf macAddressList
