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
-- Module      : Amazonka.EKS.Types.MarketplaceInformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.MarketplaceInformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about an Amazon EKS add-on from the Amazon Web Services
-- Marketplace.
--
-- /See:/ 'newMarketplaceInformation' smart constructor.
data MarketplaceInformation = MarketplaceInformation'
  { -- | The product ID from the Amazon Web Services Marketplace.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The product URL from the Amazon Web Services Marketplace.
    productUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MarketplaceInformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productId', 'marketplaceInformation_productId' - The product ID from the Amazon Web Services Marketplace.
--
-- 'productUrl', 'marketplaceInformation_productUrl' - The product URL from the Amazon Web Services Marketplace.
newMarketplaceInformation ::
  MarketplaceInformation
newMarketplaceInformation =
  MarketplaceInformation'
    { productId =
        Prelude.Nothing,
      productUrl = Prelude.Nothing
    }

-- | The product ID from the Amazon Web Services Marketplace.
marketplaceInformation_productId :: Lens.Lens' MarketplaceInformation (Prelude.Maybe Prelude.Text)
marketplaceInformation_productId = Lens.lens (\MarketplaceInformation' {productId} -> productId) (\s@MarketplaceInformation' {} a -> s {productId = a} :: MarketplaceInformation)

-- | The product URL from the Amazon Web Services Marketplace.
marketplaceInformation_productUrl :: Lens.Lens' MarketplaceInformation (Prelude.Maybe Prelude.Text)
marketplaceInformation_productUrl = Lens.lens (\MarketplaceInformation' {productUrl} -> productUrl) (\s@MarketplaceInformation' {} a -> s {productUrl = a} :: MarketplaceInformation)

instance Data.FromJSON MarketplaceInformation where
  parseJSON =
    Data.withObject
      "MarketplaceInformation"
      ( \x ->
          MarketplaceInformation'
            Prelude.<$> (x Data..:? "productId")
            Prelude.<*> (x Data..:? "productUrl")
      )

instance Prelude.Hashable MarketplaceInformation where
  hashWithSalt _salt MarketplaceInformation' {..} =
    _salt
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` productUrl

instance Prelude.NFData MarketplaceInformation where
  rnf MarketplaceInformation' {..} =
    Prelude.rnf productId
      `Prelude.seq` Prelude.rnf productUrl
