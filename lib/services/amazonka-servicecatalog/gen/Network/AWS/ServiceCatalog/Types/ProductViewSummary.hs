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
-- Module      : Network.AWS.ServiceCatalog.Types.ProductViewSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ProductViewSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.ServiceCatalog.Types.ProductType

-- | Summary information about a product view.
--
-- /See:/ 'newProductViewSummary' smart constructor.
data ProductViewSummary = ProductViewSummary'
  { -- | The owner of the product. Contact the product administrator for the
    -- significance of this value.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The URL information to obtain support for this Product.
    supportUrl :: Prelude.Maybe Prelude.Text,
    -- | Short description of the product.
    shortDescription :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the product has a default path. If the product does
    -- not have a default path, call ListLaunchPaths to disambiguate between
    -- paths. Otherwise, ListLaunchPaths is not required, and the output of
    -- ProductViewSummary can be used directly with
    -- DescribeProvisioningParameters.
    hasDefaultPath :: Prelude.Maybe Prelude.Bool,
    -- | The distributor of the product. Contact the product administrator for
    -- the significance of this value.
    distributor :: Prelude.Maybe Prelude.Text,
    -- | The name of the product.
    name :: Prelude.Maybe Prelude.Text,
    -- | The product view identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The product type. Contact the product administrator for the significance
    -- of this value. If this value is @MARKETPLACE@, the product was created
    -- by AWS Marketplace.
    type' :: Prelude.Maybe ProductType,
    -- | The email contact information to obtain support for this Product.
    supportEmail :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text,
    -- | The description of the support for this Product.
    supportDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ProductViewSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'owner', 'productViewSummary_owner' - The owner of the product. Contact the product administrator for the
-- significance of this value.
--
-- 'supportUrl', 'productViewSummary_supportUrl' - The URL information to obtain support for this Product.
--
-- 'shortDescription', 'productViewSummary_shortDescription' - Short description of the product.
--
-- 'hasDefaultPath', 'productViewSummary_hasDefaultPath' - Indicates whether the product has a default path. If the product does
-- not have a default path, call ListLaunchPaths to disambiguate between
-- paths. Otherwise, ListLaunchPaths is not required, and the output of
-- ProductViewSummary can be used directly with
-- DescribeProvisioningParameters.
--
-- 'distributor', 'productViewSummary_distributor' - The distributor of the product. Contact the product administrator for
-- the significance of this value.
--
-- 'name', 'productViewSummary_name' - The name of the product.
--
-- 'id', 'productViewSummary_id' - The product view identifier.
--
-- 'type'', 'productViewSummary_type' - The product type. Contact the product administrator for the significance
-- of this value. If this value is @MARKETPLACE@, the product was created
-- by AWS Marketplace.
--
-- 'supportEmail', 'productViewSummary_supportEmail' - The email contact information to obtain support for this Product.
--
-- 'productId', 'productViewSummary_productId' - The product identifier.
--
-- 'supportDescription', 'productViewSummary_supportDescription' - The description of the support for this Product.
newProductViewSummary ::
  ProductViewSummary
newProductViewSummary =
  ProductViewSummary'
    { owner = Prelude.Nothing,
      supportUrl = Prelude.Nothing,
      shortDescription = Prelude.Nothing,
      hasDefaultPath = Prelude.Nothing,
      distributor = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      type' = Prelude.Nothing,
      supportEmail = Prelude.Nothing,
      productId = Prelude.Nothing,
      supportDescription = Prelude.Nothing
    }

-- | The owner of the product. Contact the product administrator for the
-- significance of this value.
productViewSummary_owner :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_owner = Lens.lens (\ProductViewSummary' {owner} -> owner) (\s@ProductViewSummary' {} a -> s {owner = a} :: ProductViewSummary)

-- | The URL information to obtain support for this Product.
productViewSummary_supportUrl :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_supportUrl = Lens.lens (\ProductViewSummary' {supportUrl} -> supportUrl) (\s@ProductViewSummary' {} a -> s {supportUrl = a} :: ProductViewSummary)

-- | Short description of the product.
productViewSummary_shortDescription :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_shortDescription = Lens.lens (\ProductViewSummary' {shortDescription} -> shortDescription) (\s@ProductViewSummary' {} a -> s {shortDescription = a} :: ProductViewSummary)

-- | Indicates whether the product has a default path. If the product does
-- not have a default path, call ListLaunchPaths to disambiguate between
-- paths. Otherwise, ListLaunchPaths is not required, and the output of
-- ProductViewSummary can be used directly with
-- DescribeProvisioningParameters.
productViewSummary_hasDefaultPath :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Bool)
productViewSummary_hasDefaultPath = Lens.lens (\ProductViewSummary' {hasDefaultPath} -> hasDefaultPath) (\s@ProductViewSummary' {} a -> s {hasDefaultPath = a} :: ProductViewSummary)

-- | The distributor of the product. Contact the product administrator for
-- the significance of this value.
productViewSummary_distributor :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_distributor = Lens.lens (\ProductViewSummary' {distributor} -> distributor) (\s@ProductViewSummary' {} a -> s {distributor = a} :: ProductViewSummary)

-- | The name of the product.
productViewSummary_name :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_name = Lens.lens (\ProductViewSummary' {name} -> name) (\s@ProductViewSummary' {} a -> s {name = a} :: ProductViewSummary)

-- | The product view identifier.
productViewSummary_id :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_id = Lens.lens (\ProductViewSummary' {id} -> id) (\s@ProductViewSummary' {} a -> s {id = a} :: ProductViewSummary)

-- | The product type. Contact the product administrator for the significance
-- of this value. If this value is @MARKETPLACE@, the product was created
-- by AWS Marketplace.
productViewSummary_type :: Lens.Lens' ProductViewSummary (Prelude.Maybe ProductType)
productViewSummary_type = Lens.lens (\ProductViewSummary' {type'} -> type') (\s@ProductViewSummary' {} a -> s {type' = a} :: ProductViewSummary)

-- | The email contact information to obtain support for this Product.
productViewSummary_supportEmail :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_supportEmail = Lens.lens (\ProductViewSummary' {supportEmail} -> supportEmail) (\s@ProductViewSummary' {} a -> s {supportEmail = a} :: ProductViewSummary)

-- | The product identifier.
productViewSummary_productId :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_productId = Lens.lens (\ProductViewSummary' {productId} -> productId) (\s@ProductViewSummary' {} a -> s {productId = a} :: ProductViewSummary)

-- | The description of the support for this Product.
productViewSummary_supportDescription :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_supportDescription = Lens.lens (\ProductViewSummary' {supportDescription} -> supportDescription) (\s@ProductViewSummary' {} a -> s {supportDescription = a} :: ProductViewSummary)

instance Core.FromJSON ProductViewSummary where
  parseJSON =
    Core.withObject
      "ProductViewSummary"
      ( \x ->
          ProductViewSummary'
            Prelude.<$> (x Core..:? "Owner")
            Prelude.<*> (x Core..:? "SupportUrl")
            Prelude.<*> (x Core..:? "ShortDescription")
            Prelude.<*> (x Core..:? "HasDefaultPath")
            Prelude.<*> (x Core..:? "Distributor")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Type")
            Prelude.<*> (x Core..:? "SupportEmail")
            Prelude.<*> (x Core..:? "ProductId")
            Prelude.<*> (x Core..:? "SupportDescription")
      )

instance Prelude.Hashable ProductViewSummary

instance Prelude.NFData ProductViewSummary
