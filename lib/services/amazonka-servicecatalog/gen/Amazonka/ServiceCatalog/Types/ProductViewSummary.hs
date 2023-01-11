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
-- Module      : Amazonka.ServiceCatalog.Types.ProductViewSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.ProductViewSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.ServiceCatalog.Types.ProductType

-- | Summary information about a product view.
--
-- /See:/ 'newProductViewSummary' smart constructor.
data ProductViewSummary = ProductViewSummary'
  { -- | The distributor of the product. Contact the product administrator for
    -- the significance of this value.
    distributor :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the product has a default path. If the product does
    -- not have a default path, call ListLaunchPaths to disambiguate between
    -- paths. Otherwise, ListLaunchPaths is not required, and the output of
    -- ProductViewSummary can be used directly with
    -- DescribeProvisioningParameters.
    hasDefaultPath :: Prelude.Maybe Prelude.Bool,
    -- | The product view identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the product.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the product. Contact the product administrator for the
    -- significance of this value.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The product identifier.
    productId :: Prelude.Maybe Prelude.Text,
    -- | Short description of the product.
    shortDescription :: Prelude.Maybe Prelude.Text,
    -- | The description of the support for this Product.
    supportDescription :: Prelude.Maybe Prelude.Text,
    -- | The email contact information to obtain support for this Product.
    supportEmail :: Prelude.Maybe Prelude.Text,
    -- | The URL information to obtain support for this Product.
    supportUrl :: Prelude.Maybe Prelude.Text,
    -- | The product type. Contact the product administrator for the significance
    -- of this value. If this value is @MARKETPLACE@, the product was created
    -- by Amazon Web Services Marketplace.
    type' :: Prelude.Maybe ProductType
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
-- 'distributor', 'productViewSummary_distributor' - The distributor of the product. Contact the product administrator for
-- the significance of this value.
--
-- 'hasDefaultPath', 'productViewSummary_hasDefaultPath' - Indicates whether the product has a default path. If the product does
-- not have a default path, call ListLaunchPaths to disambiguate between
-- paths. Otherwise, ListLaunchPaths is not required, and the output of
-- ProductViewSummary can be used directly with
-- DescribeProvisioningParameters.
--
-- 'id', 'productViewSummary_id' - The product view identifier.
--
-- 'name', 'productViewSummary_name' - The name of the product.
--
-- 'owner', 'productViewSummary_owner' - The owner of the product. Contact the product administrator for the
-- significance of this value.
--
-- 'productId', 'productViewSummary_productId' - The product identifier.
--
-- 'shortDescription', 'productViewSummary_shortDescription' - Short description of the product.
--
-- 'supportDescription', 'productViewSummary_supportDescription' - The description of the support for this Product.
--
-- 'supportEmail', 'productViewSummary_supportEmail' - The email contact information to obtain support for this Product.
--
-- 'supportUrl', 'productViewSummary_supportUrl' - The URL information to obtain support for this Product.
--
-- 'type'', 'productViewSummary_type' - The product type. Contact the product administrator for the significance
-- of this value. If this value is @MARKETPLACE@, the product was created
-- by Amazon Web Services Marketplace.
newProductViewSummary ::
  ProductViewSummary
newProductViewSummary =
  ProductViewSummary'
    { distributor = Prelude.Nothing,
      hasDefaultPath = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      productId = Prelude.Nothing,
      shortDescription = Prelude.Nothing,
      supportDescription = Prelude.Nothing,
      supportEmail = Prelude.Nothing,
      supportUrl = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The distributor of the product. Contact the product administrator for
-- the significance of this value.
productViewSummary_distributor :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_distributor = Lens.lens (\ProductViewSummary' {distributor} -> distributor) (\s@ProductViewSummary' {} a -> s {distributor = a} :: ProductViewSummary)

-- | Indicates whether the product has a default path. If the product does
-- not have a default path, call ListLaunchPaths to disambiguate between
-- paths. Otherwise, ListLaunchPaths is not required, and the output of
-- ProductViewSummary can be used directly with
-- DescribeProvisioningParameters.
productViewSummary_hasDefaultPath :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Bool)
productViewSummary_hasDefaultPath = Lens.lens (\ProductViewSummary' {hasDefaultPath} -> hasDefaultPath) (\s@ProductViewSummary' {} a -> s {hasDefaultPath = a} :: ProductViewSummary)

-- | The product view identifier.
productViewSummary_id :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_id = Lens.lens (\ProductViewSummary' {id} -> id) (\s@ProductViewSummary' {} a -> s {id = a} :: ProductViewSummary)

-- | The name of the product.
productViewSummary_name :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_name = Lens.lens (\ProductViewSummary' {name} -> name) (\s@ProductViewSummary' {} a -> s {name = a} :: ProductViewSummary)

-- | The owner of the product. Contact the product administrator for the
-- significance of this value.
productViewSummary_owner :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_owner = Lens.lens (\ProductViewSummary' {owner} -> owner) (\s@ProductViewSummary' {} a -> s {owner = a} :: ProductViewSummary)

-- | The product identifier.
productViewSummary_productId :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_productId = Lens.lens (\ProductViewSummary' {productId} -> productId) (\s@ProductViewSummary' {} a -> s {productId = a} :: ProductViewSummary)

-- | Short description of the product.
productViewSummary_shortDescription :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_shortDescription = Lens.lens (\ProductViewSummary' {shortDescription} -> shortDescription) (\s@ProductViewSummary' {} a -> s {shortDescription = a} :: ProductViewSummary)

-- | The description of the support for this Product.
productViewSummary_supportDescription :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_supportDescription = Lens.lens (\ProductViewSummary' {supportDescription} -> supportDescription) (\s@ProductViewSummary' {} a -> s {supportDescription = a} :: ProductViewSummary)

-- | The email contact information to obtain support for this Product.
productViewSummary_supportEmail :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_supportEmail = Lens.lens (\ProductViewSummary' {supportEmail} -> supportEmail) (\s@ProductViewSummary' {} a -> s {supportEmail = a} :: ProductViewSummary)

-- | The URL information to obtain support for this Product.
productViewSummary_supportUrl :: Lens.Lens' ProductViewSummary (Prelude.Maybe Prelude.Text)
productViewSummary_supportUrl = Lens.lens (\ProductViewSummary' {supportUrl} -> supportUrl) (\s@ProductViewSummary' {} a -> s {supportUrl = a} :: ProductViewSummary)

-- | The product type. Contact the product administrator for the significance
-- of this value. If this value is @MARKETPLACE@, the product was created
-- by Amazon Web Services Marketplace.
productViewSummary_type :: Lens.Lens' ProductViewSummary (Prelude.Maybe ProductType)
productViewSummary_type = Lens.lens (\ProductViewSummary' {type'} -> type') (\s@ProductViewSummary' {} a -> s {type' = a} :: ProductViewSummary)

instance Data.FromJSON ProductViewSummary where
  parseJSON =
    Data.withObject
      "ProductViewSummary"
      ( \x ->
          ProductViewSummary'
            Prelude.<$> (x Data..:? "Distributor")
            Prelude.<*> (x Data..:? "HasDefaultPath")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "ProductId")
            Prelude.<*> (x Data..:? "ShortDescription")
            Prelude.<*> (x Data..:? "SupportDescription")
            Prelude.<*> (x Data..:? "SupportEmail")
            Prelude.<*> (x Data..:? "SupportUrl")
            Prelude.<*> (x Data..:? "Type")
      )

instance Prelude.Hashable ProductViewSummary where
  hashWithSalt _salt ProductViewSummary' {..} =
    _salt `Prelude.hashWithSalt` distributor
      `Prelude.hashWithSalt` hasDefaultPath
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` shortDescription
      `Prelude.hashWithSalt` supportDescription
      `Prelude.hashWithSalt` supportEmail
      `Prelude.hashWithSalt` supportUrl
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ProductViewSummary where
  rnf ProductViewSummary' {..} =
    Prelude.rnf distributor
      `Prelude.seq` Prelude.rnf hasDefaultPath
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf shortDescription
      `Prelude.seq` Prelude.rnf supportDescription
      `Prelude.seq` Prelude.rnf supportEmail
      `Prelude.seq` Prelude.rnf supportUrl
      `Prelude.seq` Prelude.rnf type'
