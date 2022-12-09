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
-- Module      : Amazonka.SecurityHub.Types.Product
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Product where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.IntegrationType

-- | Contains details about a product.
--
-- /See:/ 'newProduct' smart constructor.
data Product = Product'
  { -- | The URL to the service or product documentation about the integration
    -- with Security Hub, including how to activate the integration.
    activationUrl :: Prelude.Maybe Prelude.Text,
    -- | The categories assigned to the product.
    categories :: Prelude.Maybe [Prelude.Text],
    -- | The name of the company that provides the product.
    companyName :: Prelude.Maybe Prelude.Text,
    -- | A description of the product.
    description :: Prelude.Maybe Prelude.Text,
    -- | The types of integration that the product supports. Available values are
    -- the following.
    --
    -- -   @SEND_FINDINGS_TO_SECURITY_HUB@ - The integration sends findings to
    --     Security Hub.
    --
    -- -   @RECEIVE_FINDINGS_FROM_SECURITY_HUB@ - The integration receives
    --     findings from Security Hub.
    --
    -- -   @UPDATE_FINDINGS_IN_SECURITY_HUB@ - The integration does not send
    --     new findings to Security Hub, but does make updates to the findings
    --     that it receives from Security Hub.
    integrationTypes :: Prelude.Maybe [IntegrationType],
    -- | For integrations with Amazon Web Services services, the Amazon Web
    -- Services Console URL from which to activate the service.
    --
    -- For integrations with third-party products, the Amazon Web Services
    -- Marketplace URL from which to subscribe to or purchase the product.
    marketplaceUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the product.
    productName :: Prelude.Maybe Prelude.Text,
    -- | The resource policy associated with the product.
    productSubscriptionResourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The ARN assigned to the product.
    productArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Product' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activationUrl', 'product_activationUrl' - The URL to the service or product documentation about the integration
-- with Security Hub, including how to activate the integration.
--
-- 'categories', 'product_categories' - The categories assigned to the product.
--
-- 'companyName', 'product_companyName' - The name of the company that provides the product.
--
-- 'description', 'product_description' - A description of the product.
--
-- 'integrationTypes', 'product_integrationTypes' - The types of integration that the product supports. Available values are
-- the following.
--
-- -   @SEND_FINDINGS_TO_SECURITY_HUB@ - The integration sends findings to
--     Security Hub.
--
-- -   @RECEIVE_FINDINGS_FROM_SECURITY_HUB@ - The integration receives
--     findings from Security Hub.
--
-- -   @UPDATE_FINDINGS_IN_SECURITY_HUB@ - The integration does not send
--     new findings to Security Hub, but does make updates to the findings
--     that it receives from Security Hub.
--
-- 'marketplaceUrl', 'product_marketplaceUrl' - For integrations with Amazon Web Services services, the Amazon Web
-- Services Console URL from which to activate the service.
--
-- For integrations with third-party products, the Amazon Web Services
-- Marketplace URL from which to subscribe to or purchase the product.
--
-- 'productName', 'product_productName' - The name of the product.
--
-- 'productSubscriptionResourcePolicy', 'product_productSubscriptionResourcePolicy' - The resource policy associated with the product.
--
-- 'productArn', 'product_productArn' - The ARN assigned to the product.
newProduct ::
  -- | 'productArn'
  Prelude.Text ->
  Product
newProduct pProductArn_ =
  Product'
    { activationUrl = Prelude.Nothing,
      categories = Prelude.Nothing,
      companyName = Prelude.Nothing,
      description = Prelude.Nothing,
      integrationTypes = Prelude.Nothing,
      marketplaceUrl = Prelude.Nothing,
      productName = Prelude.Nothing,
      productSubscriptionResourcePolicy = Prelude.Nothing,
      productArn = pProductArn_
    }

-- | The URL to the service or product documentation about the integration
-- with Security Hub, including how to activate the integration.
product_activationUrl :: Lens.Lens' Product (Prelude.Maybe Prelude.Text)
product_activationUrl = Lens.lens (\Product' {activationUrl} -> activationUrl) (\s@Product' {} a -> s {activationUrl = a} :: Product)

-- | The categories assigned to the product.
product_categories :: Lens.Lens' Product (Prelude.Maybe [Prelude.Text])
product_categories = Lens.lens (\Product' {categories} -> categories) (\s@Product' {} a -> s {categories = a} :: Product) Prelude.. Lens.mapping Lens.coerced

-- | The name of the company that provides the product.
product_companyName :: Lens.Lens' Product (Prelude.Maybe Prelude.Text)
product_companyName = Lens.lens (\Product' {companyName} -> companyName) (\s@Product' {} a -> s {companyName = a} :: Product)

-- | A description of the product.
product_description :: Lens.Lens' Product (Prelude.Maybe Prelude.Text)
product_description = Lens.lens (\Product' {description} -> description) (\s@Product' {} a -> s {description = a} :: Product)

-- | The types of integration that the product supports. Available values are
-- the following.
--
-- -   @SEND_FINDINGS_TO_SECURITY_HUB@ - The integration sends findings to
--     Security Hub.
--
-- -   @RECEIVE_FINDINGS_FROM_SECURITY_HUB@ - The integration receives
--     findings from Security Hub.
--
-- -   @UPDATE_FINDINGS_IN_SECURITY_HUB@ - The integration does not send
--     new findings to Security Hub, but does make updates to the findings
--     that it receives from Security Hub.
product_integrationTypes :: Lens.Lens' Product (Prelude.Maybe [IntegrationType])
product_integrationTypes = Lens.lens (\Product' {integrationTypes} -> integrationTypes) (\s@Product' {} a -> s {integrationTypes = a} :: Product) Prelude.. Lens.mapping Lens.coerced

-- | For integrations with Amazon Web Services services, the Amazon Web
-- Services Console URL from which to activate the service.
--
-- For integrations with third-party products, the Amazon Web Services
-- Marketplace URL from which to subscribe to or purchase the product.
product_marketplaceUrl :: Lens.Lens' Product (Prelude.Maybe Prelude.Text)
product_marketplaceUrl = Lens.lens (\Product' {marketplaceUrl} -> marketplaceUrl) (\s@Product' {} a -> s {marketplaceUrl = a} :: Product)

-- | The name of the product.
product_productName :: Lens.Lens' Product (Prelude.Maybe Prelude.Text)
product_productName = Lens.lens (\Product' {productName} -> productName) (\s@Product' {} a -> s {productName = a} :: Product)

-- | The resource policy associated with the product.
product_productSubscriptionResourcePolicy :: Lens.Lens' Product (Prelude.Maybe Prelude.Text)
product_productSubscriptionResourcePolicy = Lens.lens (\Product' {productSubscriptionResourcePolicy} -> productSubscriptionResourcePolicy) (\s@Product' {} a -> s {productSubscriptionResourcePolicy = a} :: Product)

-- | The ARN assigned to the product.
product_productArn :: Lens.Lens' Product Prelude.Text
product_productArn = Lens.lens (\Product' {productArn} -> productArn) (\s@Product' {} a -> s {productArn = a} :: Product)

instance Data.FromJSON Product where
  parseJSON =
    Data.withObject
      "Product"
      ( \x ->
          Product'
            Prelude.<$> (x Data..:? "ActivationUrl")
            Prelude.<*> (x Data..:? "Categories" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "CompanyName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> ( x Data..:? "IntegrationTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "MarketplaceUrl")
            Prelude.<*> (x Data..:? "ProductName")
            Prelude.<*> (x Data..:? "ProductSubscriptionResourcePolicy")
            Prelude.<*> (x Data..: "ProductArn")
      )

instance Prelude.Hashable Product where
  hashWithSalt _salt Product' {..} =
    _salt `Prelude.hashWithSalt` activationUrl
      `Prelude.hashWithSalt` categories
      `Prelude.hashWithSalt` companyName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` integrationTypes
      `Prelude.hashWithSalt` marketplaceUrl
      `Prelude.hashWithSalt` productName
      `Prelude.hashWithSalt` productSubscriptionResourcePolicy
      `Prelude.hashWithSalt` productArn

instance Prelude.NFData Product where
  rnf Product' {..} =
    Prelude.rnf activationUrl
      `Prelude.seq` Prelude.rnf categories
      `Prelude.seq` Prelude.rnf companyName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf integrationTypes
      `Prelude.seq` Prelude.rnf marketplaceUrl
      `Prelude.seq` Prelude.rnf productName
      `Prelude.seq` Prelude.rnf productSubscriptionResourcePolicy
      `Prelude.seq` Prelude.rnf productArn
