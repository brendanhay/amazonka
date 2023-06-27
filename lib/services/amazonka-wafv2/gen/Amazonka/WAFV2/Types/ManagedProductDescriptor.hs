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
-- Module      : Amazonka.WAFV2.Types.ManagedProductDescriptor
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WAFV2.Types.ManagedProductDescriptor where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The properties of a managed product, such as an Amazon Web Services
-- Managed Rules rule group or an Amazon Web Services Marketplace managed
-- rule group.
--
-- /See:/ 'newManagedProductDescriptor' smart constructor.
data ManagedProductDescriptor = ManagedProductDescriptor'
  { -- | Indicates whether the rule group provides an advanced set of
    -- protections, such as the the Amazon Web Services Managed Rules rule
    -- groups that are used for WAF intelligent threat mitigation.
    isAdvancedManagedRuleSet :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the rule group is versioned.
    isVersioningSupported :: Prelude.Maybe Prelude.Bool,
    -- | The name of the managed rule group. For example,
    -- @AWSManagedRulesAnonymousIpList@ or @AWSManagedRulesATPRuleSet@.
    managedRuleSetName :: Prelude.Maybe Prelude.Text,
    -- | A short description of the managed rule group.
    productDescription :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the rule group. This ID is returned in the
    -- responses to create and list commands. You provide it to operations like
    -- update and delete.
    productId :: Prelude.Maybe Prelude.Text,
    -- | For Amazon Web Services Marketplace managed rule groups only, the link
    -- to the rule group product page.
    productLink :: Prelude.Maybe Prelude.Text,
    -- | The display name for the managed rule group. For example,
    -- @Anonymous IP list@ or @Account takeover prevention@.
    productTitle :: Prelude.Maybe Prelude.Text,
    -- | The Amazon resource name (ARN) of the Amazon Simple Notification Service
    -- SNS topic that\'s used to provide notification of changes to the managed
    -- rule group. You can subscribe to the SNS topic to receive notifications
    -- when the managed rule group is modified, such as for new versions and
    -- for version expiration. For more information, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide>.
    snsTopicArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the managed rule group vendor. You use this, along with the
    -- rule group name, to identify a rule group.
    vendorName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ManagedProductDescriptor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'isAdvancedManagedRuleSet', 'managedProductDescriptor_isAdvancedManagedRuleSet' - Indicates whether the rule group provides an advanced set of
-- protections, such as the the Amazon Web Services Managed Rules rule
-- groups that are used for WAF intelligent threat mitigation.
--
-- 'isVersioningSupported', 'managedProductDescriptor_isVersioningSupported' - Indicates whether the rule group is versioned.
--
-- 'managedRuleSetName', 'managedProductDescriptor_managedRuleSetName' - The name of the managed rule group. For example,
-- @AWSManagedRulesAnonymousIpList@ or @AWSManagedRulesATPRuleSet@.
--
-- 'productDescription', 'managedProductDescriptor_productDescription' - A short description of the managed rule group.
--
-- 'productId', 'managedProductDescriptor_productId' - A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
--
-- 'productLink', 'managedProductDescriptor_productLink' - For Amazon Web Services Marketplace managed rule groups only, the link
-- to the rule group product page.
--
-- 'productTitle', 'managedProductDescriptor_productTitle' - The display name for the managed rule group. For example,
-- @Anonymous IP list@ or @Account takeover prevention@.
--
-- 'snsTopicArn', 'managedProductDescriptor_snsTopicArn' - The Amazon resource name (ARN) of the Amazon Simple Notification Service
-- SNS topic that\'s used to provide notification of changes to the managed
-- rule group. You can subscribe to the SNS topic to receive notifications
-- when the managed rule group is modified, such as for new versions and
-- for version expiration. For more information, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide>.
--
-- 'vendorName', 'managedProductDescriptor_vendorName' - The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify a rule group.
newManagedProductDescriptor ::
  ManagedProductDescriptor
newManagedProductDescriptor =
  ManagedProductDescriptor'
    { isAdvancedManagedRuleSet =
        Prelude.Nothing,
      isVersioningSupported = Prelude.Nothing,
      managedRuleSetName = Prelude.Nothing,
      productDescription = Prelude.Nothing,
      productId = Prelude.Nothing,
      productLink = Prelude.Nothing,
      productTitle = Prelude.Nothing,
      snsTopicArn = Prelude.Nothing,
      vendorName = Prelude.Nothing
    }

-- | Indicates whether the rule group provides an advanced set of
-- protections, such as the the Amazon Web Services Managed Rules rule
-- groups that are used for WAF intelligent threat mitigation.
managedProductDescriptor_isAdvancedManagedRuleSet :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Bool)
managedProductDescriptor_isAdvancedManagedRuleSet = Lens.lens (\ManagedProductDescriptor' {isAdvancedManagedRuleSet} -> isAdvancedManagedRuleSet) (\s@ManagedProductDescriptor' {} a -> s {isAdvancedManagedRuleSet = a} :: ManagedProductDescriptor)

-- | Indicates whether the rule group is versioned.
managedProductDescriptor_isVersioningSupported :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Bool)
managedProductDescriptor_isVersioningSupported = Lens.lens (\ManagedProductDescriptor' {isVersioningSupported} -> isVersioningSupported) (\s@ManagedProductDescriptor' {} a -> s {isVersioningSupported = a} :: ManagedProductDescriptor)

-- | The name of the managed rule group. For example,
-- @AWSManagedRulesAnonymousIpList@ or @AWSManagedRulesATPRuleSet@.
managedProductDescriptor_managedRuleSetName :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_managedRuleSetName = Lens.lens (\ManagedProductDescriptor' {managedRuleSetName} -> managedRuleSetName) (\s@ManagedProductDescriptor' {} a -> s {managedRuleSetName = a} :: ManagedProductDescriptor)

-- | A short description of the managed rule group.
managedProductDescriptor_productDescription :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_productDescription = Lens.lens (\ManagedProductDescriptor' {productDescription} -> productDescription) (\s@ManagedProductDescriptor' {} a -> s {productDescription = a} :: ManagedProductDescriptor)

-- | A unique identifier for the rule group. This ID is returned in the
-- responses to create and list commands. You provide it to operations like
-- update and delete.
managedProductDescriptor_productId :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_productId = Lens.lens (\ManagedProductDescriptor' {productId} -> productId) (\s@ManagedProductDescriptor' {} a -> s {productId = a} :: ManagedProductDescriptor)

-- | For Amazon Web Services Marketplace managed rule groups only, the link
-- to the rule group product page.
managedProductDescriptor_productLink :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_productLink = Lens.lens (\ManagedProductDescriptor' {productLink} -> productLink) (\s@ManagedProductDescriptor' {} a -> s {productLink = a} :: ManagedProductDescriptor)

-- | The display name for the managed rule group. For example,
-- @Anonymous IP list@ or @Account takeover prevention@.
managedProductDescriptor_productTitle :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_productTitle = Lens.lens (\ManagedProductDescriptor' {productTitle} -> productTitle) (\s@ManagedProductDescriptor' {} a -> s {productTitle = a} :: ManagedProductDescriptor)

-- | The Amazon resource name (ARN) of the Amazon Simple Notification Service
-- SNS topic that\'s used to provide notification of changes to the managed
-- rule group. You can subscribe to the SNS topic to receive notifications
-- when the managed rule group is modified, such as for new versions and
-- for version expiration. For more information, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/welcome.html Amazon Simple Notification Service Developer Guide>.
managedProductDescriptor_snsTopicArn :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_snsTopicArn = Lens.lens (\ManagedProductDescriptor' {snsTopicArn} -> snsTopicArn) (\s@ManagedProductDescriptor' {} a -> s {snsTopicArn = a} :: ManagedProductDescriptor)

-- | The name of the managed rule group vendor. You use this, along with the
-- rule group name, to identify a rule group.
managedProductDescriptor_vendorName :: Lens.Lens' ManagedProductDescriptor (Prelude.Maybe Prelude.Text)
managedProductDescriptor_vendorName = Lens.lens (\ManagedProductDescriptor' {vendorName} -> vendorName) (\s@ManagedProductDescriptor' {} a -> s {vendorName = a} :: ManagedProductDescriptor)

instance Data.FromJSON ManagedProductDescriptor where
  parseJSON =
    Data.withObject
      "ManagedProductDescriptor"
      ( \x ->
          ManagedProductDescriptor'
            Prelude.<$> (x Data..:? "IsAdvancedManagedRuleSet")
            Prelude.<*> (x Data..:? "IsVersioningSupported")
            Prelude.<*> (x Data..:? "ManagedRuleSetName")
            Prelude.<*> (x Data..:? "ProductDescription")
            Prelude.<*> (x Data..:? "ProductId")
            Prelude.<*> (x Data..:? "ProductLink")
            Prelude.<*> (x Data..:? "ProductTitle")
            Prelude.<*> (x Data..:? "SnsTopicArn")
            Prelude.<*> (x Data..:? "VendorName")
      )

instance Prelude.Hashable ManagedProductDescriptor where
  hashWithSalt _salt ManagedProductDescriptor' {..} =
    _salt
      `Prelude.hashWithSalt` isAdvancedManagedRuleSet
      `Prelude.hashWithSalt` isVersioningSupported
      `Prelude.hashWithSalt` managedRuleSetName
      `Prelude.hashWithSalt` productDescription
      `Prelude.hashWithSalt` productId
      `Prelude.hashWithSalt` productLink
      `Prelude.hashWithSalt` productTitle
      `Prelude.hashWithSalt` snsTopicArn
      `Prelude.hashWithSalt` vendorName

instance Prelude.NFData ManagedProductDescriptor where
  rnf ManagedProductDescriptor' {..} =
    Prelude.rnf isAdvancedManagedRuleSet
      `Prelude.seq` Prelude.rnf isVersioningSupported
      `Prelude.seq` Prelude.rnf managedRuleSetName
      `Prelude.seq` Prelude.rnf productDescription
      `Prelude.seq` Prelude.rnf productId
      `Prelude.seq` Prelude.rnf productLink
      `Prelude.seq` Prelude.rnf productTitle
      `Prelude.seq` Prelude.rnf snsTopicArn
      `Prelude.seq` Prelude.rnf vendorName
