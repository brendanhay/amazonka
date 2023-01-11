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
-- Module      : Amazonka.EKS.Types.Addon
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EKS.Types.Addon where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EKS.Types.AddonHealth
import Amazonka.EKS.Types.AddonStatus
import Amazonka.EKS.Types.MarketplaceInformation
import qualified Amazonka.Prelude as Prelude

-- | An Amazon EKS add-on. For more information, see
-- <https://docs.aws.amazon.com/eks/latest/userguide/eks-add-ons.html Amazon EKS add-ons>
-- in the /Amazon EKS User Guide/.
--
-- /See:/ 'newAddon' smart constructor.
data Addon = Addon'
  { -- | The Amazon Resource Name (ARN) of the add-on.
    addonArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the add-on.
    addonName :: Prelude.Maybe Prelude.Text,
    -- | The version of the add-on.
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the cluster.
    clusterName :: Prelude.Maybe Prelude.Text,
    -- | The configuration values that you provided.
    configurationValues :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the add-on was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | An object that represents the health of the add-on.
    health :: Prelude.Maybe AddonHealth,
    -- | Information about an Amazon EKS add-on from the Amazon Web Services
    -- Marketplace.
    marketplaceInformation :: Prelude.Maybe MarketplaceInformation,
    -- | The date and time that the add-on was last modified.
    modifiedAt :: Prelude.Maybe Data.POSIX,
    -- | The owner of the add-on.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The publisher of the add-on.
    publisher :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that\'s bound to the
    -- Kubernetes service account that the add-on uses.
    serviceAccountRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the add-on.
    status :: Prelude.Maybe AddonStatus,
    -- | The metadata that you apply to the add-on to assist with categorization
    -- and organization. Each tag consists of a key and an optional value. You
    -- define both. Add-on tags do not propagate to any other resources
    -- associated with the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Addon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'addonArn', 'addon_addonArn' - The Amazon Resource Name (ARN) of the add-on.
--
-- 'addonName', 'addon_addonName' - The name of the add-on.
--
-- 'addonVersion', 'addon_addonVersion' - The version of the add-on.
--
-- 'clusterName', 'addon_clusterName' - The name of the cluster.
--
-- 'configurationValues', 'addon_configurationValues' - The configuration values that you provided.
--
-- 'createdAt', 'addon_createdAt' - The date and time that the add-on was created.
--
-- 'health', 'addon_health' - An object that represents the health of the add-on.
--
-- 'marketplaceInformation', 'addon_marketplaceInformation' - Information about an Amazon EKS add-on from the Amazon Web Services
-- Marketplace.
--
-- 'modifiedAt', 'addon_modifiedAt' - The date and time that the add-on was last modified.
--
-- 'owner', 'addon_owner' - The owner of the add-on.
--
-- 'publisher', 'addon_publisher' - The publisher of the add-on.
--
-- 'serviceAccountRoleArn', 'addon_serviceAccountRoleArn' - The Amazon Resource Name (ARN) of the IAM role that\'s bound to the
-- Kubernetes service account that the add-on uses.
--
-- 'status', 'addon_status' - The status of the add-on.
--
-- 'tags', 'addon_tags' - The metadata that you apply to the add-on to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both. Add-on tags do not propagate to any other resources
-- associated with the cluster.
newAddon ::
  Addon
newAddon =
  Addon'
    { addonArn = Prelude.Nothing,
      addonName = Prelude.Nothing,
      addonVersion = Prelude.Nothing,
      clusterName = Prelude.Nothing,
      configurationValues = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      health = Prelude.Nothing,
      marketplaceInformation = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      owner = Prelude.Nothing,
      publisher = Prelude.Nothing,
      serviceAccountRoleArn = Prelude.Nothing,
      status = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the add-on.
addon_addonArn :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_addonArn = Lens.lens (\Addon' {addonArn} -> addonArn) (\s@Addon' {} a -> s {addonArn = a} :: Addon)

-- | The name of the add-on.
addon_addonName :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_addonName = Lens.lens (\Addon' {addonName} -> addonName) (\s@Addon' {} a -> s {addonName = a} :: Addon)

-- | The version of the add-on.
addon_addonVersion :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_addonVersion = Lens.lens (\Addon' {addonVersion} -> addonVersion) (\s@Addon' {} a -> s {addonVersion = a} :: Addon)

-- | The name of the cluster.
addon_clusterName :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_clusterName = Lens.lens (\Addon' {clusterName} -> clusterName) (\s@Addon' {} a -> s {clusterName = a} :: Addon)

-- | The configuration values that you provided.
addon_configurationValues :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_configurationValues = Lens.lens (\Addon' {configurationValues} -> configurationValues) (\s@Addon' {} a -> s {configurationValues = a} :: Addon)

-- | The date and time that the add-on was created.
addon_createdAt :: Lens.Lens' Addon (Prelude.Maybe Prelude.UTCTime)
addon_createdAt = Lens.lens (\Addon' {createdAt} -> createdAt) (\s@Addon' {} a -> s {createdAt = a} :: Addon) Prelude.. Lens.mapping Data._Time

-- | An object that represents the health of the add-on.
addon_health :: Lens.Lens' Addon (Prelude.Maybe AddonHealth)
addon_health = Lens.lens (\Addon' {health} -> health) (\s@Addon' {} a -> s {health = a} :: Addon)

-- | Information about an Amazon EKS add-on from the Amazon Web Services
-- Marketplace.
addon_marketplaceInformation :: Lens.Lens' Addon (Prelude.Maybe MarketplaceInformation)
addon_marketplaceInformation = Lens.lens (\Addon' {marketplaceInformation} -> marketplaceInformation) (\s@Addon' {} a -> s {marketplaceInformation = a} :: Addon)

-- | The date and time that the add-on was last modified.
addon_modifiedAt :: Lens.Lens' Addon (Prelude.Maybe Prelude.UTCTime)
addon_modifiedAt = Lens.lens (\Addon' {modifiedAt} -> modifiedAt) (\s@Addon' {} a -> s {modifiedAt = a} :: Addon) Prelude.. Lens.mapping Data._Time

-- | The owner of the add-on.
addon_owner :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_owner = Lens.lens (\Addon' {owner} -> owner) (\s@Addon' {} a -> s {owner = a} :: Addon)

-- | The publisher of the add-on.
addon_publisher :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_publisher = Lens.lens (\Addon' {publisher} -> publisher) (\s@Addon' {} a -> s {publisher = a} :: Addon)

-- | The Amazon Resource Name (ARN) of the IAM role that\'s bound to the
-- Kubernetes service account that the add-on uses.
addon_serviceAccountRoleArn :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_serviceAccountRoleArn = Lens.lens (\Addon' {serviceAccountRoleArn} -> serviceAccountRoleArn) (\s@Addon' {} a -> s {serviceAccountRoleArn = a} :: Addon)

-- | The status of the add-on.
addon_status :: Lens.Lens' Addon (Prelude.Maybe AddonStatus)
addon_status = Lens.lens (\Addon' {status} -> status) (\s@Addon' {} a -> s {status = a} :: Addon)

-- | The metadata that you apply to the add-on to assist with categorization
-- and organization. Each tag consists of a key and an optional value. You
-- define both. Add-on tags do not propagate to any other resources
-- associated with the cluster.
addon_tags :: Lens.Lens' Addon (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
addon_tags = Lens.lens (\Addon' {tags} -> tags) (\s@Addon' {} a -> s {tags = a} :: Addon) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON Addon where
  parseJSON =
    Data.withObject
      "Addon"
      ( \x ->
          Addon'
            Prelude.<$> (x Data..:? "addonArn")
            Prelude.<*> (x Data..:? "addonName")
            Prelude.<*> (x Data..:? "addonVersion")
            Prelude.<*> (x Data..:? "clusterName")
            Prelude.<*> (x Data..:? "configurationValues")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "health")
            Prelude.<*> (x Data..:? "marketplaceInformation")
            Prelude.<*> (x Data..:? "modifiedAt")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "publisher")
            Prelude.<*> (x Data..:? "serviceAccountRoleArn")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable Addon where
  hashWithSalt _salt Addon' {..} =
    _salt `Prelude.hashWithSalt` addonArn
      `Prelude.hashWithSalt` addonName
      `Prelude.hashWithSalt` addonVersion
      `Prelude.hashWithSalt` clusterName
      `Prelude.hashWithSalt` configurationValues
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` health
      `Prelude.hashWithSalt` marketplaceInformation
      `Prelude.hashWithSalt` modifiedAt
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` publisher
      `Prelude.hashWithSalt` serviceAccountRoleArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tags

instance Prelude.NFData Addon where
  rnf Addon' {..} =
    Prelude.rnf addonArn
      `Prelude.seq` Prelude.rnf addonName
      `Prelude.seq` Prelude.rnf addonVersion
      `Prelude.seq` Prelude.rnf clusterName
      `Prelude.seq` Prelude.rnf configurationValues
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf health
      `Prelude.seq` Prelude.rnf marketplaceInformation
      `Prelude.seq` Prelude.rnf modifiedAt
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf publisher
      `Prelude.seq` Prelude.rnf serviceAccountRoleArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tags
