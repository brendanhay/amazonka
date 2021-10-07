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
-- Module      : Network.AWS.EKS.Types.Addon
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Addon where

import qualified Network.AWS.Core as Core
import Network.AWS.EKS.Types.AddonHealth
import Network.AWS.EKS.Types.AddonStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An Amazon EKS add-on.
--
-- /See:/ 'newAddon' smart constructor.
data Addon = Addon'
  { -- | The status of the add-on.
    status :: Prelude.Maybe AddonStatus,
    -- | The date and time that the add-on was last modified.
    modifiedAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the IAM role that is bound to the
    -- Kubernetes service account used by the add-on.
    serviceAccountRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time that the add-on was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the add-on.
    addonArn :: Prelude.Maybe Prelude.Text,
    -- | The version of the add-on.
    addonVersion :: Prelude.Maybe Prelude.Text,
    -- | The name of the add-on.
    addonName :: Prelude.Maybe Prelude.Text,
    -- | An object that represents the health of the add-on.
    health :: Prelude.Maybe AddonHealth,
    -- | The metadata that you apply to the add-on to assist with categorization
    -- and organization. Each tag consists of a key and an optional value, both
    -- of which you define. Add-on tags do not propagate to any other resources
    -- associated with the cluster.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the cluster.
    clusterName :: Prelude.Maybe Prelude.Text
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
-- 'status', 'addon_status' - The status of the add-on.
--
-- 'modifiedAt', 'addon_modifiedAt' - The date and time that the add-on was last modified.
--
-- 'serviceAccountRoleArn', 'addon_serviceAccountRoleArn' - The Amazon Resource Name (ARN) of the IAM role that is bound to the
-- Kubernetes service account used by the add-on.
--
-- 'createdAt', 'addon_createdAt' - The date and time that the add-on was created.
--
-- 'addonArn', 'addon_addonArn' - The Amazon Resource Name (ARN) of the add-on.
--
-- 'addonVersion', 'addon_addonVersion' - The version of the add-on.
--
-- 'addonName', 'addon_addonName' - The name of the add-on.
--
-- 'health', 'addon_health' - An object that represents the health of the add-on.
--
-- 'tags', 'addon_tags' - The metadata that you apply to the add-on to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Add-on tags do not propagate to any other resources
-- associated with the cluster.
--
-- 'clusterName', 'addon_clusterName' - The name of the cluster.
newAddon ::
  Addon
newAddon =
  Addon'
    { status = Prelude.Nothing,
      modifiedAt = Prelude.Nothing,
      serviceAccountRoleArn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      addonArn = Prelude.Nothing,
      addonVersion = Prelude.Nothing,
      addonName = Prelude.Nothing,
      health = Prelude.Nothing,
      tags = Prelude.Nothing,
      clusterName = Prelude.Nothing
    }

-- | The status of the add-on.
addon_status :: Lens.Lens' Addon (Prelude.Maybe AddonStatus)
addon_status = Lens.lens (\Addon' {status} -> status) (\s@Addon' {} a -> s {status = a} :: Addon)

-- | The date and time that the add-on was last modified.
addon_modifiedAt :: Lens.Lens' Addon (Prelude.Maybe Prelude.UTCTime)
addon_modifiedAt = Lens.lens (\Addon' {modifiedAt} -> modifiedAt) (\s@Addon' {} a -> s {modifiedAt = a} :: Addon) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the IAM role that is bound to the
-- Kubernetes service account used by the add-on.
addon_serviceAccountRoleArn :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_serviceAccountRoleArn = Lens.lens (\Addon' {serviceAccountRoleArn} -> serviceAccountRoleArn) (\s@Addon' {} a -> s {serviceAccountRoleArn = a} :: Addon)

-- | The date and time that the add-on was created.
addon_createdAt :: Lens.Lens' Addon (Prelude.Maybe Prelude.UTCTime)
addon_createdAt = Lens.lens (\Addon' {createdAt} -> createdAt) (\s@Addon' {} a -> s {createdAt = a} :: Addon) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the add-on.
addon_addonArn :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_addonArn = Lens.lens (\Addon' {addonArn} -> addonArn) (\s@Addon' {} a -> s {addonArn = a} :: Addon)

-- | The version of the add-on.
addon_addonVersion :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_addonVersion = Lens.lens (\Addon' {addonVersion} -> addonVersion) (\s@Addon' {} a -> s {addonVersion = a} :: Addon)

-- | The name of the add-on.
addon_addonName :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_addonName = Lens.lens (\Addon' {addonName} -> addonName) (\s@Addon' {} a -> s {addonName = a} :: Addon)

-- | An object that represents the health of the add-on.
addon_health :: Lens.Lens' Addon (Prelude.Maybe AddonHealth)
addon_health = Lens.lens (\Addon' {health} -> health) (\s@Addon' {} a -> s {health = a} :: Addon)

-- | The metadata that you apply to the add-on to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Add-on tags do not propagate to any other resources
-- associated with the cluster.
addon_tags :: Lens.Lens' Addon (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
addon_tags = Lens.lens (\Addon' {tags} -> tags) (\s@Addon' {} a -> s {tags = a} :: Addon) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the cluster.
addon_clusterName :: Lens.Lens' Addon (Prelude.Maybe Prelude.Text)
addon_clusterName = Lens.lens (\Addon' {clusterName} -> clusterName) (\s@Addon' {} a -> s {clusterName = a} :: Addon)

instance Core.FromJSON Addon where
  parseJSON =
    Core.withObject
      "Addon"
      ( \x ->
          Addon'
            Prelude.<$> (x Core..:? "status")
            Prelude.<*> (x Core..:? "modifiedAt")
            Prelude.<*> (x Core..:? "serviceAccountRoleArn")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "addonArn")
            Prelude.<*> (x Core..:? "addonVersion")
            Prelude.<*> (x Core..:? "addonName")
            Prelude.<*> (x Core..:? "health")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "clusterName")
      )

instance Prelude.Hashable Addon

instance Prelude.NFData Addon
