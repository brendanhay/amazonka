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

-- | An Amazon EKS add-on.
--
-- /See:/ 'newAddon' smart constructor.
data Addon = Addon'
  { -- | The date and time that the add-on was last modified.
    modifiedAt :: Core.Maybe Core.POSIX,
    -- | The status of the add-on.
    status :: Core.Maybe AddonStatus,
    -- | The Amazon Resource Name (ARN) of the add-on.
    addonArn :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that is bound to the
    -- Kubernetes service account used by the add-on.
    serviceAccountRoleArn :: Core.Maybe Core.Text,
    -- | The date and time that the add-on was created.
    createdAt :: Core.Maybe Core.POSIX,
    -- | The version of the add-on.
    addonVersion :: Core.Maybe Core.Text,
    -- | The name of the add-on.
    addonName :: Core.Maybe Core.Text,
    -- | An object that represents the health of the add-on.
    health :: Core.Maybe AddonHealth,
    -- | The metadata that you apply to the cluster to assist with categorization
    -- and organization. Each tag consists of a key and an optional value, both
    -- of which you define. Cluster tags do not propagate to any other
    -- resources associated with the cluster.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The name of the cluster.
    clusterName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Addon' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modifiedAt', 'addon_modifiedAt' - The date and time that the add-on was last modified.
--
-- 'status', 'addon_status' - The status of the add-on.
--
-- 'addonArn', 'addon_addonArn' - The Amazon Resource Name (ARN) of the add-on.
--
-- 'serviceAccountRoleArn', 'addon_serviceAccountRoleArn' - The Amazon Resource Name (ARN) of the IAM role that is bound to the
-- Kubernetes service account used by the add-on.
--
-- 'createdAt', 'addon_createdAt' - The date and time that the add-on was created.
--
-- 'addonVersion', 'addon_addonVersion' - The version of the add-on.
--
-- 'addonName', 'addon_addonName' - The name of the add-on.
--
-- 'health', 'addon_health' - An object that represents the health of the add-on.
--
-- 'tags', 'addon_tags' - The metadata that you apply to the cluster to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Cluster tags do not propagate to any other
-- resources associated with the cluster.
--
-- 'clusterName', 'addon_clusterName' - The name of the cluster.
newAddon ::
  Addon
newAddon =
  Addon'
    { modifiedAt = Core.Nothing,
      status = Core.Nothing,
      addonArn = Core.Nothing,
      serviceAccountRoleArn = Core.Nothing,
      createdAt = Core.Nothing,
      addonVersion = Core.Nothing,
      addonName = Core.Nothing,
      health = Core.Nothing,
      tags = Core.Nothing,
      clusterName = Core.Nothing
    }

-- | The date and time that the add-on was last modified.
addon_modifiedAt :: Lens.Lens' Addon (Core.Maybe Core.UTCTime)
addon_modifiedAt = Lens.lens (\Addon' {modifiedAt} -> modifiedAt) (\s@Addon' {} a -> s {modifiedAt = a} :: Addon) Core.. Lens.mapping Core._Time

-- | The status of the add-on.
addon_status :: Lens.Lens' Addon (Core.Maybe AddonStatus)
addon_status = Lens.lens (\Addon' {status} -> status) (\s@Addon' {} a -> s {status = a} :: Addon)

-- | The Amazon Resource Name (ARN) of the add-on.
addon_addonArn :: Lens.Lens' Addon (Core.Maybe Core.Text)
addon_addonArn = Lens.lens (\Addon' {addonArn} -> addonArn) (\s@Addon' {} a -> s {addonArn = a} :: Addon)

-- | The Amazon Resource Name (ARN) of the IAM role that is bound to the
-- Kubernetes service account used by the add-on.
addon_serviceAccountRoleArn :: Lens.Lens' Addon (Core.Maybe Core.Text)
addon_serviceAccountRoleArn = Lens.lens (\Addon' {serviceAccountRoleArn} -> serviceAccountRoleArn) (\s@Addon' {} a -> s {serviceAccountRoleArn = a} :: Addon)

-- | The date and time that the add-on was created.
addon_createdAt :: Lens.Lens' Addon (Core.Maybe Core.UTCTime)
addon_createdAt = Lens.lens (\Addon' {createdAt} -> createdAt) (\s@Addon' {} a -> s {createdAt = a} :: Addon) Core.. Lens.mapping Core._Time

-- | The version of the add-on.
addon_addonVersion :: Lens.Lens' Addon (Core.Maybe Core.Text)
addon_addonVersion = Lens.lens (\Addon' {addonVersion} -> addonVersion) (\s@Addon' {} a -> s {addonVersion = a} :: Addon)

-- | The name of the add-on.
addon_addonName :: Lens.Lens' Addon (Core.Maybe Core.Text)
addon_addonName = Lens.lens (\Addon' {addonName} -> addonName) (\s@Addon' {} a -> s {addonName = a} :: Addon)

-- | An object that represents the health of the add-on.
addon_health :: Lens.Lens' Addon (Core.Maybe AddonHealth)
addon_health = Lens.lens (\Addon' {health} -> health) (\s@Addon' {} a -> s {health = a} :: Addon)

-- | The metadata that you apply to the cluster to assist with categorization
-- and organization. Each tag consists of a key and an optional value, both
-- of which you define. Cluster tags do not propagate to any other
-- resources associated with the cluster.
addon_tags :: Lens.Lens' Addon (Core.Maybe (Core.HashMap Core.Text Core.Text))
addon_tags = Lens.lens (\Addon' {tags} -> tags) (\s@Addon' {} a -> s {tags = a} :: Addon) Core.. Lens.mapping Lens._Coerce

-- | The name of the cluster.
addon_clusterName :: Lens.Lens' Addon (Core.Maybe Core.Text)
addon_clusterName = Lens.lens (\Addon' {clusterName} -> clusterName) (\s@Addon' {} a -> s {clusterName = a} :: Addon)

instance Core.FromJSON Addon where
  parseJSON =
    Core.withObject
      "Addon"
      ( \x ->
          Addon'
            Core.<$> (x Core..:? "modifiedAt")
            Core.<*> (x Core..:? "status")
            Core.<*> (x Core..:? "addonArn")
            Core.<*> (x Core..:? "serviceAccountRoleArn")
            Core.<*> (x Core..:? "createdAt")
            Core.<*> (x Core..:? "addonVersion")
            Core.<*> (x Core..:? "addonName")
            Core.<*> (x Core..:? "health")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..:? "clusterName")
      )

instance Core.Hashable Addon

instance Core.NFData Addon
