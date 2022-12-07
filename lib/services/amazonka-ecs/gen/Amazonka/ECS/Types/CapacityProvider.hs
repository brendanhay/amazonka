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
-- Module      : Amazonka.ECS.Types.CapacityProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.CapacityProvider where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.Types.AutoScalingGroupProvider
import Amazonka.ECS.Types.CapacityProviderStatus
import Amazonka.ECS.Types.CapacityProviderUpdateStatus
import Amazonka.ECS.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | The details for a capacity provider.
--
-- /See:/ 'newCapacityProvider' smart constructor.
data CapacityProvider = CapacityProvider'
  { -- | The metadata that you apply to the capacity provider to help you
    -- categorize and organize it. Each tag consists of a key and an optional
    -- value. You define both.
    --
    -- The following basic restrictions apply to tags:
    --
    -- -   Maximum number of tags per resource - 50
    --
    -- -   For each resource, each tag key must be unique, and each tag key can
    --     have only one value.
    --
    -- -   Maximum key length - 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length - 256 Unicode characters in UTF-8
    --
    -- -   If your tagging schema is used across multiple services and
    --     resources, remember that other services may have restrictions on
    --     allowed characters. Generally allowed characters are: letters,
    --     numbers, and spaces representable in UTF-8, and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Tag keys and values are case-sensitive.
    --
    -- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
    --     such as a prefix for either keys or values as it is reserved for
    --     Amazon Web Services use. You cannot edit or delete tag keys or
    --     values with this prefix. Tags with this prefix do not count against
    --     your tags per resource limit.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the capacity provider.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the capacity provider.
    capacityProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The update status reason. This provides further details about the update
    -- status for the capacity provider.
    updateStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The update status of the capacity provider. The following are the
    -- possible states that is returned.
    --
    -- [DELETE_IN_PROGRESS]
    --     The capacity provider is in the process of being deleted.
    --
    -- [DELETE_COMPLETE]
    --     The capacity provider was successfully deleted and has an @INACTIVE@
    --     status.
    --
    -- [DELETE_FAILED]
    --     The capacity provider can\'t be deleted. The update status reason
    --     provides further details about why the delete failed.
    updateStatus :: Prelude.Maybe CapacityProviderUpdateStatus,
    -- | The current status of the capacity provider. Only capacity providers in
    -- an @ACTIVE@ state can be used in a cluster. When a capacity provider is
    -- successfully deleted, it has an @INACTIVE@ status.
    status :: Prelude.Maybe CapacityProviderStatus,
    -- | The Auto Scaling group settings for the capacity provider.
    autoScalingGroupProvider :: Prelude.Maybe AutoScalingGroupProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'capacityProvider_tags' - The metadata that you apply to the capacity provider to help you
-- categorize and organize it. Each tag consists of a key and an optional
-- value. You define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
--
-- 'name', 'capacityProvider_name' - The name of the capacity provider.
--
-- 'capacityProviderArn', 'capacityProvider_capacityProviderArn' - The Amazon Resource Name (ARN) that identifies the capacity provider.
--
-- 'updateStatusReason', 'capacityProvider_updateStatusReason' - The update status reason. This provides further details about the update
-- status for the capacity provider.
--
-- 'updateStatus', 'capacityProvider_updateStatus' - The update status of the capacity provider. The following are the
-- possible states that is returned.
--
-- [DELETE_IN_PROGRESS]
--     The capacity provider is in the process of being deleted.
--
-- [DELETE_COMPLETE]
--     The capacity provider was successfully deleted and has an @INACTIVE@
--     status.
--
-- [DELETE_FAILED]
--     The capacity provider can\'t be deleted. The update status reason
--     provides further details about why the delete failed.
--
-- 'status', 'capacityProvider_status' - The current status of the capacity provider. Only capacity providers in
-- an @ACTIVE@ state can be used in a cluster. When a capacity provider is
-- successfully deleted, it has an @INACTIVE@ status.
--
-- 'autoScalingGroupProvider', 'capacityProvider_autoScalingGroupProvider' - The Auto Scaling group settings for the capacity provider.
newCapacityProvider ::
  CapacityProvider
newCapacityProvider =
  CapacityProvider'
    { tags = Prelude.Nothing,
      name = Prelude.Nothing,
      capacityProviderArn = Prelude.Nothing,
      updateStatusReason = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      autoScalingGroupProvider = Prelude.Nothing
    }

-- | The metadata that you apply to the capacity provider to help you
-- categorize and organize it. Each tag consists of a key and an optional
-- value. You define both.
--
-- The following basic restrictions apply to tags:
--
-- -   Maximum number of tags per resource - 50
--
-- -   For each resource, each tag key must be unique, and each tag key can
--     have only one value.
--
-- -   Maximum key length - 128 Unicode characters in UTF-8
--
-- -   Maximum value length - 256 Unicode characters in UTF-8
--
-- -   If your tagging schema is used across multiple services and
--     resources, remember that other services may have restrictions on
--     allowed characters. Generally allowed characters are: letters,
--     numbers, and spaces representable in UTF-8, and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Tag keys and values are case-sensitive.
--
-- -   Do not use @aws:@, @AWS:@, or any upper or lowercase combination of
--     such as a prefix for either keys or values as it is reserved for
--     Amazon Web Services use. You cannot edit or delete tag keys or
--     values with this prefix. Tags with this prefix do not count against
--     your tags per resource limit.
capacityProvider_tags :: Lens.Lens' CapacityProvider (Prelude.Maybe [Tag])
capacityProvider_tags = Lens.lens (\CapacityProvider' {tags} -> tags) (\s@CapacityProvider' {} a -> s {tags = a} :: CapacityProvider) Prelude.. Lens.mapping Lens.coerced

-- | The name of the capacity provider.
capacityProvider_name :: Lens.Lens' CapacityProvider (Prelude.Maybe Prelude.Text)
capacityProvider_name = Lens.lens (\CapacityProvider' {name} -> name) (\s@CapacityProvider' {} a -> s {name = a} :: CapacityProvider)

-- | The Amazon Resource Name (ARN) that identifies the capacity provider.
capacityProvider_capacityProviderArn :: Lens.Lens' CapacityProvider (Prelude.Maybe Prelude.Text)
capacityProvider_capacityProviderArn = Lens.lens (\CapacityProvider' {capacityProviderArn} -> capacityProviderArn) (\s@CapacityProvider' {} a -> s {capacityProviderArn = a} :: CapacityProvider)

-- | The update status reason. This provides further details about the update
-- status for the capacity provider.
capacityProvider_updateStatusReason :: Lens.Lens' CapacityProvider (Prelude.Maybe Prelude.Text)
capacityProvider_updateStatusReason = Lens.lens (\CapacityProvider' {updateStatusReason} -> updateStatusReason) (\s@CapacityProvider' {} a -> s {updateStatusReason = a} :: CapacityProvider)

-- | The update status of the capacity provider. The following are the
-- possible states that is returned.
--
-- [DELETE_IN_PROGRESS]
--     The capacity provider is in the process of being deleted.
--
-- [DELETE_COMPLETE]
--     The capacity provider was successfully deleted and has an @INACTIVE@
--     status.
--
-- [DELETE_FAILED]
--     The capacity provider can\'t be deleted. The update status reason
--     provides further details about why the delete failed.
capacityProvider_updateStatus :: Lens.Lens' CapacityProvider (Prelude.Maybe CapacityProviderUpdateStatus)
capacityProvider_updateStatus = Lens.lens (\CapacityProvider' {updateStatus} -> updateStatus) (\s@CapacityProvider' {} a -> s {updateStatus = a} :: CapacityProvider)

-- | The current status of the capacity provider. Only capacity providers in
-- an @ACTIVE@ state can be used in a cluster. When a capacity provider is
-- successfully deleted, it has an @INACTIVE@ status.
capacityProvider_status :: Lens.Lens' CapacityProvider (Prelude.Maybe CapacityProviderStatus)
capacityProvider_status = Lens.lens (\CapacityProvider' {status} -> status) (\s@CapacityProvider' {} a -> s {status = a} :: CapacityProvider)

-- | The Auto Scaling group settings for the capacity provider.
capacityProvider_autoScalingGroupProvider :: Lens.Lens' CapacityProvider (Prelude.Maybe AutoScalingGroupProvider)
capacityProvider_autoScalingGroupProvider = Lens.lens (\CapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@CapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: CapacityProvider)

instance Data.FromJSON CapacityProvider where
  parseJSON =
    Data.withObject
      "CapacityProvider"
      ( \x ->
          CapacityProvider'
            Prelude.<$> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "capacityProviderArn")
            Prelude.<*> (x Data..:? "updateStatusReason")
            Prelude.<*> (x Data..:? "updateStatus")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "autoScalingGroupProvider")
      )

instance Prelude.Hashable CapacityProvider where
  hashWithSalt _salt CapacityProvider' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` capacityProviderArn
      `Prelude.hashWithSalt` updateStatusReason
      `Prelude.hashWithSalt` updateStatus
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` autoScalingGroupProvider

instance Prelude.NFData CapacityProvider where
  rnf CapacityProvider' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf capacityProviderArn
      `Prelude.seq` Prelude.rnf updateStatusReason
      `Prelude.seq` Prelude.rnf updateStatus
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf autoScalingGroupProvider
