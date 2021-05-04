{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ECS.Types.CapacityProvider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.CapacityProvider where

import Network.AWS.ECS.Types.AutoScalingGroupProvider
import Network.AWS.ECS.Types.CapacityProviderStatus
import Network.AWS.ECS.Types.CapacityProviderUpdateStatus
import Network.AWS.ECS.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details of a capacity provider.
--
-- /See:/ 'newCapacityProvider' smart constructor.
data CapacityProvider = CapacityProvider'
  { -- | The current status of the capacity provider. Only capacity providers in
    -- an @ACTIVE@ state can be used in a cluster. When a capacity provider is
    -- successfully deleted, it will have an @INACTIVE@ status.
    status :: Prelude.Maybe CapacityProviderStatus,
    -- | The update status reason. This provides further details about the update
    -- status for the capacity provider.
    updateStatusReason :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) that identifies the capacity provider.
    capacityProviderArn :: Prelude.Maybe Prelude.Text,
    -- | The update status of the capacity provider. The following are the
    -- possible states that will be returned.
    --
    -- [DELETE_IN_PROGRESS]
    --     The capacity provider is in the process of being deleted.
    --
    -- [DELETE_COMPLETE]
    --     The capacity provider has been successfully deleted and will have an
    --     @INACTIVE@ status.
    --
    -- [DELETE_FAILED]
    --     The capacity provider was unable to be deleted. The update status
    --     reason will provide further details about why the delete failed.
    updateStatus :: Prelude.Maybe CapacityProviderUpdateStatus,
    -- | The name of the capacity provider.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Auto Scaling group settings for the capacity provider.
    autoScalingGroupProvider :: Prelude.Maybe AutoScalingGroupProvider,
    -- | The metadata that you apply to the capacity provider to help you
    -- categorize and organize it. Each tag consists of a key and an optional
    -- value, both of which you define.
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
    --     such as a prefix for either keys or values as it is reserved for AWS
    --     use. You cannot edit or delete tag keys or values with this prefix.
    --     Tags with this prefix do not count against your tags per resource
    --     limit.
    tags :: Prelude.Maybe [Tag]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CapacityProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'capacityProvider_status' - The current status of the capacity provider. Only capacity providers in
-- an @ACTIVE@ state can be used in a cluster. When a capacity provider is
-- successfully deleted, it will have an @INACTIVE@ status.
--
-- 'updateStatusReason', 'capacityProvider_updateStatusReason' - The update status reason. This provides further details about the update
-- status for the capacity provider.
--
-- 'capacityProviderArn', 'capacityProvider_capacityProviderArn' - The Amazon Resource Name (ARN) that identifies the capacity provider.
--
-- 'updateStatus', 'capacityProvider_updateStatus' - The update status of the capacity provider. The following are the
-- possible states that will be returned.
--
-- [DELETE_IN_PROGRESS]
--     The capacity provider is in the process of being deleted.
--
-- [DELETE_COMPLETE]
--     The capacity provider has been successfully deleted and will have an
--     @INACTIVE@ status.
--
-- [DELETE_FAILED]
--     The capacity provider was unable to be deleted. The update status
--     reason will provide further details about why the delete failed.
--
-- 'name', 'capacityProvider_name' - The name of the capacity provider.
--
-- 'autoScalingGroupProvider', 'capacityProvider_autoScalingGroupProvider' - The Auto Scaling group settings for the capacity provider.
--
-- 'tags', 'capacityProvider_tags' - The metadata that you apply to the capacity provider to help you
-- categorize and organize it. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
newCapacityProvider ::
  CapacityProvider
newCapacityProvider =
  CapacityProvider'
    { status = Prelude.Nothing,
      updateStatusReason = Prelude.Nothing,
      capacityProviderArn = Prelude.Nothing,
      updateStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      autoScalingGroupProvider = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The current status of the capacity provider. Only capacity providers in
-- an @ACTIVE@ state can be used in a cluster. When a capacity provider is
-- successfully deleted, it will have an @INACTIVE@ status.
capacityProvider_status :: Lens.Lens' CapacityProvider (Prelude.Maybe CapacityProviderStatus)
capacityProvider_status = Lens.lens (\CapacityProvider' {status} -> status) (\s@CapacityProvider' {} a -> s {status = a} :: CapacityProvider)

-- | The update status reason. This provides further details about the update
-- status for the capacity provider.
capacityProvider_updateStatusReason :: Lens.Lens' CapacityProvider (Prelude.Maybe Prelude.Text)
capacityProvider_updateStatusReason = Lens.lens (\CapacityProvider' {updateStatusReason} -> updateStatusReason) (\s@CapacityProvider' {} a -> s {updateStatusReason = a} :: CapacityProvider)

-- | The Amazon Resource Name (ARN) that identifies the capacity provider.
capacityProvider_capacityProviderArn :: Lens.Lens' CapacityProvider (Prelude.Maybe Prelude.Text)
capacityProvider_capacityProviderArn = Lens.lens (\CapacityProvider' {capacityProviderArn} -> capacityProviderArn) (\s@CapacityProvider' {} a -> s {capacityProviderArn = a} :: CapacityProvider)

-- | The update status of the capacity provider. The following are the
-- possible states that will be returned.
--
-- [DELETE_IN_PROGRESS]
--     The capacity provider is in the process of being deleted.
--
-- [DELETE_COMPLETE]
--     The capacity provider has been successfully deleted and will have an
--     @INACTIVE@ status.
--
-- [DELETE_FAILED]
--     The capacity provider was unable to be deleted. The update status
--     reason will provide further details about why the delete failed.
capacityProvider_updateStatus :: Lens.Lens' CapacityProvider (Prelude.Maybe CapacityProviderUpdateStatus)
capacityProvider_updateStatus = Lens.lens (\CapacityProvider' {updateStatus} -> updateStatus) (\s@CapacityProvider' {} a -> s {updateStatus = a} :: CapacityProvider)

-- | The name of the capacity provider.
capacityProvider_name :: Lens.Lens' CapacityProvider (Prelude.Maybe Prelude.Text)
capacityProvider_name = Lens.lens (\CapacityProvider' {name} -> name) (\s@CapacityProvider' {} a -> s {name = a} :: CapacityProvider)

-- | The Auto Scaling group settings for the capacity provider.
capacityProvider_autoScalingGroupProvider :: Lens.Lens' CapacityProvider (Prelude.Maybe AutoScalingGroupProvider)
capacityProvider_autoScalingGroupProvider = Lens.lens (\CapacityProvider' {autoScalingGroupProvider} -> autoScalingGroupProvider) (\s@CapacityProvider' {} a -> s {autoScalingGroupProvider = a} :: CapacityProvider)

-- | The metadata that you apply to the capacity provider to help you
-- categorize and organize it. Each tag consists of a key and an optional
-- value, both of which you define.
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
--     such as a prefix for either keys or values as it is reserved for AWS
--     use. You cannot edit or delete tag keys or values with this prefix.
--     Tags with this prefix do not count against your tags per resource
--     limit.
capacityProvider_tags :: Lens.Lens' CapacityProvider (Prelude.Maybe [Tag])
capacityProvider_tags = Lens.lens (\CapacityProvider' {tags} -> tags) (\s@CapacityProvider' {} a -> s {tags = a} :: CapacityProvider) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON CapacityProvider where
  parseJSON =
    Prelude.withObject
      "CapacityProvider"
      ( \x ->
          CapacityProvider'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "updateStatusReason")
            Prelude.<*> (x Prelude..:? "capacityProviderArn")
            Prelude.<*> (x Prelude..:? "updateStatus")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "autoScalingGroupProvider")
            Prelude.<*> (x Prelude..:? "tags" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable CapacityProvider

instance Prelude.NFData CapacityProvider
