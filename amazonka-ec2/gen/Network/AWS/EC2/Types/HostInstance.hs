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
-- Module      : Network.AWS.EC2.Types.HostInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostInstance where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | Describes an instance running on a Dedicated Host.
--
-- /See:/ 'newHostInstance' smart constructor.
data HostInstance = HostInstance'
  { -- | The ID of the AWS account that owns the instance.
    ownerId :: Core.Maybe Core.Text,
    -- | The ID of instance that is running on the Dedicated Host.
    instanceId :: Core.Maybe Core.Text,
    -- | The instance type (for example, @m3.medium@) of the running instance.
    instanceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'HostInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'hostInstance_ownerId' - The ID of the AWS account that owns the instance.
--
-- 'instanceId', 'hostInstance_instanceId' - The ID of instance that is running on the Dedicated Host.
--
-- 'instanceType', 'hostInstance_instanceType' - The instance type (for example, @m3.medium@) of the running instance.
newHostInstance ::
  HostInstance
newHostInstance =
  HostInstance'
    { ownerId = Core.Nothing,
      instanceId = Core.Nothing,
      instanceType = Core.Nothing
    }

-- | The ID of the AWS account that owns the instance.
hostInstance_ownerId :: Lens.Lens' HostInstance (Core.Maybe Core.Text)
hostInstance_ownerId = Lens.lens (\HostInstance' {ownerId} -> ownerId) (\s@HostInstance' {} a -> s {ownerId = a} :: HostInstance)

-- | The ID of instance that is running on the Dedicated Host.
hostInstance_instanceId :: Lens.Lens' HostInstance (Core.Maybe Core.Text)
hostInstance_instanceId = Lens.lens (\HostInstance' {instanceId} -> instanceId) (\s@HostInstance' {} a -> s {instanceId = a} :: HostInstance)

-- | The instance type (for example, @m3.medium@) of the running instance.
hostInstance_instanceType :: Lens.Lens' HostInstance (Core.Maybe Core.Text)
hostInstance_instanceType = Lens.lens (\HostInstance' {instanceType} -> instanceType) (\s@HostInstance' {} a -> s {instanceType = a} :: HostInstance)

instance Core.FromXML HostInstance where
  parseXML x =
    HostInstance'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "instanceType")

instance Core.Hashable HostInstance

instance Core.NFData HostInstance
