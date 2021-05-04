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
-- Module      : Network.AWS.EC2.Types.HostInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.HostInstance where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes an instance running on a Dedicated Host.
--
-- /See:/ 'newHostInstance' smart constructor.
data HostInstance = HostInstance'
  { -- | The ID of the AWS account that owns the instance.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The ID of instance that is running on the Dedicated Host.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | The instance type (for example, @m3.medium@) of the running instance.
    instanceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { ownerId = Prelude.Nothing,
      instanceId = Prelude.Nothing,
      instanceType = Prelude.Nothing
    }

-- | The ID of the AWS account that owns the instance.
hostInstance_ownerId :: Lens.Lens' HostInstance (Prelude.Maybe Prelude.Text)
hostInstance_ownerId = Lens.lens (\HostInstance' {ownerId} -> ownerId) (\s@HostInstance' {} a -> s {ownerId = a} :: HostInstance)

-- | The ID of instance that is running on the Dedicated Host.
hostInstance_instanceId :: Lens.Lens' HostInstance (Prelude.Maybe Prelude.Text)
hostInstance_instanceId = Lens.lens (\HostInstance' {instanceId} -> instanceId) (\s@HostInstance' {} a -> s {instanceId = a} :: HostInstance)

-- | The instance type (for example, @m3.medium@) of the running instance.
hostInstance_instanceType :: Lens.Lens' HostInstance (Prelude.Maybe Prelude.Text)
hostInstance_instanceType = Lens.lens (\HostInstance' {instanceType} -> instanceType) (\s@HostInstance' {} a -> s {instanceType = a} :: HostInstance)

instance Prelude.FromXML HostInstance where
  parseXML x =
    HostInstance'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "instanceId")
      Prelude.<*> (x Prelude..@? "instanceType")

instance Prelude.Hashable HostInstance

instance Prelude.NFData HostInstance
