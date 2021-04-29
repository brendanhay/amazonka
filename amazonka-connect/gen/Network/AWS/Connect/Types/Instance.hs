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
-- Module      : Network.AWS.Connect.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Instance where

import Network.AWS.Connect.Types.DirectoryType
import Network.AWS.Connect.Types.InstanceStatus
import Network.AWS.Connect.Types.InstanceStatusReason
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Amazon Connect instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The alias of instance.
    instanceAlias :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The service role of the instance.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the instance.
    instanceStatus :: Prelude.Maybe InstanceStatus,
    -- | The identity management type.
    identityManagementType :: Prelude.Maybe DirectoryType,
    -- | When the instance was created.
    createdTime :: Prelude.Maybe Prelude.POSIX,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Relevant details why the instance was not successfully created.
    statusReason :: Prelude.Maybe InstanceStatusReason
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceAlias', 'instance_instanceAlias' - The alias of instance.
--
-- 'serviceRole', 'instance_serviceRole' - The service role of the instance.
--
-- 'outboundCallsEnabled', 'instance_outboundCallsEnabled' - Whether outbound calls are enabled.
--
-- 'arn', 'instance_arn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'id', 'instance_id' - The identifier of the Amazon Connect instance.
--
-- 'instanceStatus', 'instance_instanceStatus' - The state of the instance.
--
-- 'identityManagementType', 'instance_identityManagementType' - The identity management type.
--
-- 'createdTime', 'instance_createdTime' - When the instance was created.
--
-- 'inboundCallsEnabled', 'instance_inboundCallsEnabled' - Whether inbound calls are enabled.
--
-- 'statusReason', 'instance_statusReason' - Relevant details why the instance was not successfully created.
newInstance ::
  Instance
newInstance =
  Instance'
    { instanceAlias = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      outboundCallsEnabled = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      identityManagementType = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      inboundCallsEnabled = Prelude.Nothing,
      statusReason = Prelude.Nothing
    }

-- | The alias of instance.
instance_instanceAlias :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceAlias = Lens.lens (\Instance' {instanceAlias} -> instanceAlias) (\s@Instance' {} a -> s {instanceAlias = a} :: Instance) Prelude.. Lens.mapping Prelude._Sensitive

-- | The service role of the instance.
instance_serviceRole :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_serviceRole = Lens.lens (\Instance' {serviceRole} -> serviceRole) (\s@Instance' {} a -> s {serviceRole = a} :: Instance)

-- | Whether outbound calls are enabled.
instance_outboundCallsEnabled :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_outboundCallsEnabled = Lens.lens (\Instance' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@Instance' {} a -> s {outboundCallsEnabled = a} :: Instance)

-- | The Amazon Resource Name (ARN) of the instance.
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | The identifier of the Amazon Connect instance.
instance_id :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_id = Lens.lens (\Instance' {id} -> id) (\s@Instance' {} a -> s {id = a} :: Instance)

-- | The state of the instance.
instance_instanceStatus :: Lens.Lens' Instance (Prelude.Maybe InstanceStatus)
instance_instanceStatus = Lens.lens (\Instance' {instanceStatus} -> instanceStatus) (\s@Instance' {} a -> s {instanceStatus = a} :: Instance)

-- | The identity management type.
instance_identityManagementType :: Lens.Lens' Instance (Prelude.Maybe DirectoryType)
instance_identityManagementType = Lens.lens (\Instance' {identityManagementType} -> identityManagementType) (\s@Instance' {} a -> s {identityManagementType = a} :: Instance)

-- | When the instance was created.
instance_createdTime :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdTime = Lens.lens (\Instance' {createdTime} -> createdTime) (\s@Instance' {} a -> s {createdTime = a} :: Instance) Prelude.. Lens.mapping Prelude._Time

-- | Whether inbound calls are enabled.
instance_inboundCallsEnabled :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_inboundCallsEnabled = Lens.lens (\Instance' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@Instance' {} a -> s {inboundCallsEnabled = a} :: Instance)

-- | Relevant details why the instance was not successfully created.
instance_statusReason :: Lens.Lens' Instance (Prelude.Maybe InstanceStatusReason)
instance_statusReason = Lens.lens (\Instance' {statusReason} -> statusReason) (\s@Instance' {} a -> s {statusReason = a} :: Instance)

instance Prelude.FromJSON Instance where
  parseJSON =
    Prelude.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Prelude..:? "InstanceAlias")
            Prelude.<*> (x Prelude..:? "ServiceRole")
            Prelude.<*> (x Prelude..:? "OutboundCallsEnabled")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "InstanceStatus")
            Prelude.<*> (x Prelude..:? "IdentityManagementType")
            Prelude.<*> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "InboundCallsEnabled")
            Prelude.<*> (x Prelude..:? "StatusReason")
      )

instance Prelude.Hashable Instance

instance Prelude.NFData Instance
