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
-- Module      : Amazonka.Connect.Types.Instance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.Instance where

import Amazonka.Connect.Types.DirectoryType
import Amazonka.Connect.Types.InstanceStatus
import Amazonka.Connect.Types.InstanceStatusReason
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The Amazon Connect instance.
--
-- /See:/ 'newInstance' smart constructor.
data Instance = Instance'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the instance was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The alias of instance.
    instanceAlias :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | The state of the instance.
    instanceStatus :: Prelude.Maybe InstanceStatus,
    -- | The identity management type.
    identityManagementType :: Prelude.Maybe DirectoryType,
    -- | Relevant details why the instance was not successfully created.
    statusReason :: Prelude.Maybe InstanceStatusReason,
    -- | The service role of the instance.
    serviceRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Instance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'instance_arn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'createdTime', 'instance_createdTime' - When the instance was created.
--
-- 'outboundCallsEnabled', 'instance_outboundCallsEnabled' - Whether outbound calls are enabled.
--
-- 'inboundCallsEnabled', 'instance_inboundCallsEnabled' - Whether inbound calls are enabled.
--
-- 'instanceAlias', 'instance_instanceAlias' - The alias of instance.
--
-- 'id', 'instance_id' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'instanceStatus', 'instance_instanceStatus' - The state of the instance.
--
-- 'identityManagementType', 'instance_identityManagementType' - The identity management type.
--
-- 'statusReason', 'instance_statusReason' - Relevant details why the instance was not successfully created.
--
-- 'serviceRole', 'instance_serviceRole' - The service role of the instance.
newInstance ::
  Instance
newInstance =
  Instance'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      outboundCallsEnabled = Prelude.Nothing,
      inboundCallsEnabled = Prelude.Nothing,
      instanceAlias = Prelude.Nothing,
      id = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      identityManagementType = Prelude.Nothing,
      statusReason = Prelude.Nothing,
      serviceRole = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance.
instance_arn :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_arn = Lens.lens (\Instance' {arn} -> arn) (\s@Instance' {} a -> s {arn = a} :: Instance)

-- | When the instance was created.
instance_createdTime :: Lens.Lens' Instance (Prelude.Maybe Prelude.UTCTime)
instance_createdTime = Lens.lens (\Instance' {createdTime} -> createdTime) (\s@Instance' {} a -> s {createdTime = a} :: Instance) Prelude.. Lens.mapping Core._Time

-- | Whether outbound calls are enabled.
instance_outboundCallsEnabled :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_outboundCallsEnabled = Lens.lens (\Instance' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@Instance' {} a -> s {outboundCallsEnabled = a} :: Instance)

-- | Whether inbound calls are enabled.
instance_inboundCallsEnabled :: Lens.Lens' Instance (Prelude.Maybe Prelude.Bool)
instance_inboundCallsEnabled = Lens.lens (\Instance' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@Instance' {} a -> s {inboundCallsEnabled = a} :: Instance)

-- | The alias of instance.
instance_instanceAlias :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_instanceAlias = Lens.lens (\Instance' {instanceAlias} -> instanceAlias) (\s@Instance' {} a -> s {instanceAlias = a} :: Instance) Prelude.. Lens.mapping Core._Sensitive

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
instance_id :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_id = Lens.lens (\Instance' {id} -> id) (\s@Instance' {} a -> s {id = a} :: Instance)

-- | The state of the instance.
instance_instanceStatus :: Lens.Lens' Instance (Prelude.Maybe InstanceStatus)
instance_instanceStatus = Lens.lens (\Instance' {instanceStatus} -> instanceStatus) (\s@Instance' {} a -> s {instanceStatus = a} :: Instance)

-- | The identity management type.
instance_identityManagementType :: Lens.Lens' Instance (Prelude.Maybe DirectoryType)
instance_identityManagementType = Lens.lens (\Instance' {identityManagementType} -> identityManagementType) (\s@Instance' {} a -> s {identityManagementType = a} :: Instance)

-- | Relevant details why the instance was not successfully created.
instance_statusReason :: Lens.Lens' Instance (Prelude.Maybe InstanceStatusReason)
instance_statusReason = Lens.lens (\Instance' {statusReason} -> statusReason) (\s@Instance' {} a -> s {statusReason = a} :: Instance)

-- | The service role of the instance.
instance_serviceRole :: Lens.Lens' Instance (Prelude.Maybe Prelude.Text)
instance_serviceRole = Lens.lens (\Instance' {serviceRole} -> serviceRole) (\s@Instance' {} a -> s {serviceRole = a} :: Instance)

instance Core.FromJSON Instance where
  parseJSON =
    Core.withObject
      "Instance"
      ( \x ->
          Instance'
            Prelude.<$> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "OutboundCallsEnabled")
            Prelude.<*> (x Core..:? "InboundCallsEnabled")
            Prelude.<*> (x Core..:? "InstanceAlias")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "InstanceStatus")
            Prelude.<*> (x Core..:? "IdentityManagementType")
            Prelude.<*> (x Core..:? "StatusReason")
            Prelude.<*> (x Core..:? "ServiceRole")
      )

instance Prelude.Hashable Instance where
  hashWithSalt _salt Instance' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` outboundCallsEnabled
      `Prelude.hashWithSalt` inboundCallsEnabled
      `Prelude.hashWithSalt` instanceAlias
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` instanceStatus
      `Prelude.hashWithSalt` identityManagementType
      `Prelude.hashWithSalt` statusReason
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData Instance where
  rnf Instance' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf outboundCallsEnabled
      `Prelude.seq` Prelude.rnf inboundCallsEnabled
      `Prelude.seq` Prelude.rnf instanceAlias
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf instanceStatus
      `Prelude.seq` Prelude.rnf identityManagementType
      `Prelude.seq` Prelude.rnf statusReason
      `Prelude.seq` Prelude.rnf serviceRole
