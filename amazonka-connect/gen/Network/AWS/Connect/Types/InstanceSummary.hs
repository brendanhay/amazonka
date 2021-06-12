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
-- Module      : Network.AWS.Connect.Types.InstanceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.InstanceSummary where

import Network.AWS.Connect.Types.DirectoryType
import Network.AWS.Connect.Types.InstanceStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about the instance.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | The alias of the instance.
    instanceAlias :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The service role of the instance.
    serviceRole :: Core.Maybe Core.Text,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Core.Maybe Core.Text,
    -- | The identifier of the instance.
    id :: Core.Maybe Core.Text,
    -- | The state of the instance.
    instanceStatus :: Core.Maybe InstanceStatus,
    -- | The identity management type of the instance.
    identityManagementType :: Core.Maybe DirectoryType,
    -- | When the instance was created.
    createdTime :: Core.Maybe Core.POSIX,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceAlias', 'instanceSummary_instanceAlias' - The alias of the instance.
--
-- 'serviceRole', 'instanceSummary_serviceRole' - The service role of the instance.
--
-- 'outboundCallsEnabled', 'instanceSummary_outboundCallsEnabled' - Whether outbound calls are enabled.
--
-- 'arn', 'instanceSummary_arn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'id', 'instanceSummary_id' - The identifier of the instance.
--
-- 'instanceStatus', 'instanceSummary_instanceStatus' - The state of the instance.
--
-- 'identityManagementType', 'instanceSummary_identityManagementType' - The identity management type of the instance.
--
-- 'createdTime', 'instanceSummary_createdTime' - When the instance was created.
--
-- 'inboundCallsEnabled', 'instanceSummary_inboundCallsEnabled' - Whether inbound calls are enabled.
newInstanceSummary ::
  InstanceSummary
newInstanceSummary =
  InstanceSummary'
    { instanceAlias = Core.Nothing,
      serviceRole = Core.Nothing,
      outboundCallsEnabled = Core.Nothing,
      arn = Core.Nothing,
      id = Core.Nothing,
      instanceStatus = Core.Nothing,
      identityManagementType = Core.Nothing,
      createdTime = Core.Nothing,
      inboundCallsEnabled = Core.Nothing
    }

-- | The alias of the instance.
instanceSummary_instanceAlias :: Lens.Lens' InstanceSummary (Core.Maybe Core.Text)
instanceSummary_instanceAlias = Lens.lens (\InstanceSummary' {instanceAlias} -> instanceAlias) (\s@InstanceSummary' {} a -> s {instanceAlias = a} :: InstanceSummary) Core.. Lens.mapping Core._Sensitive

-- | The service role of the instance.
instanceSummary_serviceRole :: Lens.Lens' InstanceSummary (Core.Maybe Core.Text)
instanceSummary_serviceRole = Lens.lens (\InstanceSummary' {serviceRole} -> serviceRole) (\s@InstanceSummary' {} a -> s {serviceRole = a} :: InstanceSummary)

-- | Whether outbound calls are enabled.
instanceSummary_outboundCallsEnabled :: Lens.Lens' InstanceSummary (Core.Maybe Core.Bool)
instanceSummary_outboundCallsEnabled = Lens.lens (\InstanceSummary' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@InstanceSummary' {} a -> s {outboundCallsEnabled = a} :: InstanceSummary)

-- | The Amazon Resource Name (ARN) of the instance.
instanceSummary_arn :: Lens.Lens' InstanceSummary (Core.Maybe Core.Text)
instanceSummary_arn = Lens.lens (\InstanceSummary' {arn} -> arn) (\s@InstanceSummary' {} a -> s {arn = a} :: InstanceSummary)

-- | The identifier of the instance.
instanceSummary_id :: Lens.Lens' InstanceSummary (Core.Maybe Core.Text)
instanceSummary_id = Lens.lens (\InstanceSummary' {id} -> id) (\s@InstanceSummary' {} a -> s {id = a} :: InstanceSummary)

-- | The state of the instance.
instanceSummary_instanceStatus :: Lens.Lens' InstanceSummary (Core.Maybe InstanceStatus)
instanceSummary_instanceStatus = Lens.lens (\InstanceSummary' {instanceStatus} -> instanceStatus) (\s@InstanceSummary' {} a -> s {instanceStatus = a} :: InstanceSummary)

-- | The identity management type of the instance.
instanceSummary_identityManagementType :: Lens.Lens' InstanceSummary (Core.Maybe DirectoryType)
instanceSummary_identityManagementType = Lens.lens (\InstanceSummary' {identityManagementType} -> identityManagementType) (\s@InstanceSummary' {} a -> s {identityManagementType = a} :: InstanceSummary)

-- | When the instance was created.
instanceSummary_createdTime :: Lens.Lens' InstanceSummary (Core.Maybe Core.UTCTime)
instanceSummary_createdTime = Lens.lens (\InstanceSummary' {createdTime} -> createdTime) (\s@InstanceSummary' {} a -> s {createdTime = a} :: InstanceSummary) Core.. Lens.mapping Core._Time

-- | Whether inbound calls are enabled.
instanceSummary_inboundCallsEnabled :: Lens.Lens' InstanceSummary (Core.Maybe Core.Bool)
instanceSummary_inboundCallsEnabled = Lens.lens (\InstanceSummary' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@InstanceSummary' {} a -> s {inboundCallsEnabled = a} :: InstanceSummary)

instance Core.FromJSON InstanceSummary where
  parseJSON =
    Core.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Core.<$> (x Core..:? "InstanceAlias")
            Core.<*> (x Core..:? "ServiceRole")
            Core.<*> (x Core..:? "OutboundCallsEnabled")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Id")
            Core.<*> (x Core..:? "InstanceStatus")
            Core.<*> (x Core..:? "IdentityManagementType")
            Core.<*> (x Core..:? "CreatedTime")
            Core.<*> (x Core..:? "InboundCallsEnabled")
      )

instance Core.Hashable InstanceSummary

instance Core.NFData InstanceSummary
