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
import qualified Network.AWS.Prelude as Prelude

-- | Information about the instance.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | The alias of the instance.
    instanceAlias :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The service role of the instance.
    serviceRole :: Prelude.Maybe Prelude.Text,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The identity management type of the instance.
    identityManagementType :: Prelude.Maybe DirectoryType,
    -- | The state of the instance.
    instanceStatus :: Prelude.Maybe InstanceStatus,
    -- | The identifier of the instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | When the instance was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'identityManagementType', 'instanceSummary_identityManagementType' - The identity management type of the instance.
--
-- 'instanceStatus', 'instanceSummary_instanceStatus' - The state of the instance.
--
-- 'id', 'instanceSummary_id' - The identifier of the instance.
--
-- 'createdTime', 'instanceSummary_createdTime' - When the instance was created.
--
-- 'inboundCallsEnabled', 'instanceSummary_inboundCallsEnabled' - Whether inbound calls are enabled.
newInstanceSummary ::
  InstanceSummary
newInstanceSummary =
  InstanceSummary'
    { instanceAlias = Prelude.Nothing,
      serviceRole = Prelude.Nothing,
      outboundCallsEnabled = Prelude.Nothing,
      arn = Prelude.Nothing,
      identityManagementType = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      id = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      inboundCallsEnabled = Prelude.Nothing
    }

-- | The alias of the instance.
instanceSummary_instanceAlias :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_instanceAlias = Lens.lens (\InstanceSummary' {instanceAlias} -> instanceAlias) (\s@InstanceSummary' {} a -> s {instanceAlias = a} :: InstanceSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The service role of the instance.
instanceSummary_serviceRole :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_serviceRole = Lens.lens (\InstanceSummary' {serviceRole} -> serviceRole) (\s@InstanceSummary' {} a -> s {serviceRole = a} :: InstanceSummary)

-- | Whether outbound calls are enabled.
instanceSummary_outboundCallsEnabled :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Bool)
instanceSummary_outboundCallsEnabled = Lens.lens (\InstanceSummary' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@InstanceSummary' {} a -> s {outboundCallsEnabled = a} :: InstanceSummary)

-- | The Amazon Resource Name (ARN) of the instance.
instanceSummary_arn :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_arn = Lens.lens (\InstanceSummary' {arn} -> arn) (\s@InstanceSummary' {} a -> s {arn = a} :: InstanceSummary)

-- | The identity management type of the instance.
instanceSummary_identityManagementType :: Lens.Lens' InstanceSummary (Prelude.Maybe DirectoryType)
instanceSummary_identityManagementType = Lens.lens (\InstanceSummary' {identityManagementType} -> identityManagementType) (\s@InstanceSummary' {} a -> s {identityManagementType = a} :: InstanceSummary)

-- | The state of the instance.
instanceSummary_instanceStatus :: Lens.Lens' InstanceSummary (Prelude.Maybe InstanceStatus)
instanceSummary_instanceStatus = Lens.lens (\InstanceSummary' {instanceStatus} -> instanceStatus) (\s@InstanceSummary' {} a -> s {instanceStatus = a} :: InstanceSummary)

-- | The identifier of the instance.
instanceSummary_id :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_id = Lens.lens (\InstanceSummary' {id} -> id) (\s@InstanceSummary' {} a -> s {id = a} :: InstanceSummary)

-- | When the instance was created.
instanceSummary_createdTime :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.UTCTime)
instanceSummary_createdTime = Lens.lens (\InstanceSummary' {createdTime} -> createdTime) (\s@InstanceSummary' {} a -> s {createdTime = a} :: InstanceSummary) Prelude.. Lens.mapping Core._Time

-- | Whether inbound calls are enabled.
instanceSummary_inboundCallsEnabled :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Bool)
instanceSummary_inboundCallsEnabled = Lens.lens (\InstanceSummary' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@InstanceSummary' {} a -> s {inboundCallsEnabled = a} :: InstanceSummary)

instance Core.FromJSON InstanceSummary where
  parseJSON =
    Core.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Prelude.<$> (x Core..:? "InstanceAlias")
            Prelude.<*> (x Core..:? "ServiceRole")
            Prelude.<*> (x Core..:? "OutboundCallsEnabled")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "IdentityManagementType")
            Prelude.<*> (x Core..:? "InstanceStatus")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "InboundCallsEnabled")
      )

instance Prelude.Hashable InstanceSummary

instance Prelude.NFData InstanceSummary
