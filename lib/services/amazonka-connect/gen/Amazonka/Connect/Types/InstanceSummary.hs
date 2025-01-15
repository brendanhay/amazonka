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
-- Module      : Amazonka.Connect.Types.InstanceSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.InstanceSummary where

import Amazonka.Connect.Types.DirectoryType
import Amazonka.Connect.Types.InstanceStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about the instance.
--
-- /See:/ 'newInstanceSummary' smart constructor.
data InstanceSummary = InstanceSummary'
  { -- | The Amazon Resource Name (ARN) of the instance.
    arn :: Prelude.Maybe Prelude.Text,
    -- | When the instance was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the instance.
    id :: Prelude.Maybe Prelude.Text,
    -- | The identity management type of the instance.
    identityManagementType :: Prelude.Maybe DirectoryType,
    -- | Whether inbound calls are enabled.
    inboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The alias of the instance.
    instanceAlias :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The state of the instance.
    instanceStatus :: Prelude.Maybe InstanceStatus,
    -- | Whether outbound calls are enabled.
    outboundCallsEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The service role of the instance.
    serviceRole :: Prelude.Maybe Prelude.Text
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
-- 'arn', 'instanceSummary_arn' - The Amazon Resource Name (ARN) of the instance.
--
-- 'createdTime', 'instanceSummary_createdTime' - When the instance was created.
--
-- 'id', 'instanceSummary_id' - The identifier of the instance.
--
-- 'identityManagementType', 'instanceSummary_identityManagementType' - The identity management type of the instance.
--
-- 'inboundCallsEnabled', 'instanceSummary_inboundCallsEnabled' - Whether inbound calls are enabled.
--
-- 'instanceAlias', 'instanceSummary_instanceAlias' - The alias of the instance.
--
-- 'instanceStatus', 'instanceSummary_instanceStatus' - The state of the instance.
--
-- 'outboundCallsEnabled', 'instanceSummary_outboundCallsEnabled' - Whether outbound calls are enabled.
--
-- 'serviceRole', 'instanceSummary_serviceRole' - The service role of the instance.
newInstanceSummary ::
  InstanceSummary
newInstanceSummary =
  InstanceSummary'
    { arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      id = Prelude.Nothing,
      identityManagementType = Prelude.Nothing,
      inboundCallsEnabled = Prelude.Nothing,
      instanceAlias = Prelude.Nothing,
      instanceStatus = Prelude.Nothing,
      outboundCallsEnabled = Prelude.Nothing,
      serviceRole = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the instance.
instanceSummary_arn :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_arn = Lens.lens (\InstanceSummary' {arn} -> arn) (\s@InstanceSummary' {} a -> s {arn = a} :: InstanceSummary)

-- | When the instance was created.
instanceSummary_createdTime :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.UTCTime)
instanceSummary_createdTime = Lens.lens (\InstanceSummary' {createdTime} -> createdTime) (\s@InstanceSummary' {} a -> s {createdTime = a} :: InstanceSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the instance.
instanceSummary_id :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_id = Lens.lens (\InstanceSummary' {id} -> id) (\s@InstanceSummary' {} a -> s {id = a} :: InstanceSummary)

-- | The identity management type of the instance.
instanceSummary_identityManagementType :: Lens.Lens' InstanceSummary (Prelude.Maybe DirectoryType)
instanceSummary_identityManagementType = Lens.lens (\InstanceSummary' {identityManagementType} -> identityManagementType) (\s@InstanceSummary' {} a -> s {identityManagementType = a} :: InstanceSummary)

-- | Whether inbound calls are enabled.
instanceSummary_inboundCallsEnabled :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Bool)
instanceSummary_inboundCallsEnabled = Lens.lens (\InstanceSummary' {inboundCallsEnabled} -> inboundCallsEnabled) (\s@InstanceSummary' {} a -> s {inboundCallsEnabled = a} :: InstanceSummary)

-- | The alias of the instance.
instanceSummary_instanceAlias :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_instanceAlias = Lens.lens (\InstanceSummary' {instanceAlias} -> instanceAlias) (\s@InstanceSummary' {} a -> s {instanceAlias = a} :: InstanceSummary) Prelude.. Lens.mapping Data._Sensitive

-- | The state of the instance.
instanceSummary_instanceStatus :: Lens.Lens' InstanceSummary (Prelude.Maybe InstanceStatus)
instanceSummary_instanceStatus = Lens.lens (\InstanceSummary' {instanceStatus} -> instanceStatus) (\s@InstanceSummary' {} a -> s {instanceStatus = a} :: InstanceSummary)

-- | Whether outbound calls are enabled.
instanceSummary_outboundCallsEnabled :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Bool)
instanceSummary_outboundCallsEnabled = Lens.lens (\InstanceSummary' {outboundCallsEnabled} -> outboundCallsEnabled) (\s@InstanceSummary' {} a -> s {outboundCallsEnabled = a} :: InstanceSummary)

-- | The service role of the instance.
instanceSummary_serviceRole :: Lens.Lens' InstanceSummary (Prelude.Maybe Prelude.Text)
instanceSummary_serviceRole = Lens.lens (\InstanceSummary' {serviceRole} -> serviceRole) (\s@InstanceSummary' {} a -> s {serviceRole = a} :: InstanceSummary)

instance Data.FromJSON InstanceSummary where
  parseJSON =
    Data.withObject
      "InstanceSummary"
      ( \x ->
          InstanceSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "IdentityManagementType")
            Prelude.<*> (x Data..:? "InboundCallsEnabled")
            Prelude.<*> (x Data..:? "InstanceAlias")
            Prelude.<*> (x Data..:? "InstanceStatus")
            Prelude.<*> (x Data..:? "OutboundCallsEnabled")
            Prelude.<*> (x Data..:? "ServiceRole")
      )

instance Prelude.Hashable InstanceSummary where
  hashWithSalt _salt InstanceSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` identityManagementType
      `Prelude.hashWithSalt` inboundCallsEnabled
      `Prelude.hashWithSalt` instanceAlias
      `Prelude.hashWithSalt` instanceStatus
      `Prelude.hashWithSalt` outboundCallsEnabled
      `Prelude.hashWithSalt` serviceRole

instance Prelude.NFData InstanceSummary where
  rnf InstanceSummary' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf createdTime `Prelude.seq`
        Prelude.rnf id `Prelude.seq`
          Prelude.rnf identityManagementType `Prelude.seq`
            Prelude.rnf inboundCallsEnabled `Prelude.seq`
              Prelude.rnf instanceAlias `Prelude.seq`
                Prelude.rnf instanceStatus `Prelude.seq`
                  Prelude.rnf outboundCallsEnabled `Prelude.seq`
                    Prelude.rnf serviceRole
