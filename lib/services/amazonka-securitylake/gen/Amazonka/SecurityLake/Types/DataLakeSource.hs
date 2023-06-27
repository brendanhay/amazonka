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
-- Module      : Amazonka.SecurityLake.Types.DataLakeSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.DataLakeSource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.DataLakeSourceStatus

-- | Amazon Security Lake collects logs and events from supported Amazon Web
-- Services and custom sources. For the list of supported Amazon Web
-- Services, see the
-- <https://docs.aws.amazon.com/security-lake/latest/userguide/internal-sources.html Amazon Security Lake User Guide>.
--
-- /See:/ 'newDataLakeSource' smart constructor.
data DataLakeSource = DataLakeSource'
  { -- | The ID of the Security Lake account for which logs are collected.
    account :: Prelude.Maybe Prelude.Text,
    -- | The Open Cybersecurity Schema Framework (OCSF) event classes which
    -- describes the type of data that the custom source will send to Security
    -- Lake. The supported event classes are:
    --
    -- -   @ACCESS_ACTIVITY@
    --
    -- -   @FILE_ACTIVITY@
    --
    -- -   @KERNEL_ACTIVITY@
    --
    -- -   @KERNEL_EXTENSION@
    --
    -- -   @MEMORY_ACTIVITY@
    --
    -- -   @MODULE_ACTIVITY@
    --
    -- -   @PROCESS_ACTIVITY@
    --
    -- -   @REGISTRY_KEY_ACTIVITY@
    --
    -- -   @REGISTRY_VALUE_ACTIVITY@
    --
    -- -   @RESOURCE_ACTIVITY@
    --
    -- -   @SCHEDULED_JOB_ACTIVITY@
    --
    -- -   @SECURITY_FINDING@
    --
    -- -   @ACCOUNT_CHANGE@
    --
    -- -   @AUTHENTICATION@
    --
    -- -   @AUTHORIZATION@
    --
    -- -   @ENTITY_MANAGEMENT_AUDIT@
    --
    -- -   @DHCP_ACTIVITY@
    --
    -- -   @NETWORK_ACTIVITY@
    --
    -- -   @DNS_ACTIVITY@
    --
    -- -   @FTP_ACTIVITY@
    --
    -- -   @HTTP_ACTIVITY@
    --
    -- -   @RDP_ACTIVITY@
    --
    -- -   @SMB_ACTIVITY@
    --
    -- -   @SSH_ACTIVITY@
    --
    -- -   @CONFIG_STATE@
    --
    -- -   @INVENTORY_INFO@
    --
    -- -   @EMAIL_ACTIVITY@
    --
    -- -   @API_ACTIVITY@
    --
    -- -   @CLOUD_API@
    eventClasses :: Prelude.Maybe [Prelude.Text],
    -- | The supported Amazon Web Services from which logs and events are
    -- collected. Amazon Security Lake supports log and event collection for
    -- natively supported Amazon Web Services.
    sourceName :: Prelude.Maybe Prelude.Text,
    -- | The log status for the Security Lake account.
    sourceStatuses :: Prelude.Maybe [DataLakeSourceStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataLakeSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'account', 'dataLakeSource_account' - The ID of the Security Lake account for which logs are collected.
--
-- 'eventClasses', 'dataLakeSource_eventClasses' - The Open Cybersecurity Schema Framework (OCSF) event classes which
-- describes the type of data that the custom source will send to Security
-- Lake. The supported event classes are:
--
-- -   @ACCESS_ACTIVITY@
--
-- -   @FILE_ACTIVITY@
--
-- -   @KERNEL_ACTIVITY@
--
-- -   @KERNEL_EXTENSION@
--
-- -   @MEMORY_ACTIVITY@
--
-- -   @MODULE_ACTIVITY@
--
-- -   @PROCESS_ACTIVITY@
--
-- -   @REGISTRY_KEY_ACTIVITY@
--
-- -   @REGISTRY_VALUE_ACTIVITY@
--
-- -   @RESOURCE_ACTIVITY@
--
-- -   @SCHEDULED_JOB_ACTIVITY@
--
-- -   @SECURITY_FINDING@
--
-- -   @ACCOUNT_CHANGE@
--
-- -   @AUTHENTICATION@
--
-- -   @AUTHORIZATION@
--
-- -   @ENTITY_MANAGEMENT_AUDIT@
--
-- -   @DHCP_ACTIVITY@
--
-- -   @NETWORK_ACTIVITY@
--
-- -   @DNS_ACTIVITY@
--
-- -   @FTP_ACTIVITY@
--
-- -   @HTTP_ACTIVITY@
--
-- -   @RDP_ACTIVITY@
--
-- -   @SMB_ACTIVITY@
--
-- -   @SSH_ACTIVITY@
--
-- -   @CONFIG_STATE@
--
-- -   @INVENTORY_INFO@
--
-- -   @EMAIL_ACTIVITY@
--
-- -   @API_ACTIVITY@
--
-- -   @CLOUD_API@
--
-- 'sourceName', 'dataLakeSource_sourceName' - The supported Amazon Web Services from which logs and events are
-- collected. Amazon Security Lake supports log and event collection for
-- natively supported Amazon Web Services.
--
-- 'sourceStatuses', 'dataLakeSource_sourceStatuses' - The log status for the Security Lake account.
newDataLakeSource ::
  DataLakeSource
newDataLakeSource =
  DataLakeSource'
    { account = Prelude.Nothing,
      eventClasses = Prelude.Nothing,
      sourceName = Prelude.Nothing,
      sourceStatuses = Prelude.Nothing
    }

-- | The ID of the Security Lake account for which logs are collected.
dataLakeSource_account :: Lens.Lens' DataLakeSource (Prelude.Maybe Prelude.Text)
dataLakeSource_account = Lens.lens (\DataLakeSource' {account} -> account) (\s@DataLakeSource' {} a -> s {account = a} :: DataLakeSource)

-- | The Open Cybersecurity Schema Framework (OCSF) event classes which
-- describes the type of data that the custom source will send to Security
-- Lake. The supported event classes are:
--
-- -   @ACCESS_ACTIVITY@
--
-- -   @FILE_ACTIVITY@
--
-- -   @KERNEL_ACTIVITY@
--
-- -   @KERNEL_EXTENSION@
--
-- -   @MEMORY_ACTIVITY@
--
-- -   @MODULE_ACTIVITY@
--
-- -   @PROCESS_ACTIVITY@
--
-- -   @REGISTRY_KEY_ACTIVITY@
--
-- -   @REGISTRY_VALUE_ACTIVITY@
--
-- -   @RESOURCE_ACTIVITY@
--
-- -   @SCHEDULED_JOB_ACTIVITY@
--
-- -   @SECURITY_FINDING@
--
-- -   @ACCOUNT_CHANGE@
--
-- -   @AUTHENTICATION@
--
-- -   @AUTHORIZATION@
--
-- -   @ENTITY_MANAGEMENT_AUDIT@
--
-- -   @DHCP_ACTIVITY@
--
-- -   @NETWORK_ACTIVITY@
--
-- -   @DNS_ACTIVITY@
--
-- -   @FTP_ACTIVITY@
--
-- -   @HTTP_ACTIVITY@
--
-- -   @RDP_ACTIVITY@
--
-- -   @SMB_ACTIVITY@
--
-- -   @SSH_ACTIVITY@
--
-- -   @CONFIG_STATE@
--
-- -   @INVENTORY_INFO@
--
-- -   @EMAIL_ACTIVITY@
--
-- -   @API_ACTIVITY@
--
-- -   @CLOUD_API@
dataLakeSource_eventClasses :: Lens.Lens' DataLakeSource (Prelude.Maybe [Prelude.Text])
dataLakeSource_eventClasses = Lens.lens (\DataLakeSource' {eventClasses} -> eventClasses) (\s@DataLakeSource' {} a -> s {eventClasses = a} :: DataLakeSource) Prelude.. Lens.mapping Lens.coerced

-- | The supported Amazon Web Services from which logs and events are
-- collected. Amazon Security Lake supports log and event collection for
-- natively supported Amazon Web Services.
dataLakeSource_sourceName :: Lens.Lens' DataLakeSource (Prelude.Maybe Prelude.Text)
dataLakeSource_sourceName = Lens.lens (\DataLakeSource' {sourceName} -> sourceName) (\s@DataLakeSource' {} a -> s {sourceName = a} :: DataLakeSource)

-- | The log status for the Security Lake account.
dataLakeSource_sourceStatuses :: Lens.Lens' DataLakeSource (Prelude.Maybe [DataLakeSourceStatus])
dataLakeSource_sourceStatuses = Lens.lens (\DataLakeSource' {sourceStatuses} -> sourceStatuses) (\s@DataLakeSource' {} a -> s {sourceStatuses = a} :: DataLakeSource) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON DataLakeSource where
  parseJSON =
    Data.withObject
      "DataLakeSource"
      ( \x ->
          DataLakeSource'
            Prelude.<$> (x Data..:? "account")
            Prelude.<*> (x Data..:? "eventClasses" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sourceName")
            Prelude.<*> ( x
                            Data..:? "sourceStatuses"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataLakeSource where
  hashWithSalt _salt DataLakeSource' {..} =
    _salt
      `Prelude.hashWithSalt` account
      `Prelude.hashWithSalt` eventClasses
      `Prelude.hashWithSalt` sourceName
      `Prelude.hashWithSalt` sourceStatuses

instance Prelude.NFData DataLakeSource where
  rnf DataLakeSource' {..} =
    Prelude.rnf account
      `Prelude.seq` Prelude.rnf eventClasses
      `Prelude.seq` Prelude.rnf sourceName
      `Prelude.seq` Prelude.rnf sourceStatuses
