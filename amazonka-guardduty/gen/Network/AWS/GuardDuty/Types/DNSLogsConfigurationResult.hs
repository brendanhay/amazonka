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
-- Module      : Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.DNSLogsConfigurationResult where

import Network.AWS.GuardDuty.Types.DataSourceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information on the status of DNS logs as a data source.
--
-- /See:/ 'newDNSLogsConfigurationResult' smart constructor.
data DNSLogsConfigurationResult = DNSLogsConfigurationResult'
  { -- | Denotes whether DNS logs is enabled as a data source.
    status :: DataSourceStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DNSLogsConfigurationResult' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'dNSLogsConfigurationResult_status' - Denotes whether DNS logs is enabled as a data source.
newDNSLogsConfigurationResult ::
  -- | 'status'
  DataSourceStatus ->
  DNSLogsConfigurationResult
newDNSLogsConfigurationResult pStatus_ =
  DNSLogsConfigurationResult' {status = pStatus_}

-- | Denotes whether DNS logs is enabled as a data source.
dNSLogsConfigurationResult_status :: Lens.Lens' DNSLogsConfigurationResult DataSourceStatus
dNSLogsConfigurationResult_status = Lens.lens (\DNSLogsConfigurationResult' {status} -> status) (\s@DNSLogsConfigurationResult' {} a -> s {status = a} :: DNSLogsConfigurationResult)

instance Prelude.FromJSON DNSLogsConfigurationResult where
  parseJSON =
    Prelude.withObject
      "DNSLogsConfigurationResult"
      ( \x ->
          DNSLogsConfigurationResult'
            Prelude.<$> (x Prelude..: "status")
      )

instance Prelude.Hashable DNSLogsConfigurationResult

instance Prelude.NFData DNSLogsConfigurationResult
