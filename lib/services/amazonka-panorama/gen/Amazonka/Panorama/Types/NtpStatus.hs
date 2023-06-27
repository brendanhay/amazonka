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
-- Module      : Amazonka.Panorama.Types.NtpStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.NtpStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Panorama.Types.NetworkConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Details about an NTP server connection.
--
-- /See:/ 'newNtpStatus' smart constructor.
data NtpStatus = NtpStatus'
  { -- | The connection\'s status.
    connectionStatus :: Prelude.Maybe NetworkConnectionStatus,
    -- | The IP address of the server.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the server.
    ntpServerName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NtpStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionStatus', 'ntpStatus_connectionStatus' - The connection\'s status.
--
-- 'ipAddress', 'ntpStatus_ipAddress' - The IP address of the server.
--
-- 'ntpServerName', 'ntpStatus_ntpServerName' - The domain name of the server.
newNtpStatus ::
  NtpStatus
newNtpStatus =
  NtpStatus'
    { connectionStatus = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      ntpServerName = Prelude.Nothing
    }

-- | The connection\'s status.
ntpStatus_connectionStatus :: Lens.Lens' NtpStatus (Prelude.Maybe NetworkConnectionStatus)
ntpStatus_connectionStatus = Lens.lens (\NtpStatus' {connectionStatus} -> connectionStatus) (\s@NtpStatus' {} a -> s {connectionStatus = a} :: NtpStatus)

-- | The IP address of the server.
ntpStatus_ipAddress :: Lens.Lens' NtpStatus (Prelude.Maybe Prelude.Text)
ntpStatus_ipAddress = Lens.lens (\NtpStatus' {ipAddress} -> ipAddress) (\s@NtpStatus' {} a -> s {ipAddress = a} :: NtpStatus)

-- | The domain name of the server.
ntpStatus_ntpServerName :: Lens.Lens' NtpStatus (Prelude.Maybe Prelude.Text)
ntpStatus_ntpServerName = Lens.lens (\NtpStatus' {ntpServerName} -> ntpServerName) (\s@NtpStatus' {} a -> s {ntpServerName = a} :: NtpStatus)

instance Data.FromJSON NtpStatus where
  parseJSON =
    Data.withObject
      "NtpStatus"
      ( \x ->
          NtpStatus'
            Prelude.<$> (x Data..:? "ConnectionStatus")
            Prelude.<*> (x Data..:? "IpAddress")
            Prelude.<*> (x Data..:? "NtpServerName")
      )

instance Prelude.Hashable NtpStatus where
  hashWithSalt _salt NtpStatus' {..} =
    _salt
      `Prelude.hashWithSalt` connectionStatus
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` ntpServerName

instance Prelude.NFData NtpStatus where
  rnf NtpStatus' {..} =
    Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf ntpServerName
