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
-- Module      : Amazonka.IoT.Types.ServerCertificateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ServerCertificateSummary where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.ServerCertificateStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains information about a server certificate.
--
-- /See:/ 'newServerCertificateSummary' smart constructor.
data ServerCertificateSummary = ServerCertificateSummary'
  { -- | Details that explain the status of the server certificate.
    serverCertificateStatusDetail :: Prelude.Maybe Prelude.Text,
    -- | The status of the server certificate.
    serverCertificateStatus :: Prelude.Maybe ServerCertificateStatus,
    -- | The ARN of the server certificate.
    serverCertificateArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerCertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateStatusDetail', 'serverCertificateSummary_serverCertificateStatusDetail' - Details that explain the status of the server certificate.
--
-- 'serverCertificateStatus', 'serverCertificateSummary_serverCertificateStatus' - The status of the server certificate.
--
-- 'serverCertificateArn', 'serverCertificateSummary_serverCertificateArn' - The ARN of the server certificate.
newServerCertificateSummary ::
  ServerCertificateSummary
newServerCertificateSummary =
  ServerCertificateSummary'
    { serverCertificateStatusDetail =
        Prelude.Nothing,
      serverCertificateStatus = Prelude.Nothing,
      serverCertificateArn = Prelude.Nothing
    }

-- | Details that explain the status of the server certificate.
serverCertificateSummary_serverCertificateStatusDetail :: Lens.Lens' ServerCertificateSummary (Prelude.Maybe Prelude.Text)
serverCertificateSummary_serverCertificateStatusDetail = Lens.lens (\ServerCertificateSummary' {serverCertificateStatusDetail} -> serverCertificateStatusDetail) (\s@ServerCertificateSummary' {} a -> s {serverCertificateStatusDetail = a} :: ServerCertificateSummary)

-- | The status of the server certificate.
serverCertificateSummary_serverCertificateStatus :: Lens.Lens' ServerCertificateSummary (Prelude.Maybe ServerCertificateStatus)
serverCertificateSummary_serverCertificateStatus = Lens.lens (\ServerCertificateSummary' {serverCertificateStatus} -> serverCertificateStatus) (\s@ServerCertificateSummary' {} a -> s {serverCertificateStatus = a} :: ServerCertificateSummary)

-- | The ARN of the server certificate.
serverCertificateSummary_serverCertificateArn :: Lens.Lens' ServerCertificateSummary (Prelude.Maybe Prelude.Text)
serverCertificateSummary_serverCertificateArn = Lens.lens (\ServerCertificateSummary' {serverCertificateArn} -> serverCertificateArn) (\s@ServerCertificateSummary' {} a -> s {serverCertificateArn = a} :: ServerCertificateSummary)

instance Core.FromJSON ServerCertificateSummary where
  parseJSON =
    Core.withObject
      "ServerCertificateSummary"
      ( \x ->
          ServerCertificateSummary'
            Prelude.<$> (x Core..:? "serverCertificateStatusDetail")
            Prelude.<*> (x Core..:? "serverCertificateStatus")
            Prelude.<*> (x Core..:? "serverCertificateArn")
      )

instance Prelude.Hashable ServerCertificateSummary

instance Prelude.NFData ServerCertificateSummary
