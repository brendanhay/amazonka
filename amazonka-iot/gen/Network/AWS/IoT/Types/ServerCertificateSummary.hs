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
-- Module      : Network.AWS.IoT.Types.ServerCertificateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ServerCertificateSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.ServerCertificateStatus
import qualified Network.AWS.Lens as Lens

-- | An object that contains information about a server certificate.
--
-- /See:/ 'newServerCertificateSummary' smart constructor.
data ServerCertificateSummary = ServerCertificateSummary'
  { -- | The status of the server certificate.
    serverCertificateStatus :: Core.Maybe ServerCertificateStatus,
    -- | The ARN of the server certificate.
    serverCertificateArn :: Core.Maybe Core.Text,
    -- | Details that explain the status of the server certificate.
    serverCertificateStatusDetail :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ServerCertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverCertificateStatus', 'serverCertificateSummary_serverCertificateStatus' - The status of the server certificate.
--
-- 'serverCertificateArn', 'serverCertificateSummary_serverCertificateArn' - The ARN of the server certificate.
--
-- 'serverCertificateStatusDetail', 'serverCertificateSummary_serverCertificateStatusDetail' - Details that explain the status of the server certificate.
newServerCertificateSummary ::
  ServerCertificateSummary
newServerCertificateSummary =
  ServerCertificateSummary'
    { serverCertificateStatus =
        Core.Nothing,
      serverCertificateArn = Core.Nothing,
      serverCertificateStatusDetail = Core.Nothing
    }

-- | The status of the server certificate.
serverCertificateSummary_serverCertificateStatus :: Lens.Lens' ServerCertificateSummary (Core.Maybe ServerCertificateStatus)
serverCertificateSummary_serverCertificateStatus = Lens.lens (\ServerCertificateSummary' {serverCertificateStatus} -> serverCertificateStatus) (\s@ServerCertificateSummary' {} a -> s {serverCertificateStatus = a} :: ServerCertificateSummary)

-- | The ARN of the server certificate.
serverCertificateSummary_serverCertificateArn :: Lens.Lens' ServerCertificateSummary (Core.Maybe Core.Text)
serverCertificateSummary_serverCertificateArn = Lens.lens (\ServerCertificateSummary' {serverCertificateArn} -> serverCertificateArn) (\s@ServerCertificateSummary' {} a -> s {serverCertificateArn = a} :: ServerCertificateSummary)

-- | Details that explain the status of the server certificate.
serverCertificateSummary_serverCertificateStatusDetail :: Lens.Lens' ServerCertificateSummary (Core.Maybe Core.Text)
serverCertificateSummary_serverCertificateStatusDetail = Lens.lens (\ServerCertificateSummary' {serverCertificateStatusDetail} -> serverCertificateStatusDetail) (\s@ServerCertificateSummary' {} a -> s {serverCertificateStatusDetail = a} :: ServerCertificateSummary)

instance Core.FromJSON ServerCertificateSummary where
  parseJSON =
    Core.withObject
      "ServerCertificateSummary"
      ( \x ->
          ServerCertificateSummary'
            Core.<$> (x Core..:? "serverCertificateStatus")
            Core.<*> (x Core..:? "serverCertificateArn")
            Core.<*> (x Core..:? "serverCertificateStatusDetail")
      )

instance Core.Hashable ServerCertificateSummary

instance Core.NFData ServerCertificateSummary
