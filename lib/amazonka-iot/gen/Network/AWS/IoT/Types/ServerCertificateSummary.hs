{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.ServerCertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ServerCertificateSummary
  ( ServerCertificateSummary (..),

    -- * Smart constructor
    mkServerCertificateSummary,

    -- * Lenses
    scsServerCertificateArn,
    scsServerCertificateStatus,
    scsServerCertificateStatusDetail,
  )
where

import qualified Network.AWS.IoT.Types.AcmCertificateArn as Types
import qualified Network.AWS.IoT.Types.ServerCertificateStatus as Types
import qualified Network.AWS.IoT.Types.ServerCertificateStatusDetail as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains information about a server certificate.
--
-- /See:/ 'mkServerCertificateSummary' smart constructor.
data ServerCertificateSummary = ServerCertificateSummary'
  { -- | The ARN of the server certificate.
    serverCertificateArn :: Core.Maybe Types.AcmCertificateArn,
    -- | The status of the server certificate.
    serverCertificateStatus :: Core.Maybe Types.ServerCertificateStatus,
    -- | Details that explain the status of the server certificate.
    serverCertificateStatusDetail :: Core.Maybe Types.ServerCertificateStatusDetail
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ServerCertificateSummary' value with any optional fields omitted.
mkServerCertificateSummary ::
  ServerCertificateSummary
mkServerCertificateSummary =
  ServerCertificateSummary'
    { serverCertificateArn = Core.Nothing,
      serverCertificateStatus = Core.Nothing,
      serverCertificateStatusDetail = Core.Nothing
    }

-- | The ARN of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsServerCertificateArn :: Lens.Lens' ServerCertificateSummary (Core.Maybe Types.AcmCertificateArn)
scsServerCertificateArn = Lens.field @"serverCertificateArn"
{-# DEPRECATED scsServerCertificateArn "Use generic-lens or generic-optics with 'serverCertificateArn' instead." #-}

-- | The status of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsServerCertificateStatus :: Lens.Lens' ServerCertificateSummary (Core.Maybe Types.ServerCertificateStatus)
scsServerCertificateStatus = Lens.field @"serverCertificateStatus"
{-# DEPRECATED scsServerCertificateStatus "Use generic-lens or generic-optics with 'serverCertificateStatus' instead." #-}

-- | Details that explain the status of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateStatusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsServerCertificateStatusDetail :: Lens.Lens' ServerCertificateSummary (Core.Maybe Types.ServerCertificateStatusDetail)
scsServerCertificateStatusDetail = Lens.field @"serverCertificateStatusDetail"
{-# DEPRECATED scsServerCertificateStatusDetail "Use generic-lens or generic-optics with 'serverCertificateStatusDetail' instead." #-}

instance Core.FromJSON ServerCertificateSummary where
  parseJSON =
    Core.withObject "ServerCertificateSummary" Core.$
      \x ->
        ServerCertificateSummary'
          Core.<$> (x Core..:? "serverCertificateArn")
          Core.<*> (x Core..:? "serverCertificateStatus")
          Core.<*> (x Core..:? "serverCertificateStatusDetail")
