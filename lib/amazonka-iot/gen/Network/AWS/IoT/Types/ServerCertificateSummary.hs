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
    scsServerCertificateStatusDetail,
    scsServerCertificateStatus,
    scsServerCertificateARN,
  )
where

import Network.AWS.IoT.Types.ServerCertificateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains information about a server certificate.
--
-- /See:/ 'mkServerCertificateSummary' smart constructor.
data ServerCertificateSummary = ServerCertificateSummary'
  { serverCertificateStatusDetail ::
      Lude.Maybe Lude.Text,
    serverCertificateStatus ::
      Lude.Maybe ServerCertificateStatus,
    serverCertificateARN ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ServerCertificateSummary' with the minimum fields required to make a request.
--
-- * 'serverCertificateARN' - The ARN of the server certificate.
-- * 'serverCertificateStatus' - The status of the server certificate.
-- * 'serverCertificateStatusDetail' - Details that explain the status of the server certificate.
mkServerCertificateSummary ::
  ServerCertificateSummary
mkServerCertificateSummary =
  ServerCertificateSummary'
    { serverCertificateStatusDetail =
        Lude.Nothing,
      serverCertificateStatus = Lude.Nothing,
      serverCertificateARN = Lude.Nothing
    }

-- | Details that explain the status of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateStatusDetail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsServerCertificateStatusDetail :: Lens.Lens' ServerCertificateSummary (Lude.Maybe Lude.Text)
scsServerCertificateStatusDetail = Lens.lens (serverCertificateStatusDetail :: ServerCertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {serverCertificateStatusDetail = a} :: ServerCertificateSummary)
{-# DEPRECATED scsServerCertificateStatusDetail "Use generic-lens or generic-optics with 'serverCertificateStatusDetail' instead." #-}

-- | The status of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsServerCertificateStatus :: Lens.Lens' ServerCertificateSummary (Lude.Maybe ServerCertificateStatus)
scsServerCertificateStatus = Lens.lens (serverCertificateStatus :: ServerCertificateSummary -> Lude.Maybe ServerCertificateStatus) (\s a -> s {serverCertificateStatus = a} :: ServerCertificateSummary)
{-# DEPRECATED scsServerCertificateStatus "Use generic-lens or generic-optics with 'serverCertificateStatus' instead." #-}

-- | The ARN of the server certificate.
--
-- /Note:/ Consider using 'serverCertificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsServerCertificateARN :: Lens.Lens' ServerCertificateSummary (Lude.Maybe Lude.Text)
scsServerCertificateARN = Lens.lens (serverCertificateARN :: ServerCertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {serverCertificateARN = a} :: ServerCertificateSummary)
{-# DEPRECATED scsServerCertificateARN "Use generic-lens or generic-optics with 'serverCertificateARN' instead." #-}

instance Lude.FromJSON ServerCertificateSummary where
  parseJSON =
    Lude.withObject
      "ServerCertificateSummary"
      ( \x ->
          ServerCertificateSummary'
            Lude.<$> (x Lude..:? "serverCertificateStatusDetail")
            Lude.<*> (x Lude..:? "serverCertificateStatus")
            Lude.<*> (x Lude..:? "serverCertificateArn")
      )
