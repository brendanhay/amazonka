{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudHSMv2.Types.Certificates
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudHSMv2.Types.Certificates
  ( Certificates (..),

    -- * Smart constructor
    mkCertificates,

    -- * Lenses
    cManufacturerHardwareCertificate,
    cClusterCSR,
    cHSMCertificate,
    cClusterCertificate,
    cAWSHardwareCertificate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains one or more certificates or a certificate signing request (CSR).
--
-- /See:/ 'mkCertificates' smart constructor.
data Certificates = Certificates'
  { manufacturerHardwareCertificate ::
      Lude.Maybe Lude.Text,
    clusterCSR :: Lude.Maybe Lude.Text,
    hsmCertificate :: Lude.Maybe Lude.Text,
    clusterCertificate :: Lude.Maybe Lude.Text,
    awsHardwareCertificate :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Certificates' with the minimum fields required to make a request.
--
-- * 'awsHardwareCertificate' - The HSM hardware certificate issued (signed) by AWS CloudHSM.
-- * 'clusterCSR' - The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
-- * 'clusterCertificate' - The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
-- * 'hsmCertificate' - The HSM certificate issued (signed) by the HSM hardware.
-- * 'manufacturerHardwareCertificate' - The HSM hardware certificate issued (signed) by the hardware manufacturer.
mkCertificates ::
  Certificates
mkCertificates =
  Certificates'
    { manufacturerHardwareCertificate = Lude.Nothing,
      clusterCSR = Lude.Nothing,
      hsmCertificate = Lude.Nothing,
      clusterCertificate = Lude.Nothing,
      awsHardwareCertificate = Lude.Nothing
    }

-- | The HSM hardware certificate issued (signed) by the hardware manufacturer.
--
-- /Note:/ Consider using 'manufacturerHardwareCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cManufacturerHardwareCertificate :: Lens.Lens' Certificates (Lude.Maybe Lude.Text)
cManufacturerHardwareCertificate = Lens.lens (manufacturerHardwareCertificate :: Certificates -> Lude.Maybe Lude.Text) (\s a -> s {manufacturerHardwareCertificate = a} :: Certificates)
{-# DEPRECATED cManufacturerHardwareCertificate "Use generic-lens or generic-optics with 'manufacturerHardwareCertificate' instead." #-}

-- | The cluster's certificate signing request (CSR). The CSR exists only when the cluster's state is @UNINITIALIZED@ .
--
-- /Note:/ Consider using 'clusterCSR' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCSR :: Lens.Lens' Certificates (Lude.Maybe Lude.Text)
cClusterCSR = Lens.lens (clusterCSR :: Certificates -> Lude.Maybe Lude.Text) (\s a -> s {clusterCSR = a} :: Certificates)
{-# DEPRECATED cClusterCSR "Use generic-lens or generic-optics with 'clusterCSR' instead." #-}

-- | The HSM certificate issued (signed) by the HSM hardware.
--
-- /Note:/ Consider using 'hsmCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cHSMCertificate :: Lens.Lens' Certificates (Lude.Maybe Lude.Text)
cHSMCertificate = Lens.lens (hsmCertificate :: Certificates -> Lude.Maybe Lude.Text) (\s a -> s {hsmCertificate = a} :: Certificates)
{-# DEPRECATED cHSMCertificate "Use generic-lens or generic-optics with 'hsmCertificate' instead." #-}

-- | The cluster certificate issued (signed) by the issuing certificate authority (CA) of the cluster's owner.
--
-- /Note:/ Consider using 'clusterCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cClusterCertificate :: Lens.Lens' Certificates (Lude.Maybe Lude.Text)
cClusterCertificate = Lens.lens (clusterCertificate :: Certificates -> Lude.Maybe Lude.Text) (\s a -> s {clusterCertificate = a} :: Certificates)
{-# DEPRECATED cClusterCertificate "Use generic-lens or generic-optics with 'clusterCertificate' instead." #-}

-- | The HSM hardware certificate issued (signed) by AWS CloudHSM.
--
-- /Note:/ Consider using 'awsHardwareCertificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAWSHardwareCertificate :: Lens.Lens' Certificates (Lude.Maybe Lude.Text)
cAWSHardwareCertificate = Lens.lens (awsHardwareCertificate :: Certificates -> Lude.Maybe Lude.Text) (\s a -> s {awsHardwareCertificate = a} :: Certificates)
{-# DEPRECATED cAWSHardwareCertificate "Use generic-lens or generic-optics with 'awsHardwareCertificate' instead." #-}

instance Lude.FromJSON Certificates where
  parseJSON =
    Lude.withObject
      "Certificates"
      ( \x ->
          Certificates'
            Lude.<$> (x Lude..:? "ManufacturerHardwareCertificate")
            Lude.<*> (x Lude..:? "ClusterCsr")
            Lude.<*> (x Lude..:? "HsmCertificate")
            Lude.<*> (x Lude..:? "ClusterCertificate")
            Lude.<*> (x Lude..:? "AwsHardwareCertificate")
      )
