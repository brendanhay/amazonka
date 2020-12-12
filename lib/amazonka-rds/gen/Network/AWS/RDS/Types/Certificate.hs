{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cCertificateType,
    cCustomerOverride,
    cCertificateARN,
    cCustomerOverrideValidTill,
    cValidTill,
    cCertificateIdentifier,
    cThumbprint,
    cValidFrom,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A CA certificate for an AWS account.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateType ::
      Lude.Maybe Lude.Text,
    customerOverride :: Lude.Maybe Lude.Bool,
    certificateARN :: Lude.Maybe Lude.Text,
    customerOverrideValidTill :: Lude.Maybe Lude.DateTime,
    validTill :: Lude.Maybe Lude.DateTime,
    certificateIdentifier :: Lude.Maybe Lude.Text,
    thumbprint :: Lude.Maybe Lude.Text,
    validFrom :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) for the certificate.
-- * 'certificateIdentifier' - The unique key that identifies a certificate.
-- * 'certificateType' - The type of the certificate.
-- * 'customerOverride' - Whether there is an override for the default certificate identifier.
-- * 'customerOverrideValidTill' - If there is an override for the default certificate identifier, when the override expires.
-- * 'thumbprint' - The thumbprint of the certificate.
-- * 'validFrom' - The starting date from which the certificate is valid.
-- * 'validTill' - The final date that the certificate continues to be valid.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { certificateType = Lude.Nothing,
      customerOverride = Lude.Nothing,
      certificateARN = Lude.Nothing,
      customerOverrideValidTill = Lude.Nothing,
      validTill = Lude.Nothing,
      certificateIdentifier = Lude.Nothing,
      thumbprint = Lude.Nothing,
      validFrom = Lude.Nothing
    }

-- | The type of the certificate.
--
-- /Note:/ Consider using 'certificateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateType :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateType = Lens.lens (certificateType :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateType = a} :: Certificate)
{-# DEPRECATED cCertificateType "Use generic-lens or generic-optics with 'certificateType' instead." #-}

-- | Whether there is an override for the default certificate identifier.
--
-- /Note:/ Consider using 'customerOverride' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomerOverride :: Lens.Lens' Certificate (Lude.Maybe Lude.Bool)
cCustomerOverride = Lens.lens (customerOverride :: Certificate -> Lude.Maybe Lude.Bool) (\s a -> s {customerOverride = a} :: Certificate)
{-# DEPRECATED cCustomerOverride "Use generic-lens or generic-optics with 'customerOverride' instead." #-}

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateARN :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateARN = Lens.lens (certificateARN :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: Certificate)
{-# DEPRECATED cCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | If there is an override for the default certificate identifier, when the override expires.
--
-- /Note:/ Consider using 'customerOverrideValidTill' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCustomerOverrideValidTill :: Lens.Lens' Certificate (Lude.Maybe Lude.DateTime)
cCustomerOverrideValidTill = Lens.lens (customerOverrideValidTill :: Certificate -> Lude.Maybe Lude.DateTime) (\s a -> s {customerOverrideValidTill = a} :: Certificate)
{-# DEPRECATED cCustomerOverrideValidTill "Use generic-lens or generic-optics with 'customerOverrideValidTill' instead." #-}

-- | The final date that the certificate continues to be valid.
--
-- /Note:/ Consider using 'validTill' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidTill :: Lens.Lens' Certificate (Lude.Maybe Lude.DateTime)
cValidTill = Lens.lens (validTill :: Certificate -> Lude.Maybe Lude.DateTime) (\s a -> s {validTill = a} :: Certificate)
{-# DEPRECATED cValidTill "Use generic-lens or generic-optics with 'validTill' instead." #-}

-- | The unique key that identifies a certificate.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateIdentifier :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateIdentifier = Lens.lens (certificateIdentifier :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateIdentifier = a} :: Certificate)
{-# DEPRECATED cCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | The thumbprint of the certificate.
--
-- /Note:/ Consider using 'thumbprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cThumbprint :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cThumbprint = Lens.lens (thumbprint :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {thumbprint = a} :: Certificate)
{-# DEPRECATED cThumbprint "Use generic-lens or generic-optics with 'thumbprint' instead." #-}

-- | The starting date from which the certificate is valid.
--
-- /Note:/ Consider using 'validFrom' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidFrom :: Lens.Lens' Certificate (Lude.Maybe Lude.DateTime)
cValidFrom = Lens.lens (validFrom :: Certificate -> Lude.Maybe Lude.DateTime) (\s a -> s {validFrom = a} :: Certificate)
{-# DEPRECATED cValidFrom "Use generic-lens or generic-optics with 'validFrom' instead." #-}

instance Lude.FromXML Certificate where
  parseXML x =
    Certificate'
      Lude.<$> (x Lude..@? "CertificateType")
      Lude.<*> (x Lude..@? "CustomerOverride")
      Lude.<*> (x Lude..@? "CertificateArn")
      Lude.<*> (x Lude..@? "CustomerOverrideValidTill")
      Lude.<*> (x Lude..@? "ValidTill")
      Lude.<*> (x Lude..@? "CertificateIdentifier")
      Lude.<*> (x Lude..@? "Thumbprint")
      Lude.<*> (x Lude..@? "ValidFrom")
