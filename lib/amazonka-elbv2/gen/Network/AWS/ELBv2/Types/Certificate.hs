-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cCertificateARN,
    cIsDefault,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about an SSL server certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateARN ::
      Lude.Maybe Lude.Text,
    isDefault :: Lude.Maybe Lude.Bool
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
-- * 'certificateARN' - The Amazon Resource Name (ARN) of the certificate.
-- * 'isDefault' - Indicates whether the certificate is the default certificate. Do not set this value when specifying a certificate as an input. This value is not included in the output when describing a listener, but is included when describing listener certificates.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { certificateARN = Lude.Nothing,
      isDefault = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateARN :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateARN = Lens.lens (certificateARN :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: Certificate)
{-# DEPRECATED cCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | Indicates whether the certificate is the default certificate. Do not set this value when specifying a certificate as an input. This value is not included in the output when describing a listener, but is included when describing listener certificates.
--
-- /Note:/ Consider using 'isDefault' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cIsDefault :: Lens.Lens' Certificate (Lude.Maybe Lude.Bool)
cIsDefault = Lens.lens (isDefault :: Certificate -> Lude.Maybe Lude.Bool) (\s a -> s {isDefault = a} :: Certificate)
{-# DEPRECATED cIsDefault "Use generic-lens or generic-optics with 'isDefault' instead." #-}

instance Lude.FromXML Certificate where
  parseXML x =
    Certificate'
      Lude.<$> (x Lude..@? "CertificateArn") Lude.<*> (x Lude..@? "IsDefault")

instance Lude.ToQuery Certificate where
  toQuery Certificate' {..} =
    Lude.mconcat
      [ "CertificateArn" Lude.=: certificateARN,
        "IsDefault" Lude.=: isDefault
      ]
