{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateValidity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateValidity
  ( CertificateValidity (..),

    -- * Smart constructor
    mkCertificateValidity,

    -- * Lenses
    cvNotBefore,
    cvNotAfter,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When the certificate is valid.
--
-- /See:/ 'mkCertificateValidity' smart constructor.
data CertificateValidity = CertificateValidity'
  { notBefore ::
      Lude.Maybe Lude.Timestamp,
    notAfter :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateValidity' with the minimum fields required to make a request.
--
-- * 'notAfter' - The certificate is not valid after this date.
-- * 'notBefore' - The certificate is not valid before this date.
mkCertificateValidity ::
  CertificateValidity
mkCertificateValidity =
  CertificateValidity'
    { notBefore = Lude.Nothing,
      notAfter = Lude.Nothing
    }

-- | The certificate is not valid before this date.
--
-- /Note:/ Consider using 'notBefore' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvNotBefore :: Lens.Lens' CertificateValidity (Lude.Maybe Lude.Timestamp)
cvNotBefore = Lens.lens (notBefore :: CertificateValidity -> Lude.Maybe Lude.Timestamp) (\s a -> s {notBefore = a} :: CertificateValidity)
{-# DEPRECATED cvNotBefore "Use generic-lens or generic-optics with 'notBefore' instead." #-}

-- | The certificate is not valid after this date.
--
-- /Note:/ Consider using 'notAfter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvNotAfter :: Lens.Lens' CertificateValidity (Lude.Maybe Lude.Timestamp)
cvNotAfter = Lens.lens (notAfter :: CertificateValidity -> Lude.Maybe Lude.Timestamp) (\s a -> s {notAfter = a} :: CertificateValidity)
{-# DEPRECATED cvNotAfter "Use generic-lens or generic-optics with 'notAfter' instead." #-}

instance Lude.FromJSON CertificateValidity where
  parseJSON =
    Lude.withObject
      "CertificateValidity"
      ( \x ->
          CertificateValidity'
            Lude.<$> (x Lude..:? "notBefore") Lude.<*> (x Lude..:? "notAfter")
      )
