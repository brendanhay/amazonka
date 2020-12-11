-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CertificateAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CertificateAuthentication
  ( CertificateAuthentication (..),

    -- * Smart constructor
    mkCertificateAuthentication,

    -- * Lenses
    caClientRootCertificateChain,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the client certificate used for authentication.
--
-- /See:/ 'mkCertificateAuthentication' smart constructor.
newtype CertificateAuthentication = CertificateAuthentication'
  { clientRootCertificateChain ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateAuthentication' with the minimum fields required to make a request.
--
-- * 'clientRootCertificateChain' - The ARN of the client certificate.
mkCertificateAuthentication ::
  CertificateAuthentication
mkCertificateAuthentication =
  CertificateAuthentication'
    { clientRootCertificateChain =
        Lude.Nothing
    }

-- | The ARN of the client certificate.
--
-- /Note:/ Consider using 'clientRootCertificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caClientRootCertificateChain :: Lens.Lens' CertificateAuthentication (Lude.Maybe Lude.Text)
caClientRootCertificateChain = Lens.lens (clientRootCertificateChain :: CertificateAuthentication -> Lude.Maybe Lude.Text) (\s a -> s {clientRootCertificateChain = a} :: CertificateAuthentication)
{-# DEPRECATED caClientRootCertificateChain "Use generic-lens or generic-optics with 'clientRootCertificateChain' instead." #-}

instance Lude.FromXML CertificateAuthentication where
  parseXML x =
    CertificateAuthentication'
      Lude.<$> (x Lude..@? "clientRootCertificateChain")
