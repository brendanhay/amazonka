{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CertificateAuthenticationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CertificateAuthenticationRequest
  ( CertificateAuthenticationRequest (..),

    -- * Smart constructor
    mkCertificateAuthenticationRequest,

    -- * Lenses
    carClientRootCertificateChainARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the client certificate to be used for authentication.
--
-- /See:/ 'mkCertificateAuthenticationRequest' smart constructor.
newtype CertificateAuthenticationRequest = CertificateAuthenticationRequest'
  { -- | The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
    clientRootCertificateChainARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateAuthenticationRequest' with the minimum fields required to make a request.
--
-- * 'clientRootCertificateChainARN' - The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
mkCertificateAuthenticationRequest ::
  CertificateAuthenticationRequest
mkCertificateAuthenticationRequest =
  CertificateAuthenticationRequest'
    { clientRootCertificateChainARN =
        Lude.Nothing
    }

-- | The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
--
-- /Note:/ Consider using 'clientRootCertificateChainARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carClientRootCertificateChainARN :: Lens.Lens' CertificateAuthenticationRequest (Lude.Maybe Lude.Text)
carClientRootCertificateChainARN = Lens.lens (clientRootCertificateChainARN :: CertificateAuthenticationRequest -> Lude.Maybe Lude.Text) (\s a -> s {clientRootCertificateChainARN = a} :: CertificateAuthenticationRequest)
{-# DEPRECATED carClientRootCertificateChainARN "Use generic-lens or generic-optics with 'clientRootCertificateChainARN' instead." #-}

instance Lude.ToQuery CertificateAuthenticationRequest where
  toQuery CertificateAuthenticationRequest' {..} =
    Lude.mconcat
      [ "ClientRootCertificateChainArn"
          Lude.=: clientRootCertificateChainARN
      ]
