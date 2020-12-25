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
    carClientRootCertificateChainArn,
  )
where

import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the client certificate to be used for authentication.
--
-- /See:/ 'mkCertificateAuthenticationRequest' smart constructor.
newtype CertificateAuthenticationRequest = CertificateAuthenticationRequest'
  { -- | The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
    clientRootCertificateChainArn :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CertificateAuthenticationRequest' value with any optional fields omitted.
mkCertificateAuthenticationRequest ::
  CertificateAuthenticationRequest
mkCertificateAuthenticationRequest =
  CertificateAuthenticationRequest'
    { clientRootCertificateChainArn =
        Core.Nothing
    }

-- | The ARN of the client certificate. The certificate must be signed by a certificate authority (CA) and it must be provisioned in AWS Certificate Manager (ACM).
--
-- /Note:/ Consider using 'clientRootCertificateChainArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carClientRootCertificateChainArn :: Lens.Lens' CertificateAuthenticationRequest (Core.Maybe Types.String)
carClientRootCertificateChainArn = Lens.field @"clientRootCertificateChainArn"
{-# DEPRECATED carClientRootCertificateChainArn "Use generic-lens or generic-optics with 'clientRootCertificateChainArn' instead." #-}
