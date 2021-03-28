{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.CertificateAuthentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.CertificateAuthentication
  ( CertificateAuthentication (..)
  -- * Smart constructor
  , mkCertificateAuthentication
  -- * Lenses
  , caClientRootCertificateChain
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the client certificate used for authentication.
--
-- /See:/ 'mkCertificateAuthentication' smart constructor.
newtype CertificateAuthentication = CertificateAuthentication'
  { clientRootCertificateChain :: Core.Maybe Core.Text
    -- ^ The ARN of the client certificate. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CertificateAuthentication' value with any optional fields omitted.
mkCertificateAuthentication
    :: CertificateAuthentication
mkCertificateAuthentication
  = CertificateAuthentication'{clientRootCertificateChain =
                                 Core.Nothing}

-- | The ARN of the client certificate. 
--
-- /Note:/ Consider using 'clientRootCertificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caClientRootCertificateChain :: Lens.Lens' CertificateAuthentication (Core.Maybe Core.Text)
caClientRootCertificateChain = Lens.field @"clientRootCertificateChain"
{-# INLINEABLE caClientRootCertificateChain #-}
{-# DEPRECATED clientRootCertificateChain "Use generic-lens or generic-optics with 'clientRootCertificateChain' instead"  #-}

instance Core.FromXML CertificateAuthentication where
        parseXML x
          = CertificateAuthentication' Core.<$>
              (x Core..@? "clientRootCertificateChain")
