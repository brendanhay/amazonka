{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.ServerCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IAM.Types.ServerCertificate
  ( ServerCertificate (..)
  -- * Smart constructor
  , mkServerCertificate
  -- * Lenses
  , sServerCertificateMetadata
  , sCertificateBody
  , sCertificateChain
  ) where

import qualified Network.AWS.IAM.Types.CertificateBodyType as Types
import qualified Network.AWS.IAM.Types.CertificateChainType as Types
import qualified Network.AWS.IAM.Types.ServerCertificateMetadata as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a server certificate.
--
-- This data type is used as a response element in the 'GetServerCertificate' operation. 
--
-- /See:/ 'mkServerCertificate' smart constructor.
data ServerCertificate = ServerCertificate'
  { serverCertificateMetadata :: Types.ServerCertificateMetadata
    -- ^ The meta information of the server certificate, such as its name, path, ID, and ARN.
  , certificateBody :: Types.CertificateBodyType
    -- ^ The contents of the public key certificate.
  , certificateChain :: Core.Maybe Types.CertificateChainType
    -- ^ The contents of the public key certificate chain.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ServerCertificate' value with any optional fields omitted.
mkServerCertificate
    :: Types.ServerCertificateMetadata -- ^ 'serverCertificateMetadata'
    -> Types.CertificateBodyType -- ^ 'certificateBody'
    -> ServerCertificate
mkServerCertificate serverCertificateMetadata certificateBody
  = ServerCertificate'{serverCertificateMetadata, certificateBody,
                       certificateChain = Core.Nothing}

-- | The meta information of the server certificate, such as its name, path, ID, and ARN.
--
-- /Note:/ Consider using 'serverCertificateMetadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sServerCertificateMetadata :: Lens.Lens' ServerCertificate Types.ServerCertificateMetadata
sServerCertificateMetadata = Lens.field @"serverCertificateMetadata"
{-# INLINEABLE sServerCertificateMetadata #-}
{-# DEPRECATED serverCertificateMetadata "Use generic-lens or generic-optics with 'serverCertificateMetadata' instead"  #-}

-- | The contents of the public key certificate.
--
-- /Note:/ Consider using 'certificateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCertificateBody :: Lens.Lens' ServerCertificate Types.CertificateBodyType
sCertificateBody = Lens.field @"certificateBody"
{-# INLINEABLE sCertificateBody #-}
{-# DEPRECATED certificateBody "Use generic-lens or generic-optics with 'certificateBody' instead"  #-}

-- | The contents of the public key certificate chain.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCertificateChain :: Lens.Lens' ServerCertificate (Core.Maybe Types.CertificateChainType)
sCertificateChain = Lens.field @"certificateChain"
{-# INLINEABLE sCertificateChain #-}
{-# DEPRECATED certificateChain "Use generic-lens or generic-optics with 'certificateChain' instead"  #-}

instance Core.FromXML ServerCertificate where
        parseXML x
          = ServerCertificate' Core.<$>
              (x Core..@ "ServerCertificateMetadata") Core.<*>
                x Core..@ "CertificateBody"
                Core.<*> x Core..@? "CertificateChain"
