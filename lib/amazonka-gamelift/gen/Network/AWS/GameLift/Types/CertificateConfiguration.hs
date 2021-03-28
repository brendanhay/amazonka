{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.CertificateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.CertificateConfiguration
  ( CertificateConfiguration (..)
  -- * Smart constructor
  , mkCertificateConfiguration
  -- * Lenses
  , ccCertificateType
  ) where

import qualified Network.AWS.GameLift.Types.CertificateType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the use of a TLS/SSL certificate for a fleet. TLS certificate generation is enabled at the fleet level, with one certificate generated for the fleet. When this feature is enabled, the certificate can be retrieved using the <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-serversdk.html GameLift Server SDK> call @GetInstanceCertificate@ . All instances in a fleet share the same certificate.
--
-- /See:/ 'mkCertificateConfiguration' smart constructor.
newtype CertificateConfiguration = CertificateConfiguration'
  { certificateType :: Types.CertificateType
    -- ^ Indicates whether a TLS/SSL certificate was generated for a fleet. 
--
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CertificateConfiguration' value with any optional fields omitted.
mkCertificateConfiguration
    :: Types.CertificateType -- ^ 'certificateType'
    -> CertificateConfiguration
mkCertificateConfiguration certificateType
  = CertificateConfiguration'{certificateType}

-- | Indicates whether a TLS/SSL certificate was generated for a fleet. 
--
--
--
--
-- /Note:/ Consider using 'certificateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCertificateType :: Lens.Lens' CertificateConfiguration Types.CertificateType
ccCertificateType = Lens.field @"certificateType"
{-# INLINEABLE ccCertificateType #-}
{-# DEPRECATED certificateType "Use generic-lens or generic-optics with 'certificateType' instead"  #-}

instance Core.FromJSON CertificateConfiguration where
        toJSON CertificateConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CertificateType" Core..= certificateType)])

instance Core.FromJSON CertificateConfiguration where
        parseJSON
          = Core.withObject "CertificateConfiguration" Core.$
              \ x ->
                CertificateConfiguration' Core.<$> (x Core..: "CertificateType")
