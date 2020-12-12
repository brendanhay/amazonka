{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.CertificateConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.CertificateConfiguration
  ( CertificateConfiguration (..),

    -- * Smart constructor
    mkCertificateConfiguration,

    -- * Lenses
    ccCertificateType,
  )
where

import Network.AWS.GameLift.Types.CertificateType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the use of a TLS/SSL certificate for a fleet. TLS certificate generation is enabled at the fleet level, with one certificate generated for the fleet. When this feature is enabled, the certificate can be retrieved using the <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-serversdk.html GameLift Server SDK> call @GetInstanceCertificate@ . All instances in a fleet share the same certificate.
--
-- /See:/ 'mkCertificateConfiguration' smart constructor.
newtype CertificateConfiguration = CertificateConfiguration'
  { certificateType ::
      CertificateType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateConfiguration' with the minimum fields required to make a request.
--
-- * 'certificateType' - Indicates whether a TLS/SSL certificate was generated for a fleet.
mkCertificateConfiguration ::
  -- | 'certificateType'
  CertificateType ->
  CertificateConfiguration
mkCertificateConfiguration pCertificateType_ =
  CertificateConfiguration' {certificateType = pCertificateType_}

-- | Indicates whether a TLS/SSL certificate was generated for a fleet.
--
--
--
--
-- /Note:/ Consider using 'certificateType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccCertificateType :: Lens.Lens' CertificateConfiguration CertificateType
ccCertificateType = Lens.lens (certificateType :: CertificateConfiguration -> CertificateType) (\s a -> s {certificateType = a} :: CertificateConfiguration)
{-# DEPRECATED ccCertificateType "Use generic-lens or generic-optics with 'certificateType' instead." #-}

instance Lude.FromJSON CertificateConfiguration where
  parseJSON =
    Lude.withObject
      "CertificateConfiguration"
      ( \x ->
          CertificateConfiguration' Lude.<$> (x Lude..: "CertificateType")
      )

instance Lude.ToJSON CertificateConfiguration where
  toJSON CertificateConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CertificateType" Lude..= certificateType)]
      )
