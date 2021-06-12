{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.CertificateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.CertificateConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types.CertificateType
import qualified Network.AWS.Lens as Lens

-- | Information about the use of a TLS\/SSL certificate for a fleet. TLS
-- certificate generation is enabled at the fleet level, with one
-- certificate generated for the fleet. When this feature is enabled, the
-- certificate can be retrieved using the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-serversdk.html GameLift Server SDK>
-- call @GetInstanceCertificate@. All instances in a fleet share the same
-- certificate.
--
-- /See:/ 'newCertificateConfiguration' smart constructor.
data CertificateConfiguration = CertificateConfiguration'
  { -- | Indicates whether a TLS\/SSL certificate was generated for a fleet.
    certificateType :: CertificateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CertificateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateType', 'certificateConfiguration_certificateType' - Indicates whether a TLS\/SSL certificate was generated for a fleet.
newCertificateConfiguration ::
  -- | 'certificateType'
  CertificateType ->
  CertificateConfiguration
newCertificateConfiguration pCertificateType_ =
  CertificateConfiguration'
    { certificateType =
        pCertificateType_
    }

-- | Indicates whether a TLS\/SSL certificate was generated for a fleet.
certificateConfiguration_certificateType :: Lens.Lens' CertificateConfiguration CertificateType
certificateConfiguration_certificateType = Lens.lens (\CertificateConfiguration' {certificateType} -> certificateType) (\s@CertificateConfiguration' {} a -> s {certificateType = a} :: CertificateConfiguration)

instance Core.FromJSON CertificateConfiguration where
  parseJSON =
    Core.withObject
      "CertificateConfiguration"
      ( \x ->
          CertificateConfiguration'
            Core.<$> (x Core..: "CertificateType")
      )

instance Core.Hashable CertificateConfiguration

instance Core.NFData CertificateConfiguration

instance Core.ToJSON CertificateConfiguration where
  toJSON CertificateConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CertificateType" Core..= certificateType)
          ]
      )
