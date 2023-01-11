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
-- Module      : Amazonka.GameLift.Types.CertificateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.CertificateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.CertificateType
import qualified Amazonka.Prelude as Prelude

-- | Determines whether a TLS\/SSL certificate is generated for a fleet. This
-- feature must be enabled when creating the fleet. All instances in a
-- fleet share the same certificate. The certificate can be retrieved by
-- calling the
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-serversdk.html GameLift Server SDK>
-- operation @GetInstanceCertificate@.
--
-- /See:/ 'newCertificateConfiguration' smart constructor.
data CertificateConfiguration = CertificateConfiguration'
  { -- | Indicates whether a TLS\/SSL certificate is generated for a fleet.
    --
    -- Valid values include:
    --
    -- -   __GENERATED__ - Generate a TLS\/SSL certificate for this fleet.
    --
    -- -   __DISABLED__ - (default) Do not generate a TLS\/SSL certificate for
    --     this fleet.
    certificateType :: CertificateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateType', 'certificateConfiguration_certificateType' - Indicates whether a TLS\/SSL certificate is generated for a fleet.
--
-- Valid values include:
--
-- -   __GENERATED__ - Generate a TLS\/SSL certificate for this fleet.
--
-- -   __DISABLED__ - (default) Do not generate a TLS\/SSL certificate for
--     this fleet.
newCertificateConfiguration ::
  -- | 'certificateType'
  CertificateType ->
  CertificateConfiguration
newCertificateConfiguration pCertificateType_ =
  CertificateConfiguration'
    { certificateType =
        pCertificateType_
    }

-- | Indicates whether a TLS\/SSL certificate is generated for a fleet.
--
-- Valid values include:
--
-- -   __GENERATED__ - Generate a TLS\/SSL certificate for this fleet.
--
-- -   __DISABLED__ - (default) Do not generate a TLS\/SSL certificate for
--     this fleet.
certificateConfiguration_certificateType :: Lens.Lens' CertificateConfiguration CertificateType
certificateConfiguration_certificateType = Lens.lens (\CertificateConfiguration' {certificateType} -> certificateType) (\s@CertificateConfiguration' {} a -> s {certificateType = a} :: CertificateConfiguration)

instance Data.FromJSON CertificateConfiguration where
  parseJSON =
    Data.withObject
      "CertificateConfiguration"
      ( \x ->
          CertificateConfiguration'
            Prelude.<$> (x Data..: "CertificateType")
      )

instance Prelude.Hashable CertificateConfiguration where
  hashWithSalt _salt CertificateConfiguration' {..} =
    _salt `Prelude.hashWithSalt` certificateType

instance Prelude.NFData CertificateConfiguration where
  rnf CertificateConfiguration' {..} =
    Prelude.rnf certificateType

instance Data.ToJSON CertificateConfiguration where
  toJSON CertificateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CertificateType" Data..= certificateType)
          ]
      )
