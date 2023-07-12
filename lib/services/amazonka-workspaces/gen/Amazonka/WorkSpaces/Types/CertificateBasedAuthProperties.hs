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
-- Module      : Amazonka.WorkSpaces.Types.CertificateBasedAuthProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.CertificateBasedAuthProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.WorkSpaces.Types.CertificateBasedAuthStatusEnum

-- | Describes the properties of the certificate-based authentication you
-- want to use with your WorkSpaces.
--
-- /See:/ 'newCertificateBasedAuthProperties' smart constructor.
data CertificateBasedAuthProperties = CertificateBasedAuthProperties'
  { -- | The Amazon Resource Name (ARN) of the Amazon Web Services Certificate
    -- Manager Private CA resource.
    certificateAuthorityArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the certificate-based authentication properties.
    status :: Prelude.Maybe CertificateBasedAuthStatusEnum
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateBasedAuthProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAuthorityArn', 'certificateBasedAuthProperties_certificateAuthorityArn' - The Amazon Resource Name (ARN) of the Amazon Web Services Certificate
-- Manager Private CA resource.
--
-- 'status', 'certificateBasedAuthProperties_status' - The status of the certificate-based authentication properties.
newCertificateBasedAuthProperties ::
  CertificateBasedAuthProperties
newCertificateBasedAuthProperties =
  CertificateBasedAuthProperties'
    { certificateAuthorityArn =
        Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the Amazon Web Services Certificate
-- Manager Private CA resource.
certificateBasedAuthProperties_certificateAuthorityArn :: Lens.Lens' CertificateBasedAuthProperties (Prelude.Maybe Prelude.Text)
certificateBasedAuthProperties_certificateAuthorityArn = Lens.lens (\CertificateBasedAuthProperties' {certificateAuthorityArn} -> certificateAuthorityArn) (\s@CertificateBasedAuthProperties' {} a -> s {certificateAuthorityArn = a} :: CertificateBasedAuthProperties)

-- | The status of the certificate-based authentication properties.
certificateBasedAuthProperties_status :: Lens.Lens' CertificateBasedAuthProperties (Prelude.Maybe CertificateBasedAuthStatusEnum)
certificateBasedAuthProperties_status = Lens.lens (\CertificateBasedAuthProperties' {status} -> status) (\s@CertificateBasedAuthProperties' {} a -> s {status = a} :: CertificateBasedAuthProperties)

instance Data.FromJSON CertificateBasedAuthProperties where
  parseJSON =
    Data.withObject
      "CertificateBasedAuthProperties"
      ( \x ->
          CertificateBasedAuthProperties'
            Prelude.<$> (x Data..:? "CertificateAuthorityArn")
            Prelude.<*> (x Data..:? "Status")
      )

instance
  Prelude.Hashable
    CertificateBasedAuthProperties
  where
  hashWithSalt
    _salt
    CertificateBasedAuthProperties' {..} =
      _salt
        `Prelude.hashWithSalt` certificateAuthorityArn
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    CertificateBasedAuthProperties
  where
  rnf CertificateBasedAuthProperties' {..} =
    Prelude.rnf certificateAuthorityArn
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON CertificateBasedAuthProperties where
  toJSON CertificateBasedAuthProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CertificateAuthorityArn" Data..=)
              Prelude.<$> certificateAuthorityArn,
            ("Status" Data..=) Prelude.<$> status
          ]
      )
