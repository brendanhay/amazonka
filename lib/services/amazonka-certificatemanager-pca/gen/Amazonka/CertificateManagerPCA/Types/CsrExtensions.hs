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
-- Module      : Amazonka.CertificateManagerPCA.Types.CsrExtensions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CertificateManagerPCA.Types.CsrExtensions where

import Amazonka.CertificateManagerPCA.Types.AccessDescription
import Amazonka.CertificateManagerPCA.Types.KeyUsage
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the certificate extensions to be added to the certificate
-- signing request (CSR).
--
-- /See:/ 'newCsrExtensions' smart constructor.
data CsrExtensions = CsrExtensions'
  { -- | Indicates the purpose of the certificate and of the key contained in the
    -- certificate.
    keyUsage :: Prelude.Maybe KeyUsage,
    -- | For CA certificates, provides a path to additional information
    -- pertaining to the CA, such as revocation and policy. For more
    -- information, see
    -- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.2.2.2 Subject Information Access>
    -- in RFC 5280.
    subjectInformationAccess :: Prelude.Maybe [AccessDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CsrExtensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyUsage', 'csrExtensions_keyUsage' - Indicates the purpose of the certificate and of the key contained in the
-- certificate.
--
-- 'subjectInformationAccess', 'csrExtensions_subjectInformationAccess' - For CA certificates, provides a path to additional information
-- pertaining to the CA, such as revocation and policy. For more
-- information, see
-- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.2.2.2 Subject Information Access>
-- in RFC 5280.
newCsrExtensions ::
  CsrExtensions
newCsrExtensions =
  CsrExtensions'
    { keyUsage = Prelude.Nothing,
      subjectInformationAccess = Prelude.Nothing
    }

-- | Indicates the purpose of the certificate and of the key contained in the
-- certificate.
csrExtensions_keyUsage :: Lens.Lens' CsrExtensions (Prelude.Maybe KeyUsage)
csrExtensions_keyUsage = Lens.lens (\CsrExtensions' {keyUsage} -> keyUsage) (\s@CsrExtensions' {} a -> s {keyUsage = a} :: CsrExtensions)

-- | For CA certificates, provides a path to additional information
-- pertaining to the CA, such as revocation and policy. For more
-- information, see
-- <https://datatracker.ietf.org/doc/html/rfc5280#section-4.2.2.2 Subject Information Access>
-- in RFC 5280.
csrExtensions_subjectInformationAccess :: Lens.Lens' CsrExtensions (Prelude.Maybe [AccessDescription])
csrExtensions_subjectInformationAccess = Lens.lens (\CsrExtensions' {subjectInformationAccess} -> subjectInformationAccess) (\s@CsrExtensions' {} a -> s {subjectInformationAccess = a} :: CsrExtensions) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON CsrExtensions where
  parseJSON =
    Data.withObject
      "CsrExtensions"
      ( \x ->
          CsrExtensions'
            Prelude.<$> (x Data..:? "KeyUsage")
            Prelude.<*> ( x
                            Data..:? "SubjectInformationAccess"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable CsrExtensions where
  hashWithSalt _salt CsrExtensions' {..} =
    _salt
      `Prelude.hashWithSalt` keyUsage
      `Prelude.hashWithSalt` subjectInformationAccess

instance Prelude.NFData CsrExtensions where
  rnf CsrExtensions' {..} =
    Prelude.rnf keyUsage
      `Prelude.seq` Prelude.rnf subjectInformationAccess

instance Data.ToJSON CsrExtensions where
  toJSON CsrExtensions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KeyUsage" Data..=) Prelude.<$> keyUsage,
            ("SubjectInformationAccess" Data..=)
              Prelude.<$> subjectInformationAccess
          ]
      )
