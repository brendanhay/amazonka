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
-- Module      : Network.AWS.CertificateManagerPCA.Types.CsrExtensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CsrExtensions where

import Network.AWS.CertificateManagerPCA.Types.AccessDescription
import Network.AWS.CertificateManagerPCA.Types.KeyUsage
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the certificate extensions to be added to the certificate
-- signing request (CSR).
--
-- /See:/ 'newCsrExtensions' smart constructor.
data CsrExtensions = CsrExtensions'
  { -- | For CA certificates, provides a path to additional information
    -- pertaining to the CA, such as revocation and policy. For more
    -- information, see
    -- <https://tools.ietf.org/html/rfc5280#section-4.2.2.2 Subject Information Access>
    -- in RFC 5280.
    subjectInformationAccess :: Core.Maybe [AccessDescription],
    -- | Indicates the purpose of the certificate and of the key contained in the
    -- certificate.
    keyUsage :: Core.Maybe KeyUsage
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CsrExtensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectInformationAccess', 'csrExtensions_subjectInformationAccess' - For CA certificates, provides a path to additional information
-- pertaining to the CA, such as revocation and policy. For more
-- information, see
-- <https://tools.ietf.org/html/rfc5280#section-4.2.2.2 Subject Information Access>
-- in RFC 5280.
--
-- 'keyUsage', 'csrExtensions_keyUsage' - Indicates the purpose of the certificate and of the key contained in the
-- certificate.
newCsrExtensions ::
  CsrExtensions
newCsrExtensions =
  CsrExtensions'
    { subjectInformationAccess =
        Core.Nothing,
      keyUsage = Core.Nothing
    }

-- | For CA certificates, provides a path to additional information
-- pertaining to the CA, such as revocation and policy. For more
-- information, see
-- <https://tools.ietf.org/html/rfc5280#section-4.2.2.2 Subject Information Access>
-- in RFC 5280.
csrExtensions_subjectInformationAccess :: Lens.Lens' CsrExtensions (Core.Maybe [AccessDescription])
csrExtensions_subjectInformationAccess = Lens.lens (\CsrExtensions' {subjectInformationAccess} -> subjectInformationAccess) (\s@CsrExtensions' {} a -> s {subjectInformationAccess = a} :: CsrExtensions) Core.. Lens.mapping Lens._Coerce

-- | Indicates the purpose of the certificate and of the key contained in the
-- certificate.
csrExtensions_keyUsage :: Lens.Lens' CsrExtensions (Core.Maybe KeyUsage)
csrExtensions_keyUsage = Lens.lens (\CsrExtensions' {keyUsage} -> keyUsage) (\s@CsrExtensions' {} a -> s {keyUsage = a} :: CsrExtensions)

instance Core.FromJSON CsrExtensions where
  parseJSON =
    Core.withObject
      "CsrExtensions"
      ( \x ->
          CsrExtensions'
            Core.<$> ( x Core..:? "SubjectInformationAccess"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "KeyUsage")
      )

instance Core.Hashable CsrExtensions

instance Core.NFData CsrExtensions

instance Core.ToJSON CsrExtensions where
  toJSON CsrExtensions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SubjectInformationAccess" Core..=)
              Core.<$> subjectInformationAccess,
            ("KeyUsage" Core..=) Core.<$> keyUsage
          ]
      )
