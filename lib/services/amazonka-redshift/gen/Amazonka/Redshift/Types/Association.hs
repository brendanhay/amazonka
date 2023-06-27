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
-- Module      : Amazonka.Redshift.Types.Association
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.Association where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.CertificateAssociation

-- | Contains information about the custom domain name association.
--
-- /See:/ 'newAssociation' smart constructor.
data Association = Association'
  { -- | A list of all associated clusters and domain names tied to a specific
    -- certificate.
    certificateAssociations :: Prelude.Maybe [CertificateAssociation],
    -- | The Amazon Resource Name (ARN) for the certificate associated with the
    -- custom domain.
    customDomainCertificateArn :: Prelude.Maybe Prelude.Text,
    -- | The expiration date for the certificate.
    customDomainCertificateExpiryDate :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Association' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateAssociations', 'association_certificateAssociations' - A list of all associated clusters and domain names tied to a specific
-- certificate.
--
-- 'customDomainCertificateArn', 'association_customDomainCertificateArn' - The Amazon Resource Name (ARN) for the certificate associated with the
-- custom domain.
--
-- 'customDomainCertificateExpiryDate', 'association_customDomainCertificateExpiryDate' - The expiration date for the certificate.
newAssociation ::
  Association
newAssociation =
  Association'
    { certificateAssociations =
        Prelude.Nothing,
      customDomainCertificateArn = Prelude.Nothing,
      customDomainCertificateExpiryDate = Prelude.Nothing
    }

-- | A list of all associated clusters and domain names tied to a specific
-- certificate.
association_certificateAssociations :: Lens.Lens' Association (Prelude.Maybe [CertificateAssociation])
association_certificateAssociations = Lens.lens (\Association' {certificateAssociations} -> certificateAssociations) (\s@Association' {} a -> s {certificateAssociations = a} :: Association) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) for the certificate associated with the
-- custom domain.
association_customDomainCertificateArn :: Lens.Lens' Association (Prelude.Maybe Prelude.Text)
association_customDomainCertificateArn = Lens.lens (\Association' {customDomainCertificateArn} -> customDomainCertificateArn) (\s@Association' {} a -> s {customDomainCertificateArn = a} :: Association)

-- | The expiration date for the certificate.
association_customDomainCertificateExpiryDate :: Lens.Lens' Association (Prelude.Maybe Prelude.UTCTime)
association_customDomainCertificateExpiryDate = Lens.lens (\Association' {customDomainCertificateExpiryDate} -> customDomainCertificateExpiryDate) (\s@Association' {} a -> s {customDomainCertificateExpiryDate = a} :: Association) Prelude.. Lens.mapping Data._Time

instance Data.FromXML Association where
  parseXML x =
    Association'
      Prelude.<$> ( x
                      Data..@? "CertificateAssociations"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may
                        (Data.parseXMLList "CertificateAssociation")
                  )
      Prelude.<*> (x Data..@? "CustomDomainCertificateArn")
      Prelude.<*> (x Data..@? "CustomDomainCertificateExpiryDate")

instance Prelude.Hashable Association where
  hashWithSalt _salt Association' {..} =
    _salt
      `Prelude.hashWithSalt` certificateAssociations
      `Prelude.hashWithSalt` customDomainCertificateArn
      `Prelude.hashWithSalt` customDomainCertificateExpiryDate

instance Prelude.NFData Association where
  rnf Association' {..} =
    Prelude.rnf certificateAssociations
      `Prelude.seq` Prelude.rnf customDomainCertificateArn
      `Prelude.seq` Prelude.rnf customDomainCertificateExpiryDate
