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
-- Module      : Amazonka.RDS.Types.CertificateDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RDS.Types.CertificateDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns the details of the DB instance’s server certificate.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB instance>
-- in the /Amazon RDS User Guide/ and
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/UsingWithRDS.SSL.html Using SSL\/TLS to encrypt a connection to a DB cluster>
-- in the /Amazon Aurora User Guide/.
--
-- /See:/ 'newCertificateDetails' smart constructor.
data CertificateDetails = CertificateDetails'
  { -- | The CA identifier of the CA certificate used for the DB instance\'s
    -- server certificate.
    cAIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The expiration date of the DB instance’s server certificate.
    validTill :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cAIdentifier', 'certificateDetails_cAIdentifier' - The CA identifier of the CA certificate used for the DB instance\'s
-- server certificate.
--
-- 'validTill', 'certificateDetails_validTill' - The expiration date of the DB instance’s server certificate.
newCertificateDetails ::
  CertificateDetails
newCertificateDetails =
  CertificateDetails'
    { cAIdentifier = Prelude.Nothing,
      validTill = Prelude.Nothing
    }

-- | The CA identifier of the CA certificate used for the DB instance\'s
-- server certificate.
certificateDetails_cAIdentifier :: Lens.Lens' CertificateDetails (Prelude.Maybe Prelude.Text)
certificateDetails_cAIdentifier = Lens.lens (\CertificateDetails' {cAIdentifier} -> cAIdentifier) (\s@CertificateDetails' {} a -> s {cAIdentifier = a} :: CertificateDetails)

-- | The expiration date of the DB instance’s server certificate.
certificateDetails_validTill :: Lens.Lens' CertificateDetails (Prelude.Maybe Prelude.UTCTime)
certificateDetails_validTill = Lens.lens (\CertificateDetails' {validTill} -> validTill) (\s@CertificateDetails' {} a -> s {validTill = a} :: CertificateDetails) Prelude.. Lens.mapping Data._Time

instance Data.FromXML CertificateDetails where
  parseXML x =
    CertificateDetails'
      Prelude.<$> (x Data..@? "CAIdentifier")
      Prelude.<*> (x Data..@? "ValidTill")

instance Prelude.Hashable CertificateDetails where
  hashWithSalt _salt CertificateDetails' {..} =
    _salt
      `Prelude.hashWithSalt` cAIdentifier
      `Prelude.hashWithSalt` validTill

instance Prelude.NFData CertificateDetails where
  rnf CertificateDetails' {..} =
    Prelude.rnf cAIdentifier
      `Prelude.seq` Prelude.rnf validTill
