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
-- Module      : Amazonka.Redshift.Types.CertificateAssociation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.CertificateAssociation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | A cluster ID and custom domain name tied to a specific certificate.
-- These are typically returned in a list.
--
-- /See:/ 'newCertificateAssociation' smart constructor.
data CertificateAssociation = CertificateAssociation'
  { -- | The cluster identifier for the certificate association.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The custom domain name for the certificate association.
    customDomainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'certificateAssociation_clusterIdentifier' - The cluster identifier for the certificate association.
--
-- 'customDomainName', 'certificateAssociation_customDomainName' - The custom domain name for the certificate association.
newCertificateAssociation ::
  CertificateAssociation
newCertificateAssociation =
  CertificateAssociation'
    { clusterIdentifier =
        Prelude.Nothing,
      customDomainName = Prelude.Nothing
    }

-- | The cluster identifier for the certificate association.
certificateAssociation_clusterIdentifier :: Lens.Lens' CertificateAssociation (Prelude.Maybe Prelude.Text)
certificateAssociation_clusterIdentifier = Lens.lens (\CertificateAssociation' {clusterIdentifier} -> clusterIdentifier) (\s@CertificateAssociation' {} a -> s {clusterIdentifier = a} :: CertificateAssociation)

-- | The custom domain name for the certificate association.
certificateAssociation_customDomainName :: Lens.Lens' CertificateAssociation (Prelude.Maybe Prelude.Text)
certificateAssociation_customDomainName = Lens.lens (\CertificateAssociation' {customDomainName} -> customDomainName) (\s@CertificateAssociation' {} a -> s {customDomainName = a} :: CertificateAssociation)

instance Data.FromXML CertificateAssociation where
  parseXML x =
    CertificateAssociation'
      Prelude.<$> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "CustomDomainName")

instance Prelude.Hashable CertificateAssociation where
  hashWithSalt _salt CertificateAssociation' {..} =
    _salt
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` customDomainName

instance Prelude.NFData CertificateAssociation where
  rnf CertificateAssociation' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf customDomainName
