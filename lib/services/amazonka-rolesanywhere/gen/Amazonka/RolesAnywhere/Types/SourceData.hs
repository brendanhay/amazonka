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
-- Module      : Amazonka.RolesAnywhere.Types.SourceData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RolesAnywhere.Types.SourceData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The data field of the trust anchor depending on its type.
--
-- /See:/ 'newSourceData' smart constructor.
data SourceData = SourceData'
  { -- | The root certificate of the Certificate Manager Private Certificate
    -- Authority specified by this ARN is used in trust validation for
    -- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
    -- operations. Included for trust anchors of type @AWS_ACM_PCA@.
    acmPcaArn :: Prelude.Maybe Prelude.Text,
    -- | The PEM-encoded data for the certificate anchor. Included for trust
    -- anchors of type @CERTIFICATE_BUNDLE@.
    x509CertificateData :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SourceData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acmPcaArn', 'sourceData_acmPcaArn' - The root certificate of the Certificate Manager Private Certificate
-- Authority specified by this ARN is used in trust validation for
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operations. Included for trust anchors of type @AWS_ACM_PCA@.
--
-- 'x509CertificateData', 'sourceData_x509CertificateData' - The PEM-encoded data for the certificate anchor. Included for trust
-- anchors of type @CERTIFICATE_BUNDLE@.
newSourceData ::
  SourceData
newSourceData =
  SourceData'
    { acmPcaArn = Prelude.Nothing,
      x509CertificateData = Prelude.Nothing
    }

-- | The root certificate of the Certificate Manager Private Certificate
-- Authority specified by this ARN is used in trust validation for
-- <https://docs.aws.amazon.com/rolesanywhere/latest/APIReference/API_CreateSession.html CreateSession>
-- operations. Included for trust anchors of type @AWS_ACM_PCA@.
sourceData_acmPcaArn :: Lens.Lens' SourceData (Prelude.Maybe Prelude.Text)
sourceData_acmPcaArn = Lens.lens (\SourceData' {acmPcaArn} -> acmPcaArn) (\s@SourceData' {} a -> s {acmPcaArn = a} :: SourceData)

-- | The PEM-encoded data for the certificate anchor. Included for trust
-- anchors of type @CERTIFICATE_BUNDLE@.
sourceData_x509CertificateData :: Lens.Lens' SourceData (Prelude.Maybe Prelude.Text)
sourceData_x509CertificateData = Lens.lens (\SourceData' {x509CertificateData} -> x509CertificateData) (\s@SourceData' {} a -> s {x509CertificateData = a} :: SourceData)

instance Core.FromJSON SourceData where
  parseJSON =
    Core.withObject
      "SourceData"
      ( \x ->
          SourceData'
            Prelude.<$> (x Core..:? "acmPcaArn")
            Prelude.<*> (x Core..:? "x509CertificateData")
      )

instance Prelude.Hashable SourceData where
  hashWithSalt _salt SourceData' {..} =
    _salt `Prelude.hashWithSalt` acmPcaArn
      `Prelude.hashWithSalt` x509CertificateData

instance Prelude.NFData SourceData where
  rnf SourceData' {..} =
    Prelude.rnf acmPcaArn
      `Prelude.seq` Prelude.rnf x509CertificateData

instance Core.ToJSON SourceData where
  toJSON SourceData' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("acmPcaArn" Core..=) Prelude.<$> acmPcaArn,
            ("x509CertificateData" Core..=)
              Prelude.<$> x509CertificateData
          ]
      )
