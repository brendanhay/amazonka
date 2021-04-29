{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CertificateManager.Types.CertificateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManager.Types.CertificateSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This structure is returned in the response object of ListCertificates
-- action.
--
-- /See:/ 'newCertificateSummary' smart constructor.
data CertificateSummary = CertificateSummary'
  { -- | Amazon Resource Name (ARN) of the certificate. This is of the form:
    --
    -- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | Fully qualified domain name (FQDN), such as www.example.com or
    -- example.com, for the certificate.
    domainName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CertificateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'certificateSummary_certificateArn' - Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
--
-- 'domainName', 'certificateSummary_domainName' - Fully qualified domain name (FQDN), such as www.example.com or
-- example.com, for the certificate.
newCertificateSummary ::
  CertificateSummary
newCertificateSummary =
  CertificateSummary'
    { certificateArn =
        Prelude.Nothing,
      domainName = Prelude.Nothing
    }

-- | Amazon Resource Name (ARN) of the certificate. This is of the form:
--
-- @arn:aws:acm:region:123456789012:certificate\/12345678-1234-1234-1234-123456789012@
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>.
certificateSummary_certificateArn :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_certificateArn = Lens.lens (\CertificateSummary' {certificateArn} -> certificateArn) (\s@CertificateSummary' {} a -> s {certificateArn = a} :: CertificateSummary)

-- | Fully qualified domain name (FQDN), such as www.example.com or
-- example.com, for the certificate.
certificateSummary_domainName :: Lens.Lens' CertificateSummary (Prelude.Maybe Prelude.Text)
certificateSummary_domainName = Lens.lens (\CertificateSummary' {domainName} -> domainName) (\s@CertificateSummary' {} a -> s {domainName = a} :: CertificateSummary)

instance Prelude.FromJSON CertificateSummary where
  parseJSON =
    Prelude.withObject
      "CertificateSummary"
      ( \x ->
          CertificateSummary'
            Prelude.<$> (x Prelude..:? "CertificateArn")
            Prelude.<*> (x Prelude..:? "DomainName")
      )

instance Prelude.Hashable CertificateSummary

instance Prelude.NFData CertificateSummary
