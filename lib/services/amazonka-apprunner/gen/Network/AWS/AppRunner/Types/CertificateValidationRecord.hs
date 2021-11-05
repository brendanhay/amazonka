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
-- Module      : Amazonka.AppRunner.Types.CertificateValidationRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CertificateValidationRecord where

import Amazonka.AppRunner.Types.CertificateValidationRecordStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes a certificate CNAME record to add to your DNS. For more
-- information, see
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_AssociateCustomDomain.html AssociateCustomDomain>.
--
-- /See:/ 'newCertificateValidationRecord' smart constructor.
data CertificateValidationRecord = CertificateValidationRecord'
  { -- | The current state of the certificate CNAME record validation. It should
    -- change to @SUCCESS@ after App Runner completes validation with your DNS.
    status :: Prelude.Maybe CertificateValidationRecordStatus,
    -- | The certificate CNAME record value.
    value :: Prelude.Maybe Prelude.Text,
    -- | The certificate CNAME record name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The record type, always @CNAME@.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateValidationRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'certificateValidationRecord_status' - The current state of the certificate CNAME record validation. It should
-- change to @SUCCESS@ after App Runner completes validation with your DNS.
--
-- 'value', 'certificateValidationRecord_value' - The certificate CNAME record value.
--
-- 'name', 'certificateValidationRecord_name' - The certificate CNAME record name.
--
-- 'type'', 'certificateValidationRecord_type' - The record type, always @CNAME@.
newCertificateValidationRecord ::
  CertificateValidationRecord
newCertificateValidationRecord =
  CertificateValidationRecord'
    { status =
        Prelude.Nothing,
      value = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The current state of the certificate CNAME record validation. It should
-- change to @SUCCESS@ after App Runner completes validation with your DNS.
certificateValidationRecord_status :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe CertificateValidationRecordStatus)
certificateValidationRecord_status = Lens.lens (\CertificateValidationRecord' {status} -> status) (\s@CertificateValidationRecord' {} a -> s {status = a} :: CertificateValidationRecord)

-- | The certificate CNAME record value.
certificateValidationRecord_value :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe Prelude.Text)
certificateValidationRecord_value = Lens.lens (\CertificateValidationRecord' {value} -> value) (\s@CertificateValidationRecord' {} a -> s {value = a} :: CertificateValidationRecord)

-- | The certificate CNAME record name.
certificateValidationRecord_name :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe Prelude.Text)
certificateValidationRecord_name = Lens.lens (\CertificateValidationRecord' {name} -> name) (\s@CertificateValidationRecord' {} a -> s {name = a} :: CertificateValidationRecord)

-- | The record type, always @CNAME@.
certificateValidationRecord_type :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe Prelude.Text)
certificateValidationRecord_type = Lens.lens (\CertificateValidationRecord' {type'} -> type') (\s@CertificateValidationRecord' {} a -> s {type' = a} :: CertificateValidationRecord)

instance Core.FromJSON CertificateValidationRecord where
  parseJSON =
    Core.withObject
      "CertificateValidationRecord"
      ( \x ->
          CertificateValidationRecord'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "Value")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Type")
      )

instance Prelude.Hashable CertificateValidationRecord

instance Prelude.NFData CertificateValidationRecord
