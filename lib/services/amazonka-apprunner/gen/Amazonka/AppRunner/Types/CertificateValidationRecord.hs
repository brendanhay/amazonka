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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppRunner.Types.CertificateValidationRecord where

import Amazonka.AppRunner.Types.CertificateValidationRecordStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a certificate CNAME record to add to your DNS. For more
-- information, see
-- <https://docs.aws.amazon.com/apprunner/latest/api/API_AssociateCustomDomain.html AssociateCustomDomain>.
--
-- /See:/ 'newCertificateValidationRecord' smart constructor.
data CertificateValidationRecord = CertificateValidationRecord'
  { -- | The certificate CNAME record name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The current state of the certificate CNAME record validation. It should
    -- change to @SUCCESS@ after App Runner completes validation with your DNS.
    status :: Prelude.Maybe CertificateValidationRecordStatus,
    -- | The record type, always @CNAME@.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The certificate CNAME record value.
    value :: Prelude.Maybe Prelude.Text
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
-- 'name', 'certificateValidationRecord_name' - The certificate CNAME record name.
--
-- 'status', 'certificateValidationRecord_status' - The current state of the certificate CNAME record validation. It should
-- change to @SUCCESS@ after App Runner completes validation with your DNS.
--
-- 'type'', 'certificateValidationRecord_type' - The record type, always @CNAME@.
--
-- 'value', 'certificateValidationRecord_value' - The certificate CNAME record value.
newCertificateValidationRecord ::
  CertificateValidationRecord
newCertificateValidationRecord =
  CertificateValidationRecord'
    { name =
        Prelude.Nothing,
      status = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The certificate CNAME record name.
certificateValidationRecord_name :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe Prelude.Text)
certificateValidationRecord_name = Lens.lens (\CertificateValidationRecord' {name} -> name) (\s@CertificateValidationRecord' {} a -> s {name = a} :: CertificateValidationRecord)

-- | The current state of the certificate CNAME record validation. It should
-- change to @SUCCESS@ after App Runner completes validation with your DNS.
certificateValidationRecord_status :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe CertificateValidationRecordStatus)
certificateValidationRecord_status = Lens.lens (\CertificateValidationRecord' {status} -> status) (\s@CertificateValidationRecord' {} a -> s {status = a} :: CertificateValidationRecord)

-- | The record type, always @CNAME@.
certificateValidationRecord_type :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe Prelude.Text)
certificateValidationRecord_type = Lens.lens (\CertificateValidationRecord' {type'} -> type') (\s@CertificateValidationRecord' {} a -> s {type' = a} :: CertificateValidationRecord)

-- | The certificate CNAME record value.
certificateValidationRecord_value :: Lens.Lens' CertificateValidationRecord (Prelude.Maybe Prelude.Text)
certificateValidationRecord_value = Lens.lens (\CertificateValidationRecord' {value} -> value) (\s@CertificateValidationRecord' {} a -> s {value = a} :: CertificateValidationRecord)

instance Data.FromJSON CertificateValidationRecord where
  parseJSON =
    Data.withObject
      "CertificateValidationRecord"
      ( \x ->
          CertificateValidationRecord'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Type")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable CertificateValidationRecord where
  hashWithSalt _salt CertificateValidationRecord' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData CertificateValidationRecord where
  rnf CertificateValidationRecord' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
