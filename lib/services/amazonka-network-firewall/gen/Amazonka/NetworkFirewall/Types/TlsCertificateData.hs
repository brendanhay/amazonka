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
-- Module      : Amazonka.NetworkFirewall.Types.TlsCertificateData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.NetworkFirewall.Types.TlsCertificateData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains metadata about an Certificate Manager certificate.
--
-- /See:/ 'newTlsCertificateData' smart constructor.
data TlsCertificateData = TlsCertificateData'
  { -- | The Amazon Resource Name (ARN) of the certificate.
    certificateArn :: Prelude.Maybe Prelude.Text,
    -- | The serial number of the certificate.
    certificateSerial :: Prelude.Maybe Prelude.Text,
    -- | The status of the certificate.
    status :: Prelude.Maybe Prelude.Text,
    -- | Contains details about the certificate status, including information
    -- about certificate errors.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TlsCertificateData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'certificateArn', 'tlsCertificateData_certificateArn' - The Amazon Resource Name (ARN) of the certificate.
--
-- 'certificateSerial', 'tlsCertificateData_certificateSerial' - The serial number of the certificate.
--
-- 'status', 'tlsCertificateData_status' - The status of the certificate.
--
-- 'statusMessage', 'tlsCertificateData_statusMessage' - Contains details about the certificate status, including information
-- about certificate errors.
newTlsCertificateData ::
  TlsCertificateData
newTlsCertificateData =
  TlsCertificateData'
    { certificateArn =
        Prelude.Nothing,
      certificateSerial = Prelude.Nothing,
      status = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the certificate.
tlsCertificateData_certificateArn :: Lens.Lens' TlsCertificateData (Prelude.Maybe Prelude.Text)
tlsCertificateData_certificateArn = Lens.lens (\TlsCertificateData' {certificateArn} -> certificateArn) (\s@TlsCertificateData' {} a -> s {certificateArn = a} :: TlsCertificateData)

-- | The serial number of the certificate.
tlsCertificateData_certificateSerial :: Lens.Lens' TlsCertificateData (Prelude.Maybe Prelude.Text)
tlsCertificateData_certificateSerial = Lens.lens (\TlsCertificateData' {certificateSerial} -> certificateSerial) (\s@TlsCertificateData' {} a -> s {certificateSerial = a} :: TlsCertificateData)

-- | The status of the certificate.
tlsCertificateData_status :: Lens.Lens' TlsCertificateData (Prelude.Maybe Prelude.Text)
tlsCertificateData_status = Lens.lens (\TlsCertificateData' {status} -> status) (\s@TlsCertificateData' {} a -> s {status = a} :: TlsCertificateData)

-- | Contains details about the certificate status, including information
-- about certificate errors.
tlsCertificateData_statusMessage :: Lens.Lens' TlsCertificateData (Prelude.Maybe Prelude.Text)
tlsCertificateData_statusMessage = Lens.lens (\TlsCertificateData' {statusMessage} -> statusMessage) (\s@TlsCertificateData' {} a -> s {statusMessage = a} :: TlsCertificateData)

instance Data.FromJSON TlsCertificateData where
  parseJSON =
    Data.withObject
      "TlsCertificateData"
      ( \x ->
          TlsCertificateData'
            Prelude.<$> (x Data..:? "CertificateArn")
            Prelude.<*> (x Data..:? "CertificateSerial")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "StatusMessage")
      )

instance Prelude.Hashable TlsCertificateData where
  hashWithSalt _salt TlsCertificateData' {..} =
    _salt
      `Prelude.hashWithSalt` certificateArn
      `Prelude.hashWithSalt` certificateSerial
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData TlsCertificateData where
  rnf TlsCertificateData' {..} =
    Prelude.rnf certificateArn
      `Prelude.seq` Prelude.rnf certificateSerial
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
