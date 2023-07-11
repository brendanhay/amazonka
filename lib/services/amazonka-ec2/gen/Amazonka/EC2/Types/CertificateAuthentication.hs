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
-- Module      : Amazonka.EC2.Types.CertificateAuthentication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CertificateAuthentication where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Information about the client certificate used for authentication.
--
-- /See:/ 'newCertificateAuthentication' smart constructor.
data CertificateAuthentication = CertificateAuthentication'
  { -- | The ARN of the client certificate.
    clientRootCertificateChain :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CertificateAuthentication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientRootCertificateChain', 'certificateAuthentication_clientRootCertificateChain' - The ARN of the client certificate.
newCertificateAuthentication ::
  CertificateAuthentication
newCertificateAuthentication =
  CertificateAuthentication'
    { clientRootCertificateChain =
        Prelude.Nothing
    }

-- | The ARN of the client certificate.
certificateAuthentication_clientRootCertificateChain :: Lens.Lens' CertificateAuthentication (Prelude.Maybe Prelude.Text)
certificateAuthentication_clientRootCertificateChain = Lens.lens (\CertificateAuthentication' {clientRootCertificateChain} -> clientRootCertificateChain) (\s@CertificateAuthentication' {} a -> s {clientRootCertificateChain = a} :: CertificateAuthentication)

instance Data.FromXML CertificateAuthentication where
  parseXML x =
    CertificateAuthentication'
      Prelude.<$> (x Data..@? "clientRootCertificateChain")

instance Prelude.Hashable CertificateAuthentication where
  hashWithSalt _salt CertificateAuthentication' {..} =
    _salt
      `Prelude.hashWithSalt` clientRootCertificateChain

instance Prelude.NFData CertificateAuthentication where
  rnf CertificateAuthentication' {..} =
    Prelude.rnf clientRootCertificateChain
