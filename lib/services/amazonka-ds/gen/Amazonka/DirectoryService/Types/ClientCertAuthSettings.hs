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
-- Module      : Amazonka.DirectoryService.Types.ClientCertAuthSettings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DirectoryService.Types.ClientCertAuthSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the client certificate authentication
-- settings for the @RegisterCertificate@ and @DescribeCertificate@
-- operations.
--
-- /See:/ 'newClientCertAuthSettings' smart constructor.
data ClientCertAuthSettings = ClientCertAuthSettings'
  { -- | Specifies the URL of the default OCSP server used to check for
    -- revocation status. A secondary value to any OCSP address found in the
    -- AIA extension of the user certificate.
    oCSPUrl :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientCertAuthSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oCSPUrl', 'clientCertAuthSettings_oCSPUrl' - Specifies the URL of the default OCSP server used to check for
-- revocation status. A secondary value to any OCSP address found in the
-- AIA extension of the user certificate.
newClientCertAuthSettings ::
  ClientCertAuthSettings
newClientCertAuthSettings =
  ClientCertAuthSettings' {oCSPUrl = Prelude.Nothing}

-- | Specifies the URL of the default OCSP server used to check for
-- revocation status. A secondary value to any OCSP address found in the
-- AIA extension of the user certificate.
clientCertAuthSettings_oCSPUrl :: Lens.Lens' ClientCertAuthSettings (Prelude.Maybe Prelude.Text)
clientCertAuthSettings_oCSPUrl = Lens.lens (\ClientCertAuthSettings' {oCSPUrl} -> oCSPUrl) (\s@ClientCertAuthSettings' {} a -> s {oCSPUrl = a} :: ClientCertAuthSettings)

instance Core.FromJSON ClientCertAuthSettings where
  parseJSON =
    Core.withObject
      "ClientCertAuthSettings"
      ( \x ->
          ClientCertAuthSettings'
            Prelude.<$> (x Core..:? "OCSPUrl")
      )

instance Prelude.Hashable ClientCertAuthSettings where
  hashWithSalt _salt ClientCertAuthSettings' {..} =
    _salt `Prelude.hashWithSalt` oCSPUrl

instance Prelude.NFData ClientCertAuthSettings where
  rnf ClientCertAuthSettings' {..} = Prelude.rnf oCSPUrl

instance Core.ToJSON ClientCertAuthSettings where
  toJSON ClientCertAuthSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [("OCSPUrl" Core..=) Prelude.<$> oCSPUrl]
      )
