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
-- Module      : Amazonka.EC2.Types.ClientCertificateRevocationListStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.ClientCertificateRevocationListStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.ClientCertificateRevocationListStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Describes the state of a client certificate revocation list.
--
-- /See:/ 'newClientCertificateRevocationListStatus' smart constructor.
data ClientCertificateRevocationListStatus = ClientCertificateRevocationListStatus'
  { -- | The state of the client certificate revocation list.
    code :: Prelude.Maybe ClientCertificateRevocationListStatusCode,
    -- | A message about the status of the client certificate revocation list, if
    -- applicable.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClientCertificateRevocationListStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'clientCertificateRevocationListStatus_code' - The state of the client certificate revocation list.
--
-- 'message', 'clientCertificateRevocationListStatus_message' - A message about the status of the client certificate revocation list, if
-- applicable.
newClientCertificateRevocationListStatus ::
  ClientCertificateRevocationListStatus
newClientCertificateRevocationListStatus =
  ClientCertificateRevocationListStatus'
    { code =
        Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The state of the client certificate revocation list.
clientCertificateRevocationListStatus_code :: Lens.Lens' ClientCertificateRevocationListStatus (Prelude.Maybe ClientCertificateRevocationListStatusCode)
clientCertificateRevocationListStatus_code = Lens.lens (\ClientCertificateRevocationListStatus' {code} -> code) (\s@ClientCertificateRevocationListStatus' {} a -> s {code = a} :: ClientCertificateRevocationListStatus)

-- | A message about the status of the client certificate revocation list, if
-- applicable.
clientCertificateRevocationListStatus_message :: Lens.Lens' ClientCertificateRevocationListStatus (Prelude.Maybe Prelude.Text)
clientCertificateRevocationListStatus_message = Lens.lens (\ClientCertificateRevocationListStatus' {message} -> message) (\s@ClientCertificateRevocationListStatus' {} a -> s {message = a} :: ClientCertificateRevocationListStatus)

instance
  Data.FromXML
    ClientCertificateRevocationListStatus
  where
  parseXML x =
    ClientCertificateRevocationListStatus'
      Prelude.<$> (x Data..@? "code")
      Prelude.<*> (x Data..@? "message")

instance
  Prelude.Hashable
    ClientCertificateRevocationListStatus
  where
  hashWithSalt
    _salt
    ClientCertificateRevocationListStatus' {..} =
      _salt
        `Prelude.hashWithSalt` code
        `Prelude.hashWithSalt` message

instance
  Prelude.NFData
    ClientCertificateRevocationListStatus
  where
  rnf ClientCertificateRevocationListStatus' {..} =
    Prelude.rnf code `Prelude.seq` Prelude.rnf message
