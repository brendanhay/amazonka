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
-- Module      : Network.AWS.EC2.Types.ClientCertificateRevocationListStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ClientCertificateRevocationListStatus where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.ClientCertificateRevocationListStatusCode
import qualified Network.AWS.Lens as Lens

-- | Describes the state of a client certificate revocation list.
--
-- /See:/ 'newClientCertificateRevocationListStatus' smart constructor.
data ClientCertificateRevocationListStatus = ClientCertificateRevocationListStatus'
  { -- | A message about the status of the client certificate revocation list, if
    -- applicable.
    message :: Core.Maybe Core.Text,
    -- | The state of the client certificate revocation list.
    code :: Core.Maybe ClientCertificateRevocationListStatusCode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClientCertificateRevocationListStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'clientCertificateRevocationListStatus_message' - A message about the status of the client certificate revocation list, if
-- applicable.
--
-- 'code', 'clientCertificateRevocationListStatus_code' - The state of the client certificate revocation list.
newClientCertificateRevocationListStatus ::
  ClientCertificateRevocationListStatus
newClientCertificateRevocationListStatus =
  ClientCertificateRevocationListStatus'
    { message =
        Core.Nothing,
      code = Core.Nothing
    }

-- | A message about the status of the client certificate revocation list, if
-- applicable.
clientCertificateRevocationListStatus_message :: Lens.Lens' ClientCertificateRevocationListStatus (Core.Maybe Core.Text)
clientCertificateRevocationListStatus_message = Lens.lens (\ClientCertificateRevocationListStatus' {message} -> message) (\s@ClientCertificateRevocationListStatus' {} a -> s {message = a} :: ClientCertificateRevocationListStatus)

-- | The state of the client certificate revocation list.
clientCertificateRevocationListStatus_code :: Lens.Lens' ClientCertificateRevocationListStatus (Core.Maybe ClientCertificateRevocationListStatusCode)
clientCertificateRevocationListStatus_code = Lens.lens (\ClientCertificateRevocationListStatus' {code} -> code) (\s@ClientCertificateRevocationListStatus' {} a -> s {code = a} :: ClientCertificateRevocationListStatus)

instance
  Core.FromXML
    ClientCertificateRevocationListStatus
  where
  parseXML x =
    ClientCertificateRevocationListStatus'
      Core.<$> (x Core..@? "message") Core.<*> (x Core..@? "code")

instance
  Core.Hashable
    ClientCertificateRevocationListStatus

instance
  Core.NFData
    ClientCertificateRevocationListStatus
