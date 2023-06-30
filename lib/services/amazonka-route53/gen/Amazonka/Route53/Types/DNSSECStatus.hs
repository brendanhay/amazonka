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
-- Module      : Amazonka.Route53.Types.DNSSECStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.DNSSECStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal

-- | A string repesenting the status of DNSSEC signing.
--
-- /See:/ 'newDNSSECStatus' smart constructor.
data DNSSECStatus = DNSSECStatus'
  { -- | A string that represents the current hosted zone signing status.
    --
    -- Status can have one of the following values:
    --
    -- [SIGNING]
    --     DNSSEC signing is enabled for the hosted zone.
    --
    -- [NOT_SIGNING]
    --     DNSSEC signing is not enabled for the hosted zone.
    --
    -- [DELETING]
    --     DNSSEC signing is in the process of being removed for the hosted
    --     zone.
    --
    -- [ACTION_NEEDED]
    --     There is a problem with signing in the hosted zone that requires you
    --     to take action to resolve. For example, the customer managed key
    --     might have been deleted, or the permissions for the customer managed
    --     key might have been changed.
    --
    -- [INTERNAL_FAILURE]
    --     There was an error during a request. Before you can continue to work
    --     with DNSSEC signing, including with key-signing keys (KSKs), you
    --     must correct the problem by enabling or disabling DNSSEC signing for
    --     the hosted zone.
    serveSignature :: Prelude.Maybe Prelude.Text,
    -- | The status message provided for the following DNSSEC signing status:
    -- @INTERNAL_FAILURE@. The status message includes information about what
    -- the problem might be and steps that you can take to correct the issue.
    statusMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DNSSECStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serveSignature', 'dNSSECStatus_serveSignature' - A string that represents the current hosted zone signing status.
--
-- Status can have one of the following values:
--
-- [SIGNING]
--     DNSSEC signing is enabled for the hosted zone.
--
-- [NOT_SIGNING]
--     DNSSEC signing is not enabled for the hosted zone.
--
-- [DELETING]
--     DNSSEC signing is in the process of being removed for the hosted
--     zone.
--
-- [ACTION_NEEDED]
--     There is a problem with signing in the hosted zone that requires you
--     to take action to resolve. For example, the customer managed key
--     might have been deleted, or the permissions for the customer managed
--     key might have been changed.
--
-- [INTERNAL_FAILURE]
--     There was an error during a request. Before you can continue to work
--     with DNSSEC signing, including with key-signing keys (KSKs), you
--     must correct the problem by enabling or disabling DNSSEC signing for
--     the hosted zone.
--
-- 'statusMessage', 'dNSSECStatus_statusMessage' - The status message provided for the following DNSSEC signing status:
-- @INTERNAL_FAILURE@. The status message includes information about what
-- the problem might be and steps that you can take to correct the issue.
newDNSSECStatus ::
  DNSSECStatus
newDNSSECStatus =
  DNSSECStatus'
    { serveSignature = Prelude.Nothing,
      statusMessage = Prelude.Nothing
    }

-- | A string that represents the current hosted zone signing status.
--
-- Status can have one of the following values:
--
-- [SIGNING]
--     DNSSEC signing is enabled for the hosted zone.
--
-- [NOT_SIGNING]
--     DNSSEC signing is not enabled for the hosted zone.
--
-- [DELETING]
--     DNSSEC signing is in the process of being removed for the hosted
--     zone.
--
-- [ACTION_NEEDED]
--     There is a problem with signing in the hosted zone that requires you
--     to take action to resolve. For example, the customer managed key
--     might have been deleted, or the permissions for the customer managed
--     key might have been changed.
--
-- [INTERNAL_FAILURE]
--     There was an error during a request. Before you can continue to work
--     with DNSSEC signing, including with key-signing keys (KSKs), you
--     must correct the problem by enabling or disabling DNSSEC signing for
--     the hosted zone.
dNSSECStatus_serveSignature :: Lens.Lens' DNSSECStatus (Prelude.Maybe Prelude.Text)
dNSSECStatus_serveSignature = Lens.lens (\DNSSECStatus' {serveSignature} -> serveSignature) (\s@DNSSECStatus' {} a -> s {serveSignature = a} :: DNSSECStatus)

-- | The status message provided for the following DNSSEC signing status:
-- @INTERNAL_FAILURE@. The status message includes information about what
-- the problem might be and steps that you can take to correct the issue.
dNSSECStatus_statusMessage :: Lens.Lens' DNSSECStatus (Prelude.Maybe Prelude.Text)
dNSSECStatus_statusMessage = Lens.lens (\DNSSECStatus' {statusMessage} -> statusMessage) (\s@DNSSECStatus' {} a -> s {statusMessage = a} :: DNSSECStatus)

instance Data.FromXML DNSSECStatus where
  parseXML x =
    DNSSECStatus'
      Prelude.<$> (x Data..@? "ServeSignature")
      Prelude.<*> (x Data..@? "StatusMessage")

instance Prelude.Hashable DNSSECStatus where
  hashWithSalt _salt DNSSECStatus' {..} =
    _salt
      `Prelude.hashWithSalt` serveSignature
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData DNSSECStatus where
  rnf DNSSECStatus' {..} =
    Prelude.rnf serveSignature
      `Prelude.seq` Prelude.rnf statusMessage
