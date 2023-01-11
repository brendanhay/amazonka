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
-- Module      : Amazonka.SES.Types.IdentityVerificationAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.IdentityVerificationAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.VerificationStatus

-- | Represents the verification attributes of a single identity.
--
-- /See:/ 'newIdentityVerificationAttributes' smart constructor.
data IdentityVerificationAttributes = IdentityVerificationAttributes'
  { -- | The verification token for a domain identity. Null for email address
    -- identities.
    verificationToken :: Prelude.Maybe Prelude.Text,
    -- | The verification status of the identity: \"Pending\", \"Success\",
    -- \"Failed\", or \"TemporaryFailure\".
    verificationStatus :: VerificationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityVerificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'verificationToken', 'identityVerificationAttributes_verificationToken' - The verification token for a domain identity. Null for email address
-- identities.
--
-- 'verificationStatus', 'identityVerificationAttributes_verificationStatus' - The verification status of the identity: \"Pending\", \"Success\",
-- \"Failed\", or \"TemporaryFailure\".
newIdentityVerificationAttributes ::
  -- | 'verificationStatus'
  VerificationStatus ->
  IdentityVerificationAttributes
newIdentityVerificationAttributes
  pVerificationStatus_ =
    IdentityVerificationAttributes'
      { verificationToken =
          Prelude.Nothing,
        verificationStatus = pVerificationStatus_
      }

-- | The verification token for a domain identity. Null for email address
-- identities.
identityVerificationAttributes_verificationToken :: Lens.Lens' IdentityVerificationAttributes (Prelude.Maybe Prelude.Text)
identityVerificationAttributes_verificationToken = Lens.lens (\IdentityVerificationAttributes' {verificationToken} -> verificationToken) (\s@IdentityVerificationAttributes' {} a -> s {verificationToken = a} :: IdentityVerificationAttributes)

-- | The verification status of the identity: \"Pending\", \"Success\",
-- \"Failed\", or \"TemporaryFailure\".
identityVerificationAttributes_verificationStatus :: Lens.Lens' IdentityVerificationAttributes VerificationStatus
identityVerificationAttributes_verificationStatus = Lens.lens (\IdentityVerificationAttributes' {verificationStatus} -> verificationStatus) (\s@IdentityVerificationAttributes' {} a -> s {verificationStatus = a} :: IdentityVerificationAttributes)

instance Data.FromXML IdentityVerificationAttributes where
  parseXML x =
    IdentityVerificationAttributes'
      Prelude.<$> (x Data..@? "VerificationToken")
      Prelude.<*> (x Data..@ "VerificationStatus")

instance
  Prelude.Hashable
    IdentityVerificationAttributes
  where
  hashWithSalt
    _salt
    IdentityVerificationAttributes' {..} =
      _salt `Prelude.hashWithSalt` verificationToken
        `Prelude.hashWithSalt` verificationStatus

instance
  Prelude.NFData
    IdentityVerificationAttributes
  where
  rnf IdentityVerificationAttributes' {..} =
    Prelude.rnf verificationToken
      `Prelude.seq` Prelude.rnf verificationStatus
