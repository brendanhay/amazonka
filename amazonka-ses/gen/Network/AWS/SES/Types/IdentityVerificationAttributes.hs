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
-- Module      : Network.AWS.SES.Types.IdentityVerificationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.IdentityVerificationAttributes where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SES.Types.VerificationStatus

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromXML
    IdentityVerificationAttributes
  where
  parseXML x =
    IdentityVerificationAttributes'
      Prelude.<$> (x Prelude..@? "VerificationToken")
      Prelude.<*> (x Prelude..@ "VerificationStatus")

instance
  Prelude.Hashable
    IdentityVerificationAttributes

instance
  Prelude.NFData
    IdentityVerificationAttributes
