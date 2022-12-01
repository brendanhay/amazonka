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
-- Module      : Amazonka.SESV2.Types.IdentityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.IdentityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.IdentityType
import Amazonka.SESV2.Types.VerificationStatus

-- | Information about an email identity.
--
-- /See:/ 'newIdentityInfo' smart constructor.
data IdentityInfo = IdentityInfo'
  { -- | Indicates whether or not you can send email from the identity.
    --
    -- An /identity/ is an email address or domain that you send email from.
    -- Before you can send email from an identity, you have to demostrate that
    -- you own the identity, and that you authorize Amazon SES to send email
    -- from that identity.
    sendingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The address or domain of the identity.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The email identity type. Note: the @MANAGED_DOMAIN@ type is not
    -- supported for email identity types.
    identityType :: Prelude.Maybe IdentityType,
    -- | The verification status of the identity. The status can be one of the
    -- following:
    --
    -- -   @PENDING@ – The verification process was initiated, but Amazon SES
    --     hasn\'t yet been able to verify the identity.
    --
    -- -   @SUCCESS@ – The verification process completed successfully.
    --
    -- -   @FAILED@ – The verification process failed.
    --
    -- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
    --     from determining the verification status of the identity.
    --
    -- -   @NOT_STARTED@ – The verification process hasn\'t been initiated for
    --     the identity.
    verificationStatus :: Prelude.Maybe VerificationStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IdentityInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sendingEnabled', 'identityInfo_sendingEnabled' - Indicates whether or not you can send email from the identity.
--
-- An /identity/ is an email address or domain that you send email from.
-- Before you can send email from an identity, you have to demostrate that
-- you own the identity, and that you authorize Amazon SES to send email
-- from that identity.
--
-- 'identityName', 'identityInfo_identityName' - The address or domain of the identity.
--
-- 'identityType', 'identityInfo_identityType' - The email identity type. Note: the @MANAGED_DOMAIN@ type is not
-- supported for email identity types.
--
-- 'verificationStatus', 'identityInfo_verificationStatus' - The verification status of the identity. The status can be one of the
-- following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet been able to verify the identity.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the verification status of the identity.
--
-- -   @NOT_STARTED@ – The verification process hasn\'t been initiated for
--     the identity.
newIdentityInfo ::
  IdentityInfo
newIdentityInfo =
  IdentityInfo'
    { sendingEnabled = Prelude.Nothing,
      identityName = Prelude.Nothing,
      identityType = Prelude.Nothing,
      verificationStatus = Prelude.Nothing
    }

-- | Indicates whether or not you can send email from the identity.
--
-- An /identity/ is an email address or domain that you send email from.
-- Before you can send email from an identity, you have to demostrate that
-- you own the identity, and that you authorize Amazon SES to send email
-- from that identity.
identityInfo_sendingEnabled :: Lens.Lens' IdentityInfo (Prelude.Maybe Prelude.Bool)
identityInfo_sendingEnabled = Lens.lens (\IdentityInfo' {sendingEnabled} -> sendingEnabled) (\s@IdentityInfo' {} a -> s {sendingEnabled = a} :: IdentityInfo)

-- | The address or domain of the identity.
identityInfo_identityName :: Lens.Lens' IdentityInfo (Prelude.Maybe Prelude.Text)
identityInfo_identityName = Lens.lens (\IdentityInfo' {identityName} -> identityName) (\s@IdentityInfo' {} a -> s {identityName = a} :: IdentityInfo)

-- | The email identity type. Note: the @MANAGED_DOMAIN@ type is not
-- supported for email identity types.
identityInfo_identityType :: Lens.Lens' IdentityInfo (Prelude.Maybe IdentityType)
identityInfo_identityType = Lens.lens (\IdentityInfo' {identityType} -> identityType) (\s@IdentityInfo' {} a -> s {identityType = a} :: IdentityInfo)

-- | The verification status of the identity. The status can be one of the
-- following:
--
-- -   @PENDING@ – The verification process was initiated, but Amazon SES
--     hasn\'t yet been able to verify the identity.
--
-- -   @SUCCESS@ – The verification process completed successfully.
--
-- -   @FAILED@ – The verification process failed.
--
-- -   @TEMPORARY_FAILURE@ – A temporary issue is preventing Amazon SES
--     from determining the verification status of the identity.
--
-- -   @NOT_STARTED@ – The verification process hasn\'t been initiated for
--     the identity.
identityInfo_verificationStatus :: Lens.Lens' IdentityInfo (Prelude.Maybe VerificationStatus)
identityInfo_verificationStatus = Lens.lens (\IdentityInfo' {verificationStatus} -> verificationStatus) (\s@IdentityInfo' {} a -> s {verificationStatus = a} :: IdentityInfo)

instance Core.FromJSON IdentityInfo where
  parseJSON =
    Core.withObject
      "IdentityInfo"
      ( \x ->
          IdentityInfo'
            Prelude.<$> (x Core..:? "SendingEnabled")
            Prelude.<*> (x Core..:? "IdentityName")
            Prelude.<*> (x Core..:? "IdentityType")
            Prelude.<*> (x Core..:? "VerificationStatus")
      )

instance Prelude.Hashable IdentityInfo where
  hashWithSalt _salt IdentityInfo' {..} =
    _salt `Prelude.hashWithSalt` sendingEnabled
      `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` identityType
      `Prelude.hashWithSalt` verificationStatus

instance Prelude.NFData IdentityInfo where
  rnf IdentityInfo' {..} =
    Prelude.rnf sendingEnabled
      `Prelude.seq` Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf identityType
      `Prelude.seq` Prelude.rnf verificationStatus
