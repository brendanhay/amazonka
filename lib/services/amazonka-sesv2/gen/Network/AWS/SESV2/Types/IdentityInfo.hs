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
-- Module      : Network.AWS.SESV2.Types.IdentityInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESV2.Types.IdentityInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SESV2.Types.IdentityType

-- | Information about an email identity.
--
-- /See:/ 'newIdentityInfo' smart constructor.
data IdentityInfo = IdentityInfo'
  { -- | The email identity type. Note: the @MANAGED_DOMAIN@ type is not
    -- supported for email identity types.
    identityType :: Prelude.Maybe IdentityType,
    -- | The address or domain of the identity.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether or not you can send email from the identity.
    --
    -- An /identity/ is an email address or domain that you send email from.
    -- Before you can send email from an identity, you have to demostrate that
    -- you own the identity, and that you authorize Amazon SES to send email
    -- from that identity.
    sendingEnabled :: Prelude.Maybe Prelude.Bool
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
-- 'identityType', 'identityInfo_identityType' - The email identity type. Note: the @MANAGED_DOMAIN@ type is not
-- supported for email identity types.
--
-- 'identityName', 'identityInfo_identityName' - The address or domain of the identity.
--
-- 'sendingEnabled', 'identityInfo_sendingEnabled' - Indicates whether or not you can send email from the identity.
--
-- An /identity/ is an email address or domain that you send email from.
-- Before you can send email from an identity, you have to demostrate that
-- you own the identity, and that you authorize Amazon SES to send email
-- from that identity.
newIdentityInfo ::
  IdentityInfo
newIdentityInfo =
  IdentityInfo'
    { identityType = Prelude.Nothing,
      identityName = Prelude.Nothing,
      sendingEnabled = Prelude.Nothing
    }

-- | The email identity type. Note: the @MANAGED_DOMAIN@ type is not
-- supported for email identity types.
identityInfo_identityType :: Lens.Lens' IdentityInfo (Prelude.Maybe IdentityType)
identityInfo_identityType = Lens.lens (\IdentityInfo' {identityType} -> identityType) (\s@IdentityInfo' {} a -> s {identityType = a} :: IdentityInfo)

-- | The address or domain of the identity.
identityInfo_identityName :: Lens.Lens' IdentityInfo (Prelude.Maybe Prelude.Text)
identityInfo_identityName = Lens.lens (\IdentityInfo' {identityName} -> identityName) (\s@IdentityInfo' {} a -> s {identityName = a} :: IdentityInfo)

-- | Indicates whether or not you can send email from the identity.
--
-- An /identity/ is an email address or domain that you send email from.
-- Before you can send email from an identity, you have to demostrate that
-- you own the identity, and that you authorize Amazon SES to send email
-- from that identity.
identityInfo_sendingEnabled :: Lens.Lens' IdentityInfo (Prelude.Maybe Prelude.Bool)
identityInfo_sendingEnabled = Lens.lens (\IdentityInfo' {sendingEnabled} -> sendingEnabled) (\s@IdentityInfo' {} a -> s {sendingEnabled = a} :: IdentityInfo)

instance Core.FromJSON IdentityInfo where
  parseJSON =
    Core.withObject
      "IdentityInfo"
      ( \x ->
          IdentityInfo'
            Prelude.<$> (x Core..:? "IdentityType")
            Prelude.<*> (x Core..:? "IdentityName")
            Prelude.<*> (x Core..:? "SendingEnabled")
      )

instance Prelude.Hashable IdentityInfo

instance Prelude.NFData IdentityInfo
