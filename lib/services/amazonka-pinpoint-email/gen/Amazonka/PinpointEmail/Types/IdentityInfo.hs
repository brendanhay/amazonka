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
-- Module      : Amazonka.PinpointEmail.Types.IdentityInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointEmail.Types.IdentityInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointEmail.Types.IdentityType
import qualified Amazonka.Prelude as Prelude

-- | Information about an email identity.
--
-- /See:/ 'newIdentityInfo' smart constructor.
data IdentityInfo = IdentityInfo'
  { -- | Indicates whether or not you can send email from the identity.
    --
    -- In Amazon Pinpoint, an identity is an email address or domain that you
    -- send email from. Before you can send email from an identity, you have to
    -- demostrate that you own the identity, and that you authorize Amazon
    -- Pinpoint to send email from that identity.
    sendingEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The address or domain of the identity.
    identityName :: Prelude.Maybe Prelude.Text,
    -- | The email identity type. The identity type can be one of the following:
    --
    -- -   @EMAIL_ADDRESS@ – The identity is an email address.
    --
    -- -   @DOMAIN@ – The identity is a domain.
    --
    -- -   @MANAGED_DOMAIN@ – The identity is a domain that is managed by AWS.
    identityType :: Prelude.Maybe IdentityType
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
-- In Amazon Pinpoint, an identity is an email address or domain that you
-- send email from. Before you can send email from an identity, you have to
-- demostrate that you own the identity, and that you authorize Amazon
-- Pinpoint to send email from that identity.
--
-- 'identityName', 'identityInfo_identityName' - The address or domain of the identity.
--
-- 'identityType', 'identityInfo_identityType' - The email identity type. The identity type can be one of the following:
--
-- -   @EMAIL_ADDRESS@ – The identity is an email address.
--
-- -   @DOMAIN@ – The identity is a domain.
--
-- -   @MANAGED_DOMAIN@ – The identity is a domain that is managed by AWS.
newIdentityInfo ::
  IdentityInfo
newIdentityInfo =
  IdentityInfo'
    { sendingEnabled = Prelude.Nothing,
      identityName = Prelude.Nothing,
      identityType = Prelude.Nothing
    }

-- | Indicates whether or not you can send email from the identity.
--
-- In Amazon Pinpoint, an identity is an email address or domain that you
-- send email from. Before you can send email from an identity, you have to
-- demostrate that you own the identity, and that you authorize Amazon
-- Pinpoint to send email from that identity.
identityInfo_sendingEnabled :: Lens.Lens' IdentityInfo (Prelude.Maybe Prelude.Bool)
identityInfo_sendingEnabled = Lens.lens (\IdentityInfo' {sendingEnabled} -> sendingEnabled) (\s@IdentityInfo' {} a -> s {sendingEnabled = a} :: IdentityInfo)

-- | The address or domain of the identity.
identityInfo_identityName :: Lens.Lens' IdentityInfo (Prelude.Maybe Prelude.Text)
identityInfo_identityName = Lens.lens (\IdentityInfo' {identityName} -> identityName) (\s@IdentityInfo' {} a -> s {identityName = a} :: IdentityInfo)

-- | The email identity type. The identity type can be one of the following:
--
-- -   @EMAIL_ADDRESS@ – The identity is an email address.
--
-- -   @DOMAIN@ – The identity is a domain.
--
-- -   @MANAGED_DOMAIN@ – The identity is a domain that is managed by AWS.
identityInfo_identityType :: Lens.Lens' IdentityInfo (Prelude.Maybe IdentityType)
identityInfo_identityType = Lens.lens (\IdentityInfo' {identityType} -> identityType) (\s@IdentityInfo' {} a -> s {identityType = a} :: IdentityInfo)

instance Data.FromJSON IdentityInfo where
  parseJSON =
    Data.withObject
      "IdentityInfo"
      ( \x ->
          IdentityInfo'
            Prelude.<$> (x Data..:? "SendingEnabled")
            Prelude.<*> (x Data..:? "IdentityName")
            Prelude.<*> (x Data..:? "IdentityType")
      )

instance Prelude.Hashable IdentityInfo where
  hashWithSalt _salt IdentityInfo' {..} =
    _salt `Prelude.hashWithSalt` sendingEnabled
      `Prelude.hashWithSalt` identityName
      `Prelude.hashWithSalt` identityType

instance Prelude.NFData IdentityInfo where
  rnf IdentityInfo' {..} =
    Prelude.rnf sendingEnabled
      `Prelude.seq` Prelude.rnf identityName
      `Prelude.seq` Prelude.rnf identityType
