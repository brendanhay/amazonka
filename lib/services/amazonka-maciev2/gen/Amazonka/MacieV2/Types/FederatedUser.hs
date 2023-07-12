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
-- Module      : Amazonka.MacieV2.Types.FederatedUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.FederatedUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SessionContext
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an identity that performed an action on an
-- affected resource by using temporary security credentials. The
-- credentials were obtained using the GetFederationToken operation of the
-- Security Token Service (STS) API.
--
-- /See:/ 'newFederatedUser' smart constructor.
data FederatedUser = FederatedUser'
  { -- | The Amazon Web Services access key ID that identifies the credentials.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Web Services account that owns the
    -- entity that was used to get the credentials.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the entity that was used to get the
    -- credentials.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the entity that was used to get the
    -- credentials.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The details of the session that was created for the credentials,
    -- including the entity that issued the session.
    sessionContext :: Prelude.Maybe SessionContext
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FederatedUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'federatedUser_accessKeyId' - The Amazon Web Services access key ID that identifies the credentials.
--
-- 'accountId', 'federatedUser_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- entity that was used to get the credentials.
--
-- 'arn', 'federatedUser_arn' - The Amazon Resource Name (ARN) of the entity that was used to get the
-- credentials.
--
-- 'principalId', 'federatedUser_principalId' - The unique identifier for the entity that was used to get the
-- credentials.
--
-- 'sessionContext', 'federatedUser_sessionContext' - The details of the session that was created for the credentials,
-- including the entity that issued the session.
newFederatedUser ::
  FederatedUser
newFederatedUser =
  FederatedUser'
    { accessKeyId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      arn = Prelude.Nothing,
      principalId = Prelude.Nothing,
      sessionContext = Prelude.Nothing
    }

-- | The Amazon Web Services access key ID that identifies the credentials.
federatedUser_accessKeyId :: Lens.Lens' FederatedUser (Prelude.Maybe Prelude.Text)
federatedUser_accessKeyId = Lens.lens (\FederatedUser' {accessKeyId} -> accessKeyId) (\s@FederatedUser' {} a -> s {accessKeyId = a} :: FederatedUser)

-- | The unique identifier for the Amazon Web Services account that owns the
-- entity that was used to get the credentials.
federatedUser_accountId :: Lens.Lens' FederatedUser (Prelude.Maybe Prelude.Text)
federatedUser_accountId = Lens.lens (\FederatedUser' {accountId} -> accountId) (\s@FederatedUser' {} a -> s {accountId = a} :: FederatedUser)

-- | The Amazon Resource Name (ARN) of the entity that was used to get the
-- credentials.
federatedUser_arn :: Lens.Lens' FederatedUser (Prelude.Maybe Prelude.Text)
federatedUser_arn = Lens.lens (\FederatedUser' {arn} -> arn) (\s@FederatedUser' {} a -> s {arn = a} :: FederatedUser)

-- | The unique identifier for the entity that was used to get the
-- credentials.
federatedUser_principalId :: Lens.Lens' FederatedUser (Prelude.Maybe Prelude.Text)
federatedUser_principalId = Lens.lens (\FederatedUser' {principalId} -> principalId) (\s@FederatedUser' {} a -> s {principalId = a} :: FederatedUser)

-- | The details of the session that was created for the credentials,
-- including the entity that issued the session.
federatedUser_sessionContext :: Lens.Lens' FederatedUser (Prelude.Maybe SessionContext)
federatedUser_sessionContext = Lens.lens (\FederatedUser' {sessionContext} -> sessionContext) (\s@FederatedUser' {} a -> s {sessionContext = a} :: FederatedUser)

instance Data.FromJSON FederatedUser where
  parseJSON =
    Data.withObject
      "FederatedUser"
      ( \x ->
          FederatedUser'
            Prelude.<$> (x Data..:? "accessKeyId")
            Prelude.<*> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "principalId")
            Prelude.<*> (x Data..:? "sessionContext")
      )

instance Prelude.Hashable FederatedUser where
  hashWithSalt _salt FederatedUser' {..} =
    _salt
      `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` sessionContext

instance Prelude.NFData FederatedUser where
  rnf FederatedUser' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf sessionContext
