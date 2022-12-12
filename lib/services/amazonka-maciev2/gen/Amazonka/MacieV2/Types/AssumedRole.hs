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
-- Module      : Amazonka.MacieV2.Types.AssumedRole
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.AssumedRole where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.SessionContext
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an identity that performed an action on an
-- affected resource by using temporary security credentials. The
-- credentials were obtained using the AssumeRole operation of the Security
-- Token Service (STS) API.
--
-- /See:/ 'newAssumedRole' smart constructor.
data AssumedRole = AssumedRole'
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
-- Create a value of 'AssumedRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'assumedRole_accessKeyId' - The Amazon Web Services access key ID that identifies the credentials.
--
-- 'accountId', 'assumedRole_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- entity that was used to get the credentials.
--
-- 'arn', 'assumedRole_arn' - The Amazon Resource Name (ARN) of the entity that was used to get the
-- credentials.
--
-- 'principalId', 'assumedRole_principalId' - The unique identifier for the entity that was used to get the
-- credentials.
--
-- 'sessionContext', 'assumedRole_sessionContext' - The details of the session that was created for the credentials,
-- including the entity that issued the session.
newAssumedRole ::
  AssumedRole
newAssumedRole =
  AssumedRole'
    { accessKeyId = Prelude.Nothing,
      accountId = Prelude.Nothing,
      arn = Prelude.Nothing,
      principalId = Prelude.Nothing,
      sessionContext = Prelude.Nothing
    }

-- | The Amazon Web Services access key ID that identifies the credentials.
assumedRole_accessKeyId :: Lens.Lens' AssumedRole (Prelude.Maybe Prelude.Text)
assumedRole_accessKeyId = Lens.lens (\AssumedRole' {accessKeyId} -> accessKeyId) (\s@AssumedRole' {} a -> s {accessKeyId = a} :: AssumedRole)

-- | The unique identifier for the Amazon Web Services account that owns the
-- entity that was used to get the credentials.
assumedRole_accountId :: Lens.Lens' AssumedRole (Prelude.Maybe Prelude.Text)
assumedRole_accountId = Lens.lens (\AssumedRole' {accountId} -> accountId) (\s@AssumedRole' {} a -> s {accountId = a} :: AssumedRole)

-- | The Amazon Resource Name (ARN) of the entity that was used to get the
-- credentials.
assumedRole_arn :: Lens.Lens' AssumedRole (Prelude.Maybe Prelude.Text)
assumedRole_arn = Lens.lens (\AssumedRole' {arn} -> arn) (\s@AssumedRole' {} a -> s {arn = a} :: AssumedRole)

-- | The unique identifier for the entity that was used to get the
-- credentials.
assumedRole_principalId :: Lens.Lens' AssumedRole (Prelude.Maybe Prelude.Text)
assumedRole_principalId = Lens.lens (\AssumedRole' {principalId} -> principalId) (\s@AssumedRole' {} a -> s {principalId = a} :: AssumedRole)

-- | The details of the session that was created for the credentials,
-- including the entity that issued the session.
assumedRole_sessionContext :: Lens.Lens' AssumedRole (Prelude.Maybe SessionContext)
assumedRole_sessionContext = Lens.lens (\AssumedRole' {sessionContext} -> sessionContext) (\s@AssumedRole' {} a -> s {sessionContext = a} :: AssumedRole)

instance Data.FromJSON AssumedRole where
  parseJSON =
    Data.withObject
      "AssumedRole"
      ( \x ->
          AssumedRole'
            Prelude.<$> (x Data..:? "accessKeyId")
            Prelude.<*> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "principalId")
            Prelude.<*> (x Data..:? "sessionContext")
      )

instance Prelude.Hashable AssumedRole where
  hashWithSalt _salt AssumedRole' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` sessionContext

instance Prelude.NFData AssumedRole where
  rnf AssumedRole' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf sessionContext
