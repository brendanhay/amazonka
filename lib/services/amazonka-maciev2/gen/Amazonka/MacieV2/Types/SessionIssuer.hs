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
-- Module      : Amazonka.MacieV2.Types.SessionIssuer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.SessionIssuer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the source and type of temporary security
-- credentials that were issued to an entity.
--
-- /See:/ 'newSessionIssuer' smart constructor.
data SessionIssuer = SessionIssuer'
  { -- | The unique identifier for the Amazon Web Services account that owns the
    -- entity that was used to get the credentials.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the source account, IAM user, or role
    -- that was used to get the credentials.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the entity that was used to get the
    -- credentials.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The source of the temporary security credentials, such as Root, IAMUser,
    -- or Role.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The name or alias of the user or role that issued the session. This
    -- value is null if the credentials were obtained from a root account that
    -- doesn\'t have an alias.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SessionIssuer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'sessionIssuer_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- entity that was used to get the credentials.
--
-- 'arn', 'sessionIssuer_arn' - The Amazon Resource Name (ARN) of the source account, IAM user, or role
-- that was used to get the credentials.
--
-- 'principalId', 'sessionIssuer_principalId' - The unique identifier for the entity that was used to get the
-- credentials.
--
-- 'type'', 'sessionIssuer_type' - The source of the temporary security credentials, such as Root, IAMUser,
-- or Role.
--
-- 'userName', 'sessionIssuer_userName' - The name or alias of the user or role that issued the session. This
-- value is null if the credentials were obtained from a root account that
-- doesn\'t have an alias.
newSessionIssuer ::
  SessionIssuer
newSessionIssuer =
  SessionIssuer'
    { accountId = Prelude.Nothing,
      arn = Prelude.Nothing,
      principalId = Prelude.Nothing,
      type' = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The unique identifier for the Amazon Web Services account that owns the
-- entity that was used to get the credentials.
sessionIssuer_accountId :: Lens.Lens' SessionIssuer (Prelude.Maybe Prelude.Text)
sessionIssuer_accountId = Lens.lens (\SessionIssuer' {accountId} -> accountId) (\s@SessionIssuer' {} a -> s {accountId = a} :: SessionIssuer)

-- | The Amazon Resource Name (ARN) of the source account, IAM user, or role
-- that was used to get the credentials.
sessionIssuer_arn :: Lens.Lens' SessionIssuer (Prelude.Maybe Prelude.Text)
sessionIssuer_arn = Lens.lens (\SessionIssuer' {arn} -> arn) (\s@SessionIssuer' {} a -> s {arn = a} :: SessionIssuer)

-- | The unique identifier for the entity that was used to get the
-- credentials.
sessionIssuer_principalId :: Lens.Lens' SessionIssuer (Prelude.Maybe Prelude.Text)
sessionIssuer_principalId = Lens.lens (\SessionIssuer' {principalId} -> principalId) (\s@SessionIssuer' {} a -> s {principalId = a} :: SessionIssuer)

-- | The source of the temporary security credentials, such as Root, IAMUser,
-- or Role.
sessionIssuer_type :: Lens.Lens' SessionIssuer (Prelude.Maybe Prelude.Text)
sessionIssuer_type = Lens.lens (\SessionIssuer' {type'} -> type') (\s@SessionIssuer' {} a -> s {type' = a} :: SessionIssuer)

-- | The name or alias of the user or role that issued the session. This
-- value is null if the credentials were obtained from a root account that
-- doesn\'t have an alias.
sessionIssuer_userName :: Lens.Lens' SessionIssuer (Prelude.Maybe Prelude.Text)
sessionIssuer_userName = Lens.lens (\SessionIssuer' {userName} -> userName) (\s@SessionIssuer' {} a -> s {userName = a} :: SessionIssuer)

instance Data.FromJSON SessionIssuer where
  parseJSON =
    Data.withObject
      "SessionIssuer"
      ( \x ->
          SessionIssuer'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "principalId")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "userName")
      )

instance Prelude.Hashable SessionIssuer where
  hashWithSalt _salt SessionIssuer' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` userName

instance Prelude.NFData SessionIssuer where
  rnf SessionIssuer' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf userName
