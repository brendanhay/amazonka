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
-- Module      : Amazonka.Organizations.Types.DelegatedAdministrator
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.DelegatedAdministrator where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.AccountJoinedMethod
import Amazonka.Organizations.Types.AccountStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the delegated administrator.
--
-- /See:/ 'newDelegatedAdministrator' smart constructor.
data DelegatedAdministrator = DelegatedAdministrator'
  { -- | The Amazon Resource Name (ARN) of the delegated administrator\'s
    -- account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the account was made a delegated administrator.
    delegationEnabledDate :: Prelude.Maybe Data.POSIX,
    -- | The email address that is associated with the delegated administrator\'s
    -- Amazon Web Services account.
    email :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier (ID) of the delegated administrator\'s account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The method by which the delegated administrator\'s account joined the
    -- organization.
    joinedMethod :: Prelude.Maybe AccountJoinedMethod,
    -- | The date when the delegated administrator\'s account became a part of
    -- the organization.
    joinedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The friendly name of the delegated administrator\'s account.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The status of the delegated administrator\'s account in the
    -- organization.
    status :: Prelude.Maybe AccountStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DelegatedAdministrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'delegatedAdministrator_arn' - The Amazon Resource Name (ARN) of the delegated administrator\'s
-- account.
--
-- 'delegationEnabledDate', 'delegatedAdministrator_delegationEnabledDate' - The date when the account was made a delegated administrator.
--
-- 'email', 'delegatedAdministrator_email' - The email address that is associated with the delegated administrator\'s
-- Amazon Web Services account.
--
-- 'id', 'delegatedAdministrator_id' - The unique identifier (ID) of the delegated administrator\'s account.
--
-- 'joinedMethod', 'delegatedAdministrator_joinedMethod' - The method by which the delegated administrator\'s account joined the
-- organization.
--
-- 'joinedTimestamp', 'delegatedAdministrator_joinedTimestamp' - The date when the delegated administrator\'s account became a part of
-- the organization.
--
-- 'name', 'delegatedAdministrator_name' - The friendly name of the delegated administrator\'s account.
--
-- 'status', 'delegatedAdministrator_status' - The status of the delegated administrator\'s account in the
-- organization.
newDelegatedAdministrator ::
  DelegatedAdministrator
newDelegatedAdministrator =
  DelegatedAdministrator'
    { arn = Prelude.Nothing,
      delegationEnabledDate = Prelude.Nothing,
      email = Prelude.Nothing,
      id = Prelude.Nothing,
      joinedMethod = Prelude.Nothing,
      joinedTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the delegated administrator\'s
-- account.
delegatedAdministrator_arn :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_arn = Lens.lens (\DelegatedAdministrator' {arn} -> arn) (\s@DelegatedAdministrator' {} a -> s {arn = a} :: DelegatedAdministrator)

-- | The date when the account was made a delegated administrator.
delegatedAdministrator_delegationEnabledDate :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.UTCTime)
delegatedAdministrator_delegationEnabledDate = Lens.lens (\DelegatedAdministrator' {delegationEnabledDate} -> delegationEnabledDate) (\s@DelegatedAdministrator' {} a -> s {delegationEnabledDate = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Time

-- | The email address that is associated with the delegated administrator\'s
-- Amazon Web Services account.
delegatedAdministrator_email :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_email = Lens.lens (\DelegatedAdministrator' {email} -> email) (\s@DelegatedAdministrator' {} a -> s {email = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier (ID) of the delegated administrator\'s account.
delegatedAdministrator_id :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_id = Lens.lens (\DelegatedAdministrator' {id} -> id) (\s@DelegatedAdministrator' {} a -> s {id = a} :: DelegatedAdministrator)

-- | The method by which the delegated administrator\'s account joined the
-- organization.
delegatedAdministrator_joinedMethod :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe AccountJoinedMethod)
delegatedAdministrator_joinedMethod = Lens.lens (\DelegatedAdministrator' {joinedMethod} -> joinedMethod) (\s@DelegatedAdministrator' {} a -> s {joinedMethod = a} :: DelegatedAdministrator)

-- | The date when the delegated administrator\'s account became a part of
-- the organization.
delegatedAdministrator_joinedTimestamp :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.UTCTime)
delegatedAdministrator_joinedTimestamp = Lens.lens (\DelegatedAdministrator' {joinedTimestamp} -> joinedTimestamp) (\s@DelegatedAdministrator' {} a -> s {joinedTimestamp = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Time

-- | The friendly name of the delegated administrator\'s account.
delegatedAdministrator_name :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_name = Lens.lens (\DelegatedAdministrator' {name} -> name) (\s@DelegatedAdministrator' {} a -> s {name = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Sensitive

-- | The status of the delegated administrator\'s account in the
-- organization.
delegatedAdministrator_status :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe AccountStatus)
delegatedAdministrator_status = Lens.lens (\DelegatedAdministrator' {status} -> status) (\s@DelegatedAdministrator' {} a -> s {status = a} :: DelegatedAdministrator)

instance Data.FromJSON DelegatedAdministrator where
  parseJSON =
    Data.withObject
      "DelegatedAdministrator"
      ( \x ->
          DelegatedAdministrator'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "DelegationEnabledDate")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "JoinedMethod")
            Prelude.<*> (x Data..:? "JoinedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable DelegatedAdministrator where
  hashWithSalt _salt DelegatedAdministrator' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` delegationEnabledDate
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` joinedMethod
      `Prelude.hashWithSalt` joinedTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData DelegatedAdministrator where
  rnf DelegatedAdministrator' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf delegationEnabledDate
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf joinedMethod
      `Prelude.seq` Prelude.rnf joinedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
