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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
  { -- | The friendly name of the delegated administrator\'s account.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The email address that is associated with the delegated administrator\'s
    -- Amazon Web Services account.
    email :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the delegated administrator\'s
    -- account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The status of the delegated administrator\'s account in the
    -- organization.
    status :: Prelude.Maybe AccountStatus,
    -- | The unique identifier (ID) of the delegated administrator\'s account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The date when the delegated administrator\'s account became a part of
    -- the organization.
    joinedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The method by which the delegated administrator\'s account joined the
    -- organization.
    joinedMethod :: Prelude.Maybe AccountJoinedMethod,
    -- | The date when the account was made a delegated administrator.
    delegationEnabledDate :: Prelude.Maybe Data.POSIX
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
-- 'name', 'delegatedAdministrator_name' - The friendly name of the delegated administrator\'s account.
--
-- 'email', 'delegatedAdministrator_email' - The email address that is associated with the delegated administrator\'s
-- Amazon Web Services account.
--
-- 'arn', 'delegatedAdministrator_arn' - The Amazon Resource Name (ARN) of the delegated administrator\'s
-- account.
--
-- 'status', 'delegatedAdministrator_status' - The status of the delegated administrator\'s account in the
-- organization.
--
-- 'id', 'delegatedAdministrator_id' - The unique identifier (ID) of the delegated administrator\'s account.
--
-- 'joinedTimestamp', 'delegatedAdministrator_joinedTimestamp' - The date when the delegated administrator\'s account became a part of
-- the organization.
--
-- 'joinedMethod', 'delegatedAdministrator_joinedMethod' - The method by which the delegated administrator\'s account joined the
-- organization.
--
-- 'delegationEnabledDate', 'delegatedAdministrator_delegationEnabledDate' - The date when the account was made a delegated administrator.
newDelegatedAdministrator ::
  DelegatedAdministrator
newDelegatedAdministrator =
  DelegatedAdministrator'
    { name = Prelude.Nothing,
      email = Prelude.Nothing,
      arn = Prelude.Nothing,
      status = Prelude.Nothing,
      id = Prelude.Nothing,
      joinedTimestamp = Prelude.Nothing,
      joinedMethod = Prelude.Nothing,
      delegationEnabledDate = Prelude.Nothing
    }

-- | The friendly name of the delegated administrator\'s account.
delegatedAdministrator_name :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_name = Lens.lens (\DelegatedAdministrator' {name} -> name) (\s@DelegatedAdministrator' {} a -> s {name = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Sensitive

-- | The email address that is associated with the delegated administrator\'s
-- Amazon Web Services account.
delegatedAdministrator_email :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_email = Lens.lens (\DelegatedAdministrator' {email} -> email) (\s@DelegatedAdministrator' {} a -> s {email = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the delegated administrator\'s
-- account.
delegatedAdministrator_arn :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_arn = Lens.lens (\DelegatedAdministrator' {arn} -> arn) (\s@DelegatedAdministrator' {} a -> s {arn = a} :: DelegatedAdministrator)

-- | The status of the delegated administrator\'s account in the
-- organization.
delegatedAdministrator_status :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe AccountStatus)
delegatedAdministrator_status = Lens.lens (\DelegatedAdministrator' {status} -> status) (\s@DelegatedAdministrator' {} a -> s {status = a} :: DelegatedAdministrator)

-- | The unique identifier (ID) of the delegated administrator\'s account.
delegatedAdministrator_id :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_id = Lens.lens (\DelegatedAdministrator' {id} -> id) (\s@DelegatedAdministrator' {} a -> s {id = a} :: DelegatedAdministrator)

-- | The date when the delegated administrator\'s account became a part of
-- the organization.
delegatedAdministrator_joinedTimestamp :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.UTCTime)
delegatedAdministrator_joinedTimestamp = Lens.lens (\DelegatedAdministrator' {joinedTimestamp} -> joinedTimestamp) (\s@DelegatedAdministrator' {} a -> s {joinedTimestamp = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Time

-- | The method by which the delegated administrator\'s account joined the
-- organization.
delegatedAdministrator_joinedMethod :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe AccountJoinedMethod)
delegatedAdministrator_joinedMethod = Lens.lens (\DelegatedAdministrator' {joinedMethod} -> joinedMethod) (\s@DelegatedAdministrator' {} a -> s {joinedMethod = a} :: DelegatedAdministrator)

-- | The date when the account was made a delegated administrator.
delegatedAdministrator_delegationEnabledDate :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.UTCTime)
delegatedAdministrator_delegationEnabledDate = Lens.lens (\DelegatedAdministrator' {delegationEnabledDate} -> delegationEnabledDate) (\s@DelegatedAdministrator' {} a -> s {delegationEnabledDate = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON DelegatedAdministrator where
  parseJSON =
    Data.withObject
      "DelegatedAdministrator"
      ( \x ->
          DelegatedAdministrator'
            Prelude.<$> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "JoinedTimestamp")
            Prelude.<*> (x Data..:? "JoinedMethod")
            Prelude.<*> (x Data..:? "DelegationEnabledDate")
      )

instance Prelude.Hashable DelegatedAdministrator where
  hashWithSalt _salt DelegatedAdministrator' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` joinedTimestamp
      `Prelude.hashWithSalt` joinedMethod
      `Prelude.hashWithSalt` delegationEnabledDate

instance Prelude.NFData DelegatedAdministrator where
  rnf DelegatedAdministrator' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf joinedTimestamp
      `Prelude.seq` Prelude.rnf joinedMethod
      `Prelude.seq` Prelude.rnf delegationEnabledDate
