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
-- Module      : Network.AWS.Organizations.Types.DelegatedAdministrator
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Organizations.Types.DelegatedAdministrator where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types.AccountJoinedMethod
import Network.AWS.Organizations.Types.AccountStatus
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the delegated administrator.
--
-- /See:/ 'newDelegatedAdministrator' smart constructor.
data DelegatedAdministrator = DelegatedAdministrator'
  { -- | The status of the delegated administrator\'s account in the
    -- organization.
    status :: Prelude.Maybe AccountStatus,
    -- | The method by which the delegated administrator\'s account joined the
    -- organization.
    joinedMethod :: Prelude.Maybe AccountJoinedMethod,
    -- | The Amazon Resource Name (ARN) of the delegated administrator\'s
    -- account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date when the delegated administrator\'s account became a part of
    -- the organization.
    joinedTimestamp :: Prelude.Maybe Prelude.POSIX,
    -- | The unique identifier (ID) of the delegated administrator\'s account.
    id :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the delegated administrator\'s account.
    name :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The email address that is associated with the delegated administrator\'s
    -- AWS account.
    email :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The date when the account was made a delegated administrator.
    delegationEnabledDate :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DelegatedAdministrator' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'delegatedAdministrator_status' - The status of the delegated administrator\'s account in the
-- organization.
--
-- 'joinedMethod', 'delegatedAdministrator_joinedMethod' - The method by which the delegated administrator\'s account joined the
-- organization.
--
-- 'arn', 'delegatedAdministrator_arn' - The Amazon Resource Name (ARN) of the delegated administrator\'s
-- account.
--
-- 'joinedTimestamp', 'delegatedAdministrator_joinedTimestamp' - The date when the delegated administrator\'s account became a part of
-- the organization.
--
-- 'id', 'delegatedAdministrator_id' - The unique identifier (ID) of the delegated administrator\'s account.
--
-- 'name', 'delegatedAdministrator_name' - The friendly name of the delegated administrator\'s account.
--
-- 'email', 'delegatedAdministrator_email' - The email address that is associated with the delegated administrator\'s
-- AWS account.
--
-- 'delegationEnabledDate', 'delegatedAdministrator_delegationEnabledDate' - The date when the account was made a delegated administrator.
newDelegatedAdministrator ::
  DelegatedAdministrator
newDelegatedAdministrator =
  DelegatedAdministrator'
    { status = Prelude.Nothing,
      joinedMethod = Prelude.Nothing,
      arn = Prelude.Nothing,
      joinedTimestamp = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      email = Prelude.Nothing,
      delegationEnabledDate = Prelude.Nothing
    }

-- | The status of the delegated administrator\'s account in the
-- organization.
delegatedAdministrator_status :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe AccountStatus)
delegatedAdministrator_status = Lens.lens (\DelegatedAdministrator' {status} -> status) (\s@DelegatedAdministrator' {} a -> s {status = a} :: DelegatedAdministrator)

-- | The method by which the delegated administrator\'s account joined the
-- organization.
delegatedAdministrator_joinedMethod :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe AccountJoinedMethod)
delegatedAdministrator_joinedMethod = Lens.lens (\DelegatedAdministrator' {joinedMethod} -> joinedMethod) (\s@DelegatedAdministrator' {} a -> s {joinedMethod = a} :: DelegatedAdministrator)

-- | The Amazon Resource Name (ARN) of the delegated administrator\'s
-- account.
delegatedAdministrator_arn :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_arn = Lens.lens (\DelegatedAdministrator' {arn} -> arn) (\s@DelegatedAdministrator' {} a -> s {arn = a} :: DelegatedAdministrator)

-- | The date when the delegated administrator\'s account became a part of
-- the organization.
delegatedAdministrator_joinedTimestamp :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.UTCTime)
delegatedAdministrator_joinedTimestamp = Lens.lens (\DelegatedAdministrator' {joinedTimestamp} -> joinedTimestamp) (\s@DelegatedAdministrator' {} a -> s {joinedTimestamp = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Prelude._Time

-- | The unique identifier (ID) of the delegated administrator\'s account.
delegatedAdministrator_id :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_id = Lens.lens (\DelegatedAdministrator' {id} -> id) (\s@DelegatedAdministrator' {} a -> s {id = a} :: DelegatedAdministrator)

-- | The friendly name of the delegated administrator\'s account.
delegatedAdministrator_name :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_name = Lens.lens (\DelegatedAdministrator' {name} -> name) (\s@DelegatedAdministrator' {} a -> s {name = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Prelude._Sensitive

-- | The email address that is associated with the delegated administrator\'s
-- AWS account.
delegatedAdministrator_email :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.Text)
delegatedAdministrator_email = Lens.lens (\DelegatedAdministrator' {email} -> email) (\s@DelegatedAdministrator' {} a -> s {email = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Prelude._Sensitive

-- | The date when the account was made a delegated administrator.
delegatedAdministrator_delegationEnabledDate :: Lens.Lens' DelegatedAdministrator (Prelude.Maybe Prelude.UTCTime)
delegatedAdministrator_delegationEnabledDate = Lens.lens (\DelegatedAdministrator' {delegationEnabledDate} -> delegationEnabledDate) (\s@DelegatedAdministrator' {} a -> s {delegationEnabledDate = a} :: DelegatedAdministrator) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON DelegatedAdministrator where
  parseJSON =
    Prelude.withObject
      "DelegatedAdministrator"
      ( \x ->
          DelegatedAdministrator'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "JoinedMethod")
            Prelude.<*> (x Prelude..:? "Arn")
            Prelude.<*> (x Prelude..:? "JoinedTimestamp")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Email")
            Prelude.<*> (x Prelude..:? "DelegationEnabledDate")
      )

instance Prelude.Hashable DelegatedAdministrator

instance Prelude.NFData DelegatedAdministrator
