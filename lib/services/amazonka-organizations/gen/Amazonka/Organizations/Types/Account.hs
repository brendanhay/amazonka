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
-- Module      : Amazonka.Organizations.Types.Account
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Organizations.Types.Account where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Organizations.Types.AccountJoinedMethod
import Amazonka.Organizations.Types.AccountStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an Amazon Web Services account that is a
-- member of an organization.
--
-- /See:/ 'newAccount' smart constructor.
data Account = Account'
  { -- | The Amazon Resource Name (ARN) of the account.
    --
    -- For more information about ARNs in Organizations, see
    -- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
    -- in the /Amazon Web Services Service Authorization Reference/.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The email address associated with the Amazon Web Services account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter
    -- is a string of characters that represents a standard internet email
    -- address.
    email :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The unique identifier (ID) of the account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
    -- string requires exactly 12 digits.
    id :: Prelude.Maybe Prelude.Text,
    -- | The method by which the account joined the organization.
    joinedMethod :: Prelude.Maybe AccountJoinedMethod,
    -- | The date the account became a part of the organization.
    joinedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The friendly name of the account.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of any of the characters in the
    -- ASCII character range.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The status of the account in the organization.
    status :: Prelude.Maybe AccountStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Account' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'account_arn' - The Amazon Resource Name (ARN) of the account.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
--
-- 'email', 'account_email' - The email address associated with the Amazon Web Services account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter
-- is a string of characters that represents a standard internet email
-- address.
--
-- 'id', 'account_id' - The unique identifier (ID) of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
--
-- 'joinedMethod', 'account_joinedMethod' - The method by which the account joined the organization.
--
-- 'joinedTimestamp', 'account_joinedTimestamp' - The date the account became a part of the organization.
--
-- 'name', 'account_name' - The friendly name of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
--
-- 'status', 'account_status' - The status of the account in the organization.
newAccount ::
  Account
newAccount =
  Account'
    { arn = Prelude.Nothing,
      email = Prelude.Nothing,
      id = Prelude.Nothing,
      joinedMethod = Prelude.Nothing,
      joinedTimestamp = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the account.
--
-- For more information about ARNs in Organizations, see
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsorganizations.html#awsorganizations-resources-for-iam-policies ARN Formats Supported by Organizations>
-- in the /Amazon Web Services Service Authorization Reference/.
account_arn :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_arn = Lens.lens (\Account' {arn} -> arn) (\s@Account' {} a -> s {arn = a} :: Account)

-- | The email address associated with the Amazon Web Services account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for this parameter
-- is a string of characters that represents a standard internet email
-- address.
account_email :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_email = Lens.lens (\Account' {email} -> email) (\s@Account' {} a -> s {email = a} :: Account) Prelude.. Lens.mapping Data._Sensitive

-- | The unique identifier (ID) of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for an account ID
-- string requires exactly 12 digits.
account_id :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_id = Lens.lens (\Account' {id} -> id) (\s@Account' {} a -> s {id = a} :: Account)

-- | The method by which the account joined the organization.
account_joinedMethod :: Lens.Lens' Account (Prelude.Maybe AccountJoinedMethod)
account_joinedMethod = Lens.lens (\Account' {joinedMethod} -> joinedMethod) (\s@Account' {} a -> s {joinedMethod = a} :: Account)

-- | The date the account became a part of the organization.
account_joinedTimestamp :: Lens.Lens' Account (Prelude.Maybe Prelude.UTCTime)
account_joinedTimestamp = Lens.lens (\Account' {joinedTimestamp} -> joinedTimestamp) (\s@Account' {} a -> s {joinedTimestamp = a} :: Account) Prelude.. Lens.mapping Data._Time

-- | The friendly name of the account.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of any of the characters in the
-- ASCII character range.
account_name :: Lens.Lens' Account (Prelude.Maybe Prelude.Text)
account_name = Lens.lens (\Account' {name} -> name) (\s@Account' {} a -> s {name = a} :: Account) Prelude.. Lens.mapping Data._Sensitive

-- | The status of the account in the organization.
account_status :: Lens.Lens' Account (Prelude.Maybe AccountStatus)
account_status = Lens.lens (\Account' {status} -> status) (\s@Account' {} a -> s {status = a} :: Account)

instance Data.FromJSON Account where
  parseJSON =
    Data.withObject
      "Account"
      ( \x ->
          Account'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Email")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "JoinedMethod")
            Prelude.<*> (x Data..:? "JoinedTimestamp")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
      )

instance Prelude.Hashable Account where
  hashWithSalt _salt Account' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` joinedMethod
      `Prelude.hashWithSalt` joinedTimestamp
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData Account where
  rnf Account' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf joinedMethod
      `Prelude.seq` Prelude.rnf joinedTimestamp
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
