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
-- Module      : Amazonka.MacieV2.Types.Member
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.Member where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.RelationshipStatus
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an account that\'s associated with an Amazon
-- Macie administrator account.
--
-- /See:/ 'newMember' smart constructor.
data Member = Member'
  { -- | The Amazon Web Services account ID for the account.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID for the administrator account.
    administratorAccountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The email address for the account. This value is null if the account is
    -- associated with the administrator account through Organizations.
    email :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when an Amazon
    -- Macie membership invitation was last sent to the account. This value is
    -- null if a Macie membership invitation hasn\'t been sent to the account.
    invitedAt :: Prelude.Maybe Data.ISO8601,
    -- | (Deprecated) The Amazon Web Services account ID for the administrator
    -- account. This property has been replaced by the administratorAccountId
    -- property and is retained only for backward compatibility.
    masterAccountId :: Prelude.Maybe Prelude.Text,
    -- | The current status of the relationship between the account and the
    -- administrator account.
    relationshipStatus :: Prelude.Maybe RelationshipStatus,
    -- | A map of key-value pairs that specifies which tags (keys and values) are
    -- associated with the account in Amazon Macie.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date and time, in UTC and extended ISO 8601 format, of the most
    -- recent change to the status of the relationship between the account and
    -- the administrator account.
    updatedAt :: Prelude.Maybe Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Member' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'member_accountId' - The Amazon Web Services account ID for the account.
--
-- 'administratorAccountId', 'member_administratorAccountId' - The Amazon Web Services account ID for the administrator account.
--
-- 'arn', 'member_arn' - The Amazon Resource Name (ARN) of the account.
--
-- 'email', 'member_email' - The email address for the account. This value is null if the account is
-- associated with the administrator account through Organizations.
--
-- 'invitedAt', 'member_invitedAt' - The date and time, in UTC and extended ISO 8601 format, when an Amazon
-- Macie membership invitation was last sent to the account. This value is
-- null if a Macie membership invitation hasn\'t been sent to the account.
--
-- 'masterAccountId', 'member_masterAccountId' - (Deprecated) The Amazon Web Services account ID for the administrator
-- account. This property has been replaced by the administratorAccountId
-- property and is retained only for backward compatibility.
--
-- 'relationshipStatus', 'member_relationshipStatus' - The current status of the relationship between the account and the
-- administrator account.
--
-- 'tags', 'member_tags' - A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the account in Amazon Macie.
--
-- 'updatedAt', 'member_updatedAt' - The date and time, in UTC and extended ISO 8601 format, of the most
-- recent change to the status of the relationship between the account and
-- the administrator account.
newMember ::
  Member
newMember =
  Member'
    { accountId = Prelude.Nothing,
      administratorAccountId = Prelude.Nothing,
      arn = Prelude.Nothing,
      email = Prelude.Nothing,
      invitedAt = Prelude.Nothing,
      masterAccountId = Prelude.Nothing,
      relationshipStatus = Prelude.Nothing,
      tags = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Amazon Web Services account ID for the account.
member_accountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_accountId = Lens.lens (\Member' {accountId} -> accountId) (\s@Member' {} a -> s {accountId = a} :: Member)

-- | The Amazon Web Services account ID for the administrator account.
member_administratorAccountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_administratorAccountId = Lens.lens (\Member' {administratorAccountId} -> administratorAccountId) (\s@Member' {} a -> s {administratorAccountId = a} :: Member)

-- | The Amazon Resource Name (ARN) of the account.
member_arn :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_arn = Lens.lens (\Member' {arn} -> arn) (\s@Member' {} a -> s {arn = a} :: Member)

-- | The email address for the account. This value is null if the account is
-- associated with the administrator account through Organizations.
member_email :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_email = Lens.lens (\Member' {email} -> email) (\s@Member' {} a -> s {email = a} :: Member)

-- | The date and time, in UTC and extended ISO 8601 format, when an Amazon
-- Macie membership invitation was last sent to the account. This value is
-- null if a Macie membership invitation hasn\'t been sent to the account.
member_invitedAt :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_invitedAt = Lens.lens (\Member' {invitedAt} -> invitedAt) (\s@Member' {} a -> s {invitedAt = a} :: Member) Prelude.. Lens.mapping Data._Time

-- | (Deprecated) The Amazon Web Services account ID for the administrator
-- account. This property has been replaced by the administratorAccountId
-- property and is retained only for backward compatibility.
member_masterAccountId :: Lens.Lens' Member (Prelude.Maybe Prelude.Text)
member_masterAccountId = Lens.lens (\Member' {masterAccountId} -> masterAccountId) (\s@Member' {} a -> s {masterAccountId = a} :: Member)

-- | The current status of the relationship between the account and the
-- administrator account.
member_relationshipStatus :: Lens.Lens' Member (Prelude.Maybe RelationshipStatus)
member_relationshipStatus = Lens.lens (\Member' {relationshipStatus} -> relationshipStatus) (\s@Member' {} a -> s {relationshipStatus = a} :: Member)

-- | A map of key-value pairs that specifies which tags (keys and values) are
-- associated with the account in Amazon Macie.
member_tags :: Lens.Lens' Member (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
member_tags = Lens.lens (\Member' {tags} -> tags) (\s@Member' {} a -> s {tags = a} :: Member) Prelude.. Lens.mapping Lens.coerced

-- | The date and time, in UTC and extended ISO 8601 format, of the most
-- recent change to the status of the relationship between the account and
-- the administrator account.
member_updatedAt :: Lens.Lens' Member (Prelude.Maybe Prelude.UTCTime)
member_updatedAt = Lens.lens (\Member' {updatedAt} -> updatedAt) (\s@Member' {} a -> s {updatedAt = a} :: Member) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON Member where
  parseJSON =
    Data.withObject
      "Member"
      ( \x ->
          Member'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "administratorAccountId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "email")
            Prelude.<*> (x Data..:? "invitedAt")
            Prelude.<*> (x Data..:? "masterAccountId")
            Prelude.<*> (x Data..:? "relationshipStatus")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "updatedAt")
      )

instance Prelude.Hashable Member where
  hashWithSalt _salt Member' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` administratorAccountId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` email
      `Prelude.hashWithSalt` invitedAt
      `Prelude.hashWithSalt` masterAccountId
      `Prelude.hashWithSalt` relationshipStatus
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Member where
  rnf Member' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf administratorAccountId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf email
      `Prelude.seq` Prelude.rnf invitedAt
      `Prelude.seq` Prelude.rnf masterAccountId
      `Prelude.seq` Prelude.rnf relationshipStatus
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf updatedAt
