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
-- Module      : Amazonka.SecurityHub.Types.AwsIamAccessKeyDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamAccessKeyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAccessKeySessionContext
import Amazonka.SecurityHub.Types.AwsIamAccessKeyStatus

-- | IAM access key details related to a finding.
--
-- /See:/ 'newAwsIamAccessKeyDetails' smart constructor.
data AwsIamAccessKeyDetails = AwsIamAccessKeyDetails'
  { -- | The identifier of the access key.
    accessKeyId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the account for the key.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the IAM access key was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the principal associated with an access key.
    principalId :: Prelude.Maybe Prelude.Text,
    -- | The name of the principal.
    principalName :: Prelude.Maybe Prelude.Text,
    -- | The type of principal associated with an access key.
    principalType :: Prelude.Maybe Prelude.Text,
    -- | Information about the session that the key was used for.
    sessionContext :: Prelude.Maybe AwsIamAccessKeySessionContext,
    -- | The status of the IAM access key related to a finding.
    status :: Prelude.Maybe AwsIamAccessKeyStatus,
    -- | The user associated with the IAM access key related to a finding.
    --
    -- The @UserName@ parameter has been replaced with the @PrincipalName@
    -- parameter because access keys can also be assigned to principals that
    -- are not IAM users.
    userName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamAccessKeyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessKeyId', 'awsIamAccessKeyDetails_accessKeyId' - The identifier of the access key.
--
-- 'accountId', 'awsIamAccessKeyDetails_accountId' - The Amazon Web Services account ID of the account for the key.
--
-- 'createdAt', 'awsIamAccessKeyDetails_createdAt' - Indicates when the IAM access key was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'principalId', 'awsIamAccessKeyDetails_principalId' - The ID of the principal associated with an access key.
--
-- 'principalName', 'awsIamAccessKeyDetails_principalName' - The name of the principal.
--
-- 'principalType', 'awsIamAccessKeyDetails_principalType' - The type of principal associated with an access key.
--
-- 'sessionContext', 'awsIamAccessKeyDetails_sessionContext' - Information about the session that the key was used for.
--
-- 'status', 'awsIamAccessKeyDetails_status' - The status of the IAM access key related to a finding.
--
-- 'userName', 'awsIamAccessKeyDetails_userName' - The user associated with the IAM access key related to a finding.
--
-- The @UserName@ parameter has been replaced with the @PrincipalName@
-- parameter because access keys can also be assigned to principals that
-- are not IAM users.
newAwsIamAccessKeyDetails ::
  AwsIamAccessKeyDetails
newAwsIamAccessKeyDetails =
  AwsIamAccessKeyDetails'
    { accessKeyId =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      principalId = Prelude.Nothing,
      principalName = Prelude.Nothing,
      principalType = Prelude.Nothing,
      sessionContext = Prelude.Nothing,
      status = Prelude.Nothing,
      userName = Prelude.Nothing
    }

-- | The identifier of the access key.
awsIamAccessKeyDetails_accessKeyId :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_accessKeyId = Lens.lens (\AwsIamAccessKeyDetails' {accessKeyId} -> accessKeyId) (\s@AwsIamAccessKeyDetails' {} a -> s {accessKeyId = a} :: AwsIamAccessKeyDetails)

-- | The Amazon Web Services account ID of the account for the key.
awsIamAccessKeyDetails_accountId :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_accountId = Lens.lens (\AwsIamAccessKeyDetails' {accountId} -> accountId) (\s@AwsIamAccessKeyDetails' {} a -> s {accountId = a} :: AwsIamAccessKeyDetails)

-- | Indicates when the IAM access key was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamAccessKeyDetails_createdAt :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_createdAt = Lens.lens (\AwsIamAccessKeyDetails' {createdAt} -> createdAt) (\s@AwsIamAccessKeyDetails' {} a -> s {createdAt = a} :: AwsIamAccessKeyDetails)

-- | The ID of the principal associated with an access key.
awsIamAccessKeyDetails_principalId :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_principalId = Lens.lens (\AwsIamAccessKeyDetails' {principalId} -> principalId) (\s@AwsIamAccessKeyDetails' {} a -> s {principalId = a} :: AwsIamAccessKeyDetails)

-- | The name of the principal.
awsIamAccessKeyDetails_principalName :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_principalName = Lens.lens (\AwsIamAccessKeyDetails' {principalName} -> principalName) (\s@AwsIamAccessKeyDetails' {} a -> s {principalName = a} :: AwsIamAccessKeyDetails)

-- | The type of principal associated with an access key.
awsIamAccessKeyDetails_principalType :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_principalType = Lens.lens (\AwsIamAccessKeyDetails' {principalType} -> principalType) (\s@AwsIamAccessKeyDetails' {} a -> s {principalType = a} :: AwsIamAccessKeyDetails)

-- | Information about the session that the key was used for.
awsIamAccessKeyDetails_sessionContext :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe AwsIamAccessKeySessionContext)
awsIamAccessKeyDetails_sessionContext = Lens.lens (\AwsIamAccessKeyDetails' {sessionContext} -> sessionContext) (\s@AwsIamAccessKeyDetails' {} a -> s {sessionContext = a} :: AwsIamAccessKeyDetails)

-- | The status of the IAM access key related to a finding.
awsIamAccessKeyDetails_status :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe AwsIamAccessKeyStatus)
awsIamAccessKeyDetails_status = Lens.lens (\AwsIamAccessKeyDetails' {status} -> status) (\s@AwsIamAccessKeyDetails' {} a -> s {status = a} :: AwsIamAccessKeyDetails)

-- | The user associated with the IAM access key related to a finding.
--
-- The @UserName@ parameter has been replaced with the @PrincipalName@
-- parameter because access keys can also be assigned to principals that
-- are not IAM users.
awsIamAccessKeyDetails_userName :: Lens.Lens' AwsIamAccessKeyDetails (Prelude.Maybe Prelude.Text)
awsIamAccessKeyDetails_userName = Lens.lens (\AwsIamAccessKeyDetails' {userName} -> userName) (\s@AwsIamAccessKeyDetails' {} a -> s {userName = a} :: AwsIamAccessKeyDetails)

instance Data.FromJSON AwsIamAccessKeyDetails where
  parseJSON =
    Data.withObject
      "AwsIamAccessKeyDetails"
      ( \x ->
          AwsIamAccessKeyDetails'
            Prelude.<$> (x Data..:? "AccessKeyId")
            Prelude.<*> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "PrincipalId")
            Prelude.<*> (x Data..:? "PrincipalName")
            Prelude.<*> (x Data..:? "PrincipalType")
            Prelude.<*> (x Data..:? "SessionContext")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UserName")
      )

instance Prelude.Hashable AwsIamAccessKeyDetails where
  hashWithSalt _salt AwsIamAccessKeyDetails' {..} =
    _salt `Prelude.hashWithSalt` accessKeyId
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` principalId
      `Prelude.hashWithSalt` principalName
      `Prelude.hashWithSalt` principalType
      `Prelude.hashWithSalt` sessionContext
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` userName

instance Prelude.NFData AwsIamAccessKeyDetails where
  rnf AwsIamAccessKeyDetails' {..} =
    Prelude.rnf accessKeyId
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf principalId
      `Prelude.seq` Prelude.rnf principalName
      `Prelude.seq` Prelude.rnf principalType
      `Prelude.seq` Prelude.rnf sessionContext
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf userName

instance Data.ToJSON AwsIamAccessKeyDetails where
  toJSON AwsIamAccessKeyDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessKeyId" Data..=) Prelude.<$> accessKeyId,
            ("AccountId" Data..=) Prelude.<$> accountId,
            ("CreatedAt" Data..=) Prelude.<$> createdAt,
            ("PrincipalId" Data..=) Prelude.<$> principalId,
            ("PrincipalName" Data..=) Prelude.<$> principalName,
            ("PrincipalType" Data..=) Prelude.<$> principalType,
            ("SessionContext" Data..=)
              Prelude.<$> sessionContext,
            ("Status" Data..=) Prelude.<$> status,
            ("UserName" Data..=) Prelude.<$> userName
          ]
      )
