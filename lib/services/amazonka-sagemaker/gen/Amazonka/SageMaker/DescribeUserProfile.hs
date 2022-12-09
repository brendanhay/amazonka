{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.DescribeUserProfile
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user profile. For more information, see @CreateUserProfile@.
module Amazonka.SageMaker.DescribeUserProfile
  ( -- * Creating a Request
    DescribeUserProfile (..),
    newDescribeUserProfile,

    -- * Request Lenses
    describeUserProfile_domainId,
    describeUserProfile_userProfileName,

    -- * Destructuring the Response
    DescribeUserProfileResponse (..),
    newDescribeUserProfileResponse,

    -- * Response Lenses
    describeUserProfileResponse_creationTime,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_status,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_userSettings,
    describeUserProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name. This value is not case sensitive.
    userProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domainId', 'describeUserProfile_domainId' - The domain ID.
--
-- 'userProfileName', 'describeUserProfile_userProfileName' - The user profile name. This value is not case sensitive.
newDescribeUserProfile ::
  -- | 'domainId'
  Prelude.Text ->
  -- | 'userProfileName'
  Prelude.Text ->
  DescribeUserProfile
newDescribeUserProfile pDomainId_ pUserProfileName_ =
  DescribeUserProfile'
    { domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | The domain ID.
describeUserProfile_domainId :: Lens.Lens' DescribeUserProfile Prelude.Text
describeUserProfile_domainId = Lens.lens (\DescribeUserProfile' {domainId} -> domainId) (\s@DescribeUserProfile' {} a -> s {domainId = a} :: DescribeUserProfile)

-- | The user profile name. This value is not case sensitive.
describeUserProfile_userProfileName :: Lens.Lens' DescribeUserProfile Prelude.Text
describeUserProfile_userProfileName = Lens.lens (\DescribeUserProfile' {userProfileName} -> userProfileName) (\s@DescribeUserProfile' {} a -> s {userProfileName = a} :: DescribeUserProfile)

instance Core.AWSRequest DescribeUserProfile where
  type
    AWSResponse DescribeUserProfile =
      DescribeUserProfileResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Prelude.<$> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "DomainId")
            Prelude.<*> (x Data..?> "FailureReason")
            Prelude.<*> (x Data..?> "HomeEfsFileSystemUid")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "SingleSignOnUserIdentifier")
            Prelude.<*> (x Data..?> "SingleSignOnUserValue")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "UserProfileArn")
            Prelude.<*> (x Data..?> "UserProfileName")
            Prelude.<*> (x Data..?> "UserSettings")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserProfile where
  hashWithSalt _salt DescribeUserProfile' {..} =
    _salt `Prelude.hashWithSalt` domainId
      `Prelude.hashWithSalt` userProfileName

instance Prelude.NFData DescribeUserProfile where
  rnf DescribeUserProfile' {..} =
    Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf userProfileName

instance Data.ToHeaders DescribeUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.DescribeUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Data..= domainId),
            Prelude.Just
              ("UserProfileName" Data..= userProfileName)
          ]
      )

instance Data.ToPath DescribeUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the domain that contains the profile.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
    -- volume.
    homeEfsFileSystemUid :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The IAM Identity Center user identifier.
    singleSignOnUserIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The IAM Identity Center user value.
    singleSignOnUserValue :: Prelude.Maybe Prelude.Text,
    -- | The status.
    status :: Prelude.Maybe UserProfileStatus,
    -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'describeUserProfileResponse_creationTime' - The creation time.
--
-- 'domainId', 'describeUserProfileResponse_domainId' - The ID of the domain that contains the profile.
--
-- 'failureReason', 'describeUserProfileResponse_failureReason' - The failure reason.
--
-- 'homeEfsFileSystemUid', 'describeUserProfileResponse_homeEfsFileSystemUid' - The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
--
-- 'lastModifiedTime', 'describeUserProfileResponse_lastModifiedTime' - The last modified time.
--
-- 'singleSignOnUserIdentifier', 'describeUserProfileResponse_singleSignOnUserIdentifier' - The IAM Identity Center user identifier.
--
-- 'singleSignOnUserValue', 'describeUserProfileResponse_singleSignOnUserValue' - The IAM Identity Center user value.
--
-- 'status', 'describeUserProfileResponse_status' - The status.
--
-- 'userProfileArn', 'describeUserProfileResponse_userProfileArn' - The user profile Amazon Resource Name (ARN).
--
-- 'userProfileName', 'describeUserProfileResponse_userProfileName' - The user profile name.
--
-- 'userSettings', 'describeUserProfileResponse_userSettings' - A collection of settings.
--
-- 'httpStatus', 'describeUserProfileResponse_httpStatus' - The response's http status code.
newDescribeUserProfileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeUserProfileResponse
newDescribeUserProfileResponse pHttpStatus_ =
  DescribeUserProfileResponse'
    { creationTime =
        Prelude.Nothing,
      domainId = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      homeEfsFileSystemUid = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      singleSignOnUserIdentifier = Prelude.Nothing,
      singleSignOnUserValue = Prelude.Nothing,
      status = Prelude.Nothing,
      userProfileArn = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation time.
describeUserProfileResponse_creationTime :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeUserProfileResponse_creationTime = Lens.lens (\DescribeUserProfileResponse' {creationTime} -> creationTime) (\s@DescribeUserProfileResponse' {} a -> s {creationTime = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The ID of the domain that contains the profile.
describeUserProfileResponse_domainId :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_domainId = Lens.lens (\DescribeUserProfileResponse' {domainId} -> domainId) (\s@DescribeUserProfileResponse' {} a -> s {domainId = a} :: DescribeUserProfileResponse)

-- | The failure reason.
describeUserProfileResponse_failureReason :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_failureReason = Lens.lens (\DescribeUserProfileResponse' {failureReason} -> failureReason) (\s@DescribeUserProfileResponse' {} a -> s {failureReason = a} :: DescribeUserProfileResponse)

-- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
describeUserProfileResponse_homeEfsFileSystemUid :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_homeEfsFileSystemUid = Lens.lens (\DescribeUserProfileResponse' {homeEfsFileSystemUid} -> homeEfsFileSystemUid) (\s@DescribeUserProfileResponse' {} a -> s {homeEfsFileSystemUid = a} :: DescribeUserProfileResponse)

-- | The last modified time.
describeUserProfileResponse_lastModifiedTime :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeUserProfileResponse_lastModifiedTime = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTime = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Data._Time

-- | The IAM Identity Center user identifier.
describeUserProfileResponse_singleSignOnUserIdentifier :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_singleSignOnUserIdentifier = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserIdentifier = a} :: DescribeUserProfileResponse)

-- | The IAM Identity Center user value.
describeUserProfileResponse_singleSignOnUserValue :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_singleSignOnUserValue = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserValue = a} :: DescribeUserProfileResponse)

-- | The status.
describeUserProfileResponse_status :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe UserProfileStatus)
describeUserProfileResponse_status = Lens.lens (\DescribeUserProfileResponse' {status} -> status) (\s@DescribeUserProfileResponse' {} a -> s {status = a} :: DescribeUserProfileResponse)

-- | The user profile Amazon Resource Name (ARN).
describeUserProfileResponse_userProfileArn :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_userProfileArn = Lens.lens (\DescribeUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@DescribeUserProfileResponse' {} a -> s {userProfileArn = a} :: DescribeUserProfileResponse)

-- | The user profile name.
describeUserProfileResponse_userProfileName :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_userProfileName = Lens.lens (\DescribeUserProfileResponse' {userProfileName} -> userProfileName) (\s@DescribeUserProfileResponse' {} a -> s {userProfileName = a} :: DescribeUserProfileResponse)

-- | A collection of settings.
describeUserProfileResponse_userSettings :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe UserSettings)
describeUserProfileResponse_userSettings = Lens.lens (\DescribeUserProfileResponse' {userSettings} -> userSettings) (\s@DescribeUserProfileResponse' {} a -> s {userSettings = a} :: DescribeUserProfileResponse)

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Prelude.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

instance Prelude.NFData DescribeUserProfileResponse where
  rnf DescribeUserProfileResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf homeEfsFileSystemUid
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf singleSignOnUserIdentifier
      `Prelude.seq` Prelude.rnf singleSignOnUserValue
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf userProfileArn
      `Prelude.seq` Prelude.rnf userProfileName
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf httpStatus
