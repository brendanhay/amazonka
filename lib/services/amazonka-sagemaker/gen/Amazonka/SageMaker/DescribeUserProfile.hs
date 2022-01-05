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
-- Copyright   : (c) 2013-2021 Brendan Hay
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
    describeUserProfileResponse_userSettings,
    describeUserProfileResponse_status,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Prelude.<$> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "UserSettings")
            Prelude.<*> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "FailureReason")
            Prelude.<*> (x Core..?> "SingleSignOnUserValue")
            Prelude.<*> (x Core..?> "UserProfileName")
            Prelude.<*> (x Core..?> "LastModifiedTime")
            Prelude.<*> (x Core..?> "HomeEfsFileSystemUid")
            Prelude.<*> (x Core..?> "UserProfileArn")
            Prelude.<*> (x Core..?> "SingleSignOnUserIdentifier")
            Prelude.<*> (x Core..?> "DomainId")
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

instance Core.ToHeaders DescribeUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeUserProfile" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Core..= domainId),
            Prelude.Just
              ("UserProfileName" Core..= userProfileName)
          ]
      )

instance Core.ToPath DescribeUserProfile where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The creation time.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | The status.
    status :: Prelude.Maybe UserProfileStatus,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The SSO user value.
    singleSignOnUserValue :: Prelude.Maybe Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
    -- volume.
    homeEfsFileSystemUid :: Prelude.Maybe Prelude.Text,
    -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The SSO user identifier.
    singleSignOnUserIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The ID of the domain that contains the profile.
    domainId :: Prelude.Maybe Prelude.Text,
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
-- 'userSettings', 'describeUserProfileResponse_userSettings' - A collection of settings.
--
-- 'status', 'describeUserProfileResponse_status' - The status.
--
-- 'failureReason', 'describeUserProfileResponse_failureReason' - The failure reason.
--
-- 'singleSignOnUserValue', 'describeUserProfileResponse_singleSignOnUserValue' - The SSO user value.
--
-- 'userProfileName', 'describeUserProfileResponse_userProfileName' - The user profile name.
--
-- 'lastModifiedTime', 'describeUserProfileResponse_lastModifiedTime' - The last modified time.
--
-- 'homeEfsFileSystemUid', 'describeUserProfileResponse_homeEfsFileSystemUid' - The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
--
-- 'userProfileArn', 'describeUserProfileResponse_userProfileArn' - The user profile Amazon Resource Name (ARN).
--
-- 'singleSignOnUserIdentifier', 'describeUserProfileResponse_singleSignOnUserIdentifier' - The SSO user identifier.
--
-- 'domainId', 'describeUserProfileResponse_domainId' - The ID of the domain that contains the profile.
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
      userSettings = Prelude.Nothing,
      status = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      singleSignOnUserValue = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      homeEfsFileSystemUid = Prelude.Nothing,
      userProfileArn = Prelude.Nothing,
      singleSignOnUserIdentifier = Prelude.Nothing,
      domainId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The creation time.
describeUserProfileResponse_creationTime :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeUserProfileResponse_creationTime = Lens.lens (\DescribeUserProfileResponse' {creationTime} -> creationTime) (\s@DescribeUserProfileResponse' {} a -> s {creationTime = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Core._Time

-- | A collection of settings.
describeUserProfileResponse_userSettings :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe UserSettings)
describeUserProfileResponse_userSettings = Lens.lens (\DescribeUserProfileResponse' {userSettings} -> userSettings) (\s@DescribeUserProfileResponse' {} a -> s {userSettings = a} :: DescribeUserProfileResponse)

-- | The status.
describeUserProfileResponse_status :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe UserProfileStatus)
describeUserProfileResponse_status = Lens.lens (\DescribeUserProfileResponse' {status} -> status) (\s@DescribeUserProfileResponse' {} a -> s {status = a} :: DescribeUserProfileResponse)

-- | The failure reason.
describeUserProfileResponse_failureReason :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_failureReason = Lens.lens (\DescribeUserProfileResponse' {failureReason} -> failureReason) (\s@DescribeUserProfileResponse' {} a -> s {failureReason = a} :: DescribeUserProfileResponse)

-- | The SSO user value.
describeUserProfileResponse_singleSignOnUserValue :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_singleSignOnUserValue = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserValue = a} :: DescribeUserProfileResponse)

-- | The user profile name.
describeUserProfileResponse_userProfileName :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_userProfileName = Lens.lens (\DescribeUserProfileResponse' {userProfileName} -> userProfileName) (\s@DescribeUserProfileResponse' {} a -> s {userProfileName = a} :: DescribeUserProfileResponse)

-- | The last modified time.
describeUserProfileResponse_lastModifiedTime :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeUserProfileResponse_lastModifiedTime = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTime = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Core._Time

-- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
describeUserProfileResponse_homeEfsFileSystemUid :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_homeEfsFileSystemUid = Lens.lens (\DescribeUserProfileResponse' {homeEfsFileSystemUid} -> homeEfsFileSystemUid) (\s@DescribeUserProfileResponse' {} a -> s {homeEfsFileSystemUid = a} :: DescribeUserProfileResponse)

-- | The user profile Amazon Resource Name (ARN).
describeUserProfileResponse_userProfileArn :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_userProfileArn = Lens.lens (\DescribeUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@DescribeUserProfileResponse' {} a -> s {userProfileArn = a} :: DescribeUserProfileResponse)

-- | The SSO user identifier.
describeUserProfileResponse_singleSignOnUserIdentifier :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_singleSignOnUserIdentifier = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserIdentifier = a} :: DescribeUserProfileResponse)

-- | The ID of the domain that contains the profile.
describeUserProfileResponse_domainId :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_domainId = Lens.lens (\DescribeUserProfileResponse' {domainId} -> domainId) (\s@DescribeUserProfileResponse' {} a -> s {domainId = a} :: DescribeUserProfileResponse)

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Prelude.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

instance Prelude.NFData DescribeUserProfileResponse where
  rnf DescribeUserProfileResponse' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf userSettings
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf singleSignOnUserValue
      `Prelude.seq` Prelude.rnf userProfileName
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf homeEfsFileSystemUid
      `Prelude.seq` Prelude.rnf userProfileArn
      `Prelude.seq` Prelude.rnf singleSignOnUserIdentifier
      `Prelude.seq` Prelude.rnf domainId
      `Prelude.seq` Prelude.rnf httpStatus
