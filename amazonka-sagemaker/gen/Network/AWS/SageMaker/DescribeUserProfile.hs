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
-- Module      : Network.AWS.SageMaker.DescribeUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user profile. For more information, see @CreateUserProfile@.
module Network.AWS.SageMaker.DescribeUserProfile
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
    describeUserProfileResponse_status,
    describeUserProfileResponse_creationTime,
    describeUserProfileResponse_userSettings,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The domain ID.
    domainId :: Core.Text,
    -- | The user profile name.
    userProfileName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'userProfileName', 'describeUserProfile_userProfileName' - The user profile name.
newDescribeUserProfile ::
  -- | 'domainId'
  Core.Text ->
  -- | 'userProfileName'
  Core.Text ->
  DescribeUserProfile
newDescribeUserProfile pDomainId_ pUserProfileName_ =
  DescribeUserProfile'
    { domainId = pDomainId_,
      userProfileName = pUserProfileName_
    }

-- | The domain ID.
describeUserProfile_domainId :: Lens.Lens' DescribeUserProfile Core.Text
describeUserProfile_domainId = Lens.lens (\DescribeUserProfile' {domainId} -> domainId) (\s@DescribeUserProfile' {} a -> s {domainId = a} :: DescribeUserProfile)

-- | The user profile name.
describeUserProfile_userProfileName :: Lens.Lens' DescribeUserProfile Core.Text
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
            Core.<$> (x Core..?> "Status")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "UserSettings")
            Core.<*> (x Core..?> "UserProfileName")
            Core.<*> (x Core..?> "DomainId")
            Core.<*> (x Core..?> "UserProfileArn")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "HomeEfsFileSystemUid")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "SingleSignOnUserIdentifier")
            Core.<*> (x Core..?> "SingleSignOnUserValue")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeUserProfile

instance Core.NFData DescribeUserProfile

instance Core.ToHeaders DescribeUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.DescribeUserProfile" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("DomainId" Core..= domainId),
            Core.Just
              ("UserProfileName" Core..= userProfileName)
          ]
      )

instance Core.ToPath DescribeUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The status.
    status :: Core.Maybe UserProfileStatus,
    -- | The creation time.
    creationTime :: Core.Maybe Core.POSIX,
    -- | A collection of settings.
    userSettings :: Core.Maybe UserSettings,
    -- | The user profile name.
    userProfileName :: Core.Maybe Core.Text,
    -- | The ID of the domain that contains the profile.
    domainId :: Core.Maybe Core.Text,
    -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Core.Maybe Core.Text,
    -- | The failure reason.
    failureReason :: Core.Maybe Core.Text,
    -- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
    -- volume.
    homeEfsFileSystemUid :: Core.Maybe Core.Text,
    -- | The last modified time.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The SSO user identifier.
    singleSignOnUserIdentifier :: Core.Maybe Core.Text,
    -- | The SSO user value.
    singleSignOnUserValue :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeUserProfileResponse_status' - The status.
--
-- 'creationTime', 'describeUserProfileResponse_creationTime' - The creation time.
--
-- 'userSettings', 'describeUserProfileResponse_userSettings' - A collection of settings.
--
-- 'userProfileName', 'describeUserProfileResponse_userProfileName' - The user profile name.
--
-- 'domainId', 'describeUserProfileResponse_domainId' - The ID of the domain that contains the profile.
--
-- 'userProfileArn', 'describeUserProfileResponse_userProfileArn' - The user profile Amazon Resource Name (ARN).
--
-- 'failureReason', 'describeUserProfileResponse_failureReason' - The failure reason.
--
-- 'homeEfsFileSystemUid', 'describeUserProfileResponse_homeEfsFileSystemUid' - The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
--
-- 'lastModifiedTime', 'describeUserProfileResponse_lastModifiedTime' - The last modified time.
--
-- 'singleSignOnUserIdentifier', 'describeUserProfileResponse_singleSignOnUserIdentifier' - The SSO user identifier.
--
-- 'singleSignOnUserValue', 'describeUserProfileResponse_singleSignOnUserValue' - The SSO user value.
--
-- 'httpStatus', 'describeUserProfileResponse_httpStatus' - The response's http status code.
newDescribeUserProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeUserProfileResponse
newDescribeUserProfileResponse pHttpStatus_ =
  DescribeUserProfileResponse'
    { status = Core.Nothing,
      creationTime = Core.Nothing,
      userSettings = Core.Nothing,
      userProfileName = Core.Nothing,
      domainId = Core.Nothing,
      userProfileArn = Core.Nothing,
      failureReason = Core.Nothing,
      homeEfsFileSystemUid = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      singleSignOnUserIdentifier = Core.Nothing,
      singleSignOnUserValue = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status.
describeUserProfileResponse_status :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe UserProfileStatus)
describeUserProfileResponse_status = Lens.lens (\DescribeUserProfileResponse' {status} -> status) (\s@DescribeUserProfileResponse' {} a -> s {status = a} :: DescribeUserProfileResponse)

-- | The creation time.
describeUserProfileResponse_creationTime :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.UTCTime)
describeUserProfileResponse_creationTime = Lens.lens (\DescribeUserProfileResponse' {creationTime} -> creationTime) (\s@DescribeUserProfileResponse' {} a -> s {creationTime = a} :: DescribeUserProfileResponse) Core.. Lens.mapping Core._Time

-- | A collection of settings.
describeUserProfileResponse_userSettings :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe UserSettings)
describeUserProfileResponse_userSettings = Lens.lens (\DescribeUserProfileResponse' {userSettings} -> userSettings) (\s@DescribeUserProfileResponse' {} a -> s {userSettings = a} :: DescribeUserProfileResponse)

-- | The user profile name.
describeUserProfileResponse_userProfileName :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_userProfileName = Lens.lens (\DescribeUserProfileResponse' {userProfileName} -> userProfileName) (\s@DescribeUserProfileResponse' {} a -> s {userProfileName = a} :: DescribeUserProfileResponse)

-- | The ID of the domain that contains the profile.
describeUserProfileResponse_domainId :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_domainId = Lens.lens (\DescribeUserProfileResponse' {domainId} -> domainId) (\s@DescribeUserProfileResponse' {} a -> s {domainId = a} :: DescribeUserProfileResponse)

-- | The user profile Amazon Resource Name (ARN).
describeUserProfileResponse_userProfileArn :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_userProfileArn = Lens.lens (\DescribeUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@DescribeUserProfileResponse' {} a -> s {userProfileArn = a} :: DescribeUserProfileResponse)

-- | The failure reason.
describeUserProfileResponse_failureReason :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_failureReason = Lens.lens (\DescribeUserProfileResponse' {failureReason} -> failureReason) (\s@DescribeUserProfileResponse' {} a -> s {failureReason = a} :: DescribeUserProfileResponse)

-- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
describeUserProfileResponse_homeEfsFileSystemUid :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_homeEfsFileSystemUid = Lens.lens (\DescribeUserProfileResponse' {homeEfsFileSystemUid} -> homeEfsFileSystemUid) (\s@DescribeUserProfileResponse' {} a -> s {homeEfsFileSystemUid = a} :: DescribeUserProfileResponse)

-- | The last modified time.
describeUserProfileResponse_lastModifiedTime :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.UTCTime)
describeUserProfileResponse_lastModifiedTime = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTime = a} :: DescribeUserProfileResponse) Core.. Lens.mapping Core._Time

-- | The SSO user identifier.
describeUserProfileResponse_singleSignOnUserIdentifier :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_singleSignOnUserIdentifier = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserIdentifier = a} :: DescribeUserProfileResponse)

-- | The SSO user value.
describeUserProfileResponse_singleSignOnUserValue :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_singleSignOnUserValue = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserValue = a} :: DescribeUserProfileResponse)

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Core.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

instance Core.NFData DescribeUserProfileResponse
