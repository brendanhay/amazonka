{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The domain ID.
    domainId :: Prelude.Text,
    -- | The user profile name.
    userProfileName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

-- | The user profile name.
describeUserProfile_userProfileName :: Lens.Lens' DescribeUserProfile Prelude.Text
describeUserProfile_userProfileName = Lens.lens (\DescribeUserProfile' {userProfileName} -> userProfileName) (\s@DescribeUserProfile' {} a -> s {userProfileName = a} :: DescribeUserProfile)

instance Prelude.AWSRequest DescribeUserProfile where
  type
    Rs DescribeUserProfile =
      DescribeUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Prelude.<$> (x Prelude..?> "Status")
            Prelude.<*> (x Prelude..?> "CreationTime")
            Prelude.<*> (x Prelude..?> "UserSettings")
            Prelude.<*> (x Prelude..?> "UserProfileName")
            Prelude.<*> (x Prelude..?> "DomainId")
            Prelude.<*> (x Prelude..?> "UserProfileArn")
            Prelude.<*> (x Prelude..?> "FailureReason")
            Prelude.<*> (x Prelude..?> "HomeEfsFileSystemUid")
            Prelude.<*> (x Prelude..?> "LastModifiedTime")
            Prelude.<*> (x Prelude..?> "SingleSignOnUserIdentifier")
            Prelude.<*> (x Prelude..?> "SingleSignOnUserValue")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeUserProfile

instance Prelude.NFData DescribeUserProfile

instance Prelude.ToHeaders DescribeUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "SageMaker.DescribeUserProfile" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("DomainId" Prelude..= domainId),
            Prelude.Just
              ("UserProfileName" Prelude..= userProfileName)
          ]
      )

instance Prelude.ToPath DescribeUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The status.
    status :: Prelude.Maybe UserProfileStatus,
    -- | The creation time.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | A collection of settings.
    userSettings :: Prelude.Maybe UserSettings,
    -- | The user profile name.
    userProfileName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the domain that contains the profile.
    domainId :: Prelude.Maybe Prelude.Text,
    -- | The user profile Amazon Resource Name (ARN).
    userProfileArn :: Prelude.Maybe Prelude.Text,
    -- | The failure reason.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
    -- volume.
    homeEfsFileSystemUid :: Prelude.Maybe Prelude.Text,
    -- | The last modified time.
    lastModifiedTime :: Prelude.Maybe Prelude.POSIX,
    -- | The SSO user identifier.
    singleSignOnUserIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The SSO user value.
    singleSignOnUserValue :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeUserProfileResponse
newDescribeUserProfileResponse pHttpStatus_ =
  DescribeUserProfileResponse'
    { status =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      userSettings = Prelude.Nothing,
      userProfileName = Prelude.Nothing,
      domainId = Prelude.Nothing,
      userProfileArn = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      homeEfsFileSystemUid = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      singleSignOnUserIdentifier = Prelude.Nothing,
      singleSignOnUserValue = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The status.
describeUserProfileResponse_status :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe UserProfileStatus)
describeUserProfileResponse_status = Lens.lens (\DescribeUserProfileResponse' {status} -> status) (\s@DescribeUserProfileResponse' {} a -> s {status = a} :: DescribeUserProfileResponse)

-- | The creation time.
describeUserProfileResponse_creationTime :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeUserProfileResponse_creationTime = Lens.lens (\DescribeUserProfileResponse' {creationTime} -> creationTime) (\s@DescribeUserProfileResponse' {} a -> s {creationTime = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Prelude._Time

-- | A collection of settings.
describeUserProfileResponse_userSettings :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe UserSettings)
describeUserProfileResponse_userSettings = Lens.lens (\DescribeUserProfileResponse' {userSettings} -> userSettings) (\s@DescribeUserProfileResponse' {} a -> s {userSettings = a} :: DescribeUserProfileResponse)

-- | The user profile name.
describeUserProfileResponse_userProfileName :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_userProfileName = Lens.lens (\DescribeUserProfileResponse' {userProfileName} -> userProfileName) (\s@DescribeUserProfileResponse' {} a -> s {userProfileName = a} :: DescribeUserProfileResponse)

-- | The ID of the domain that contains the profile.
describeUserProfileResponse_domainId :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_domainId = Lens.lens (\DescribeUserProfileResponse' {domainId} -> domainId) (\s@DescribeUserProfileResponse' {} a -> s {domainId = a} :: DescribeUserProfileResponse)

-- | The user profile Amazon Resource Name (ARN).
describeUserProfileResponse_userProfileArn :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_userProfileArn = Lens.lens (\DescribeUserProfileResponse' {userProfileArn} -> userProfileArn) (\s@DescribeUserProfileResponse' {} a -> s {userProfileArn = a} :: DescribeUserProfileResponse)

-- | The failure reason.
describeUserProfileResponse_failureReason :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_failureReason = Lens.lens (\DescribeUserProfileResponse' {failureReason} -> failureReason) (\s@DescribeUserProfileResponse' {} a -> s {failureReason = a} :: DescribeUserProfileResponse)

-- | The ID of the user\'s profile in the Amazon Elastic File System (EFS)
-- volume.
describeUserProfileResponse_homeEfsFileSystemUid :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_homeEfsFileSystemUid = Lens.lens (\DescribeUserProfileResponse' {homeEfsFileSystemUid} -> homeEfsFileSystemUid) (\s@DescribeUserProfileResponse' {} a -> s {homeEfsFileSystemUid = a} :: DescribeUserProfileResponse)

-- | The last modified time.
describeUserProfileResponse_lastModifiedTime :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.UTCTime)
describeUserProfileResponse_lastModifiedTime = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTime = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Prelude._Time

-- | The SSO user identifier.
describeUserProfileResponse_singleSignOnUserIdentifier :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_singleSignOnUserIdentifier = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserIdentifier} -> singleSignOnUserIdentifier) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserIdentifier = a} :: DescribeUserProfileResponse)

-- | The SSO user value.
describeUserProfileResponse_singleSignOnUserValue :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_singleSignOnUserValue = Lens.lens (\DescribeUserProfileResponse' {singleSignOnUserValue} -> singleSignOnUserValue) (\s@DescribeUserProfileResponse' {} a -> s {singleSignOnUserValue = a} :: DescribeUserProfileResponse)

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Prelude.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

instance Prelude.NFData DescribeUserProfileResponse
