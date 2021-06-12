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
-- Module      : Network.AWS.CodeStar.DescribeUserProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user in AWS CodeStar and the user attributes across all
-- projects.
module Network.AWS.CodeStar.DescribeUserProfile
  ( -- * Creating a Request
    DescribeUserProfile (..),
    newDescribeUserProfile,

    -- * Request Lenses
    describeUserProfile_userArn,

    -- * Destructuring the Response
    DescribeUserProfileResponse (..),
    newDescribeUserProfileResponse,

    -- * Response Lenses
    describeUserProfileResponse_sshPublicKey,
    describeUserProfileResponse_displayName,
    describeUserProfileResponse_emailAddress,
    describeUserProfileResponse_httpStatus,
    describeUserProfileResponse_userArn,
    describeUserProfileResponse_createdTimestamp,
    describeUserProfileResponse_lastModifiedTimestamp,
  )
where

import Network.AWS.CodeStar.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Core.Text
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
-- 'userArn', 'describeUserProfile_userArn' - The Amazon Resource Name (ARN) of the user.
newDescribeUserProfile ::
  -- | 'userArn'
  Core.Text ->
  DescribeUserProfile
newDescribeUserProfile pUserArn_ =
  DescribeUserProfile' {userArn = pUserArn_}

-- | The Amazon Resource Name (ARN) of the user.
describeUserProfile_userArn :: Lens.Lens' DescribeUserProfile Core.Text
describeUserProfile_userArn = Lens.lens (\DescribeUserProfile' {userArn} -> userArn) (\s@DescribeUserProfile' {} a -> s {userArn = a} :: DescribeUserProfile)

instance Core.AWSRequest DescribeUserProfile where
  type
    AWSResponse DescribeUserProfile =
      DescribeUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Core.<$> (x Core..?> "sshPublicKey")
            Core.<*> (x Core..?> "displayName")
            Core.<*> (x Core..?> "emailAddress")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "userArn")
            Core.<*> (x Core..:> "createdTimestamp")
            Core.<*> (x Core..:> "lastModifiedTimestamp")
      )

instance Core.Hashable DescribeUserProfile

instance Core.NFData DescribeUserProfile

instance Core.ToHeaders DescribeUserProfile where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "CodeStar_20170419.DescribeUserProfile" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeUserProfile where
  toJSON DescribeUserProfile' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("userArn" Core..= userArn)]
      )

instance Core.ToPath DescribeUserProfile where
  toPath = Core.const "/"

instance Core.ToQuery DescribeUserProfile where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The SSH public key associated with the user. This SSH public key is
    -- associated with the user profile, and can be used in conjunction with
    -- the associated private key for access to project resources, such as
    -- Amazon EC2 instances, if a project owner grants remote access to those
    -- resources.
    sshPublicKey :: Core.Maybe Core.Text,
    -- | The display name shown for the user in AWS CodeStar projects. For
    -- example, this could be set to both first and last name (\"Mary Major\")
    -- or a single name (\"Mary\"). The display name is also used to generate
    -- the initial icon associated with the user in AWS CodeStar projects. If
    -- spaces are included in the display name, the first character that
    -- appears after the space will be used as the second character in the user
    -- initial icon. The initial icon displays a maximum of two characters, so
    -- a display name with more than one space (for example \"Mary Jane
    -- Major\") would generate an initial icon using the first character and
    -- the first character after the space (\"MJ\", not \"MM\").
    displayName :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The email address for the user. Optional.
    emailAddress :: Core.Maybe (Core.Sensitive Core.Text),
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Core.Text,
    -- | The date and time when the user profile was created in AWS CodeStar, in
    -- timestamp format.
    createdTimestamp :: Core.POSIX,
    -- | The date and time when the user profile was last modified, in timestamp
    -- format.
    lastModifiedTimestamp :: Core.POSIX
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sshPublicKey', 'describeUserProfileResponse_sshPublicKey' - The SSH public key associated with the user. This SSH public key is
-- associated with the user profile, and can be used in conjunction with
-- the associated private key for access to project resources, such as
-- Amazon EC2 instances, if a project owner grants remote access to those
-- resources.
--
-- 'displayName', 'describeUserProfileResponse_displayName' - The display name shown for the user in AWS CodeStar projects. For
-- example, this could be set to both first and last name (\"Mary Major\")
-- or a single name (\"Mary\"). The display name is also used to generate
-- the initial icon associated with the user in AWS CodeStar projects. If
-- spaces are included in the display name, the first character that
-- appears after the space will be used as the second character in the user
-- initial icon. The initial icon displays a maximum of two characters, so
-- a display name with more than one space (for example \"Mary Jane
-- Major\") would generate an initial icon using the first character and
-- the first character after the space (\"MJ\", not \"MM\").
--
-- 'emailAddress', 'describeUserProfileResponse_emailAddress' - The email address for the user. Optional.
--
-- 'httpStatus', 'describeUserProfileResponse_httpStatus' - The response's http status code.
--
-- 'userArn', 'describeUserProfileResponse_userArn' - The Amazon Resource Name (ARN) of the user.
--
-- 'createdTimestamp', 'describeUserProfileResponse_createdTimestamp' - The date and time when the user profile was created in AWS CodeStar, in
-- timestamp format.
--
-- 'lastModifiedTimestamp', 'describeUserProfileResponse_lastModifiedTimestamp' - The date and time when the user profile was last modified, in timestamp
-- format.
newDescribeUserProfileResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'userArn'
  Core.Text ->
  -- | 'createdTimestamp'
  Core.UTCTime ->
  -- | 'lastModifiedTimestamp'
  Core.UTCTime ->
  DescribeUserProfileResponse
newDescribeUserProfileResponse
  pHttpStatus_
  pUserArn_
  pCreatedTimestamp_
  pLastModifiedTimestamp_ =
    DescribeUserProfileResponse'
      { sshPublicKey =
          Core.Nothing,
        displayName = Core.Nothing,
        emailAddress = Core.Nothing,
        httpStatus = pHttpStatus_,
        userArn = pUserArn_,
        createdTimestamp =
          Core._Time Lens.# pCreatedTimestamp_,
        lastModifiedTimestamp =
          Core._Time Lens.# pLastModifiedTimestamp_
      }

-- | The SSH public key associated with the user. This SSH public key is
-- associated with the user profile, and can be used in conjunction with
-- the associated private key for access to project resources, such as
-- Amazon EC2 instances, if a project owner grants remote access to those
-- resources.
describeUserProfileResponse_sshPublicKey :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_sshPublicKey = Lens.lens (\DescribeUserProfileResponse' {sshPublicKey} -> sshPublicKey) (\s@DescribeUserProfileResponse' {} a -> s {sshPublicKey = a} :: DescribeUserProfileResponse)

-- | The display name shown for the user in AWS CodeStar projects. For
-- example, this could be set to both first and last name (\"Mary Major\")
-- or a single name (\"Mary\"). The display name is also used to generate
-- the initial icon associated with the user in AWS CodeStar projects. If
-- spaces are included in the display name, the first character that
-- appears after the space will be used as the second character in the user
-- initial icon. The initial icon displays a maximum of two characters, so
-- a display name with more than one space (for example \"Mary Jane
-- Major\") would generate an initial icon using the first character and
-- the first character after the space (\"MJ\", not \"MM\").
describeUserProfileResponse_displayName :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_displayName = Lens.lens (\DescribeUserProfileResponse' {displayName} -> displayName) (\s@DescribeUserProfileResponse' {} a -> s {displayName = a} :: DescribeUserProfileResponse) Core.. Lens.mapping Core._Sensitive

-- | The email address for the user. Optional.
describeUserProfileResponse_emailAddress :: Lens.Lens' DescribeUserProfileResponse (Core.Maybe Core.Text)
describeUserProfileResponse_emailAddress = Lens.lens (\DescribeUserProfileResponse' {emailAddress} -> emailAddress) (\s@DescribeUserProfileResponse' {} a -> s {emailAddress = a} :: DescribeUserProfileResponse) Core.. Lens.mapping Core._Sensitive

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Core.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user.
describeUserProfileResponse_userArn :: Lens.Lens' DescribeUserProfileResponse Core.Text
describeUserProfileResponse_userArn = Lens.lens (\DescribeUserProfileResponse' {userArn} -> userArn) (\s@DescribeUserProfileResponse' {} a -> s {userArn = a} :: DescribeUserProfileResponse)

-- | The date and time when the user profile was created in AWS CodeStar, in
-- timestamp format.
describeUserProfileResponse_createdTimestamp :: Lens.Lens' DescribeUserProfileResponse Core.UTCTime
describeUserProfileResponse_createdTimestamp = Lens.lens (\DescribeUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@DescribeUserProfileResponse' {} a -> s {createdTimestamp = a} :: DescribeUserProfileResponse) Core.. Core._Time

-- | The date and time when the user profile was last modified, in timestamp
-- format.
describeUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' DescribeUserProfileResponse Core.UTCTime
describeUserProfileResponse_lastModifiedTimestamp = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeUserProfileResponse) Core.. Core._Time

instance Core.NFData DescribeUserProfileResponse
