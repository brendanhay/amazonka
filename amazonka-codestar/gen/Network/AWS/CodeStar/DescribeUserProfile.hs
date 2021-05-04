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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Text
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
-- 'userArn', 'describeUserProfile_userArn' - The Amazon Resource Name (ARN) of the user.
newDescribeUserProfile ::
  -- | 'userArn'
  Prelude.Text ->
  DescribeUserProfile
newDescribeUserProfile pUserArn_ =
  DescribeUserProfile' {userArn = pUserArn_}

-- | The Amazon Resource Name (ARN) of the user.
describeUserProfile_userArn :: Lens.Lens' DescribeUserProfile Prelude.Text
describeUserProfile_userArn = Lens.lens (\DescribeUserProfile' {userArn} -> userArn) (\s@DescribeUserProfile' {} a -> s {userArn = a} :: DescribeUserProfile)

instance Prelude.AWSRequest DescribeUserProfile where
  type
    Rs DescribeUserProfile =
      DescribeUserProfileResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeUserProfileResponse'
            Prelude.<$> (x Prelude..?> "sshPublicKey")
            Prelude.<*> (x Prelude..?> "displayName")
            Prelude.<*> (x Prelude..?> "emailAddress")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "userArn")
            Prelude.<*> (x Prelude..:> "createdTimestamp")
            Prelude.<*> (x Prelude..:> "lastModifiedTimestamp")
      )

instance Prelude.Hashable DescribeUserProfile

instance Prelude.NFData DescribeUserProfile

instance Prelude.ToHeaders DescribeUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "CodeStar_20170419.DescribeUserProfile" ::
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
          [Prelude.Just ("userArn" Prelude..= userArn)]
      )

instance Prelude.ToPath DescribeUserProfile where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The SSH public key associated with the user. This SSH public key is
    -- associated with the user profile, and can be used in conjunction with
    -- the associated private key for access to project resources, such as
    -- Amazon EC2 instances, if a project owner grants remote access to those
    -- resources.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
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
    displayName :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The email address for the user. Optional.
    emailAddress :: Prelude.Maybe (Prelude.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Text,
    -- | The date and time when the user profile was created in AWS CodeStar, in
    -- timestamp format.
    createdTimestamp :: Prelude.POSIX,
    -- | The date and time when the user profile was last modified, in timestamp
    -- format.
    lastModifiedTimestamp :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'userArn'
  Prelude.Text ->
  -- | 'createdTimestamp'
  Prelude.UTCTime ->
  -- | 'lastModifiedTimestamp'
  Prelude.UTCTime ->
  DescribeUserProfileResponse
newDescribeUserProfileResponse
  pHttpStatus_
  pUserArn_
  pCreatedTimestamp_
  pLastModifiedTimestamp_ =
    DescribeUserProfileResponse'
      { sshPublicKey =
          Prelude.Nothing,
        displayName = Prelude.Nothing,
        emailAddress = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        userArn = pUserArn_,
        createdTimestamp =
          Prelude._Time Lens.# pCreatedTimestamp_,
        lastModifiedTimestamp =
          Prelude._Time Lens.# pLastModifiedTimestamp_
      }

-- | The SSH public key associated with the user. This SSH public key is
-- associated with the user profile, and can be used in conjunction with
-- the associated private key for access to project resources, such as
-- Amazon EC2 instances, if a project owner grants remote access to those
-- resources.
describeUserProfileResponse_sshPublicKey :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
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
describeUserProfileResponse_displayName :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_displayName = Lens.lens (\DescribeUserProfileResponse' {displayName} -> displayName) (\s@DescribeUserProfileResponse' {} a -> s {displayName = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The email address for the user. Optional.
describeUserProfileResponse_emailAddress :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_emailAddress = Lens.lens (\DescribeUserProfileResponse' {emailAddress} -> emailAddress) (\s@DescribeUserProfileResponse' {} a -> s {emailAddress = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Prelude._Sensitive

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Prelude.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user.
describeUserProfileResponse_userArn :: Lens.Lens' DescribeUserProfileResponse Prelude.Text
describeUserProfileResponse_userArn = Lens.lens (\DescribeUserProfileResponse' {userArn} -> userArn) (\s@DescribeUserProfileResponse' {} a -> s {userArn = a} :: DescribeUserProfileResponse)

-- | The date and time when the user profile was created in AWS CodeStar, in
-- timestamp format.
describeUserProfileResponse_createdTimestamp :: Lens.Lens' DescribeUserProfileResponse Prelude.UTCTime
describeUserProfileResponse_createdTimestamp = Lens.lens (\DescribeUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@DescribeUserProfileResponse' {} a -> s {createdTimestamp = a} :: DescribeUserProfileResponse) Prelude.. Prelude._Time

-- | The date and time when the user profile was last modified, in timestamp
-- format.
describeUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' DescribeUserProfileResponse Prelude.UTCTime
describeUserProfileResponse_lastModifiedTimestamp = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeUserProfileResponse) Prelude.. Prelude._Time

instance Prelude.NFData DescribeUserProfileResponse
