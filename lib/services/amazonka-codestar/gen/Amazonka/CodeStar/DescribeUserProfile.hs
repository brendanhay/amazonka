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
-- Module      : Amazonka.CodeStar.DescribeUserProfile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a user in AWS CodeStar and the user attributes across all
-- projects.
module Amazonka.CodeStar.DescribeUserProfile
  ( -- * Creating a Request
    DescribeUserProfile (..),
    newDescribeUserProfile,

    -- * Request Lenses
    describeUserProfile_userArn,

    -- * Destructuring the Response
    DescribeUserProfileResponse (..),
    newDescribeUserProfileResponse,

    -- * Response Lenses
    describeUserProfileResponse_displayName,
    describeUserProfileResponse_emailAddress,
    describeUserProfileResponse_sshPublicKey,
    describeUserProfileResponse_httpStatus,
    describeUserProfileResponse_userArn,
    describeUserProfileResponse_createdTimestamp,
    describeUserProfileResponse_lastModifiedTimestamp,
  )
where

import Amazonka.CodeStar.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeUserProfile' smart constructor.
data DescribeUserProfile = DescribeUserProfile'
  { -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Text
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
            Prelude.<$> (x Data..?> "displayName")
            Prelude.<*> (x Data..?> "emailAddress")
            Prelude.<*> (x Data..?> "sshPublicKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "userArn")
            Prelude.<*> (x Data..:> "createdTimestamp")
            Prelude.<*> (x Data..:> "lastModifiedTimestamp")
      )

instance Prelude.Hashable DescribeUserProfile where
  hashWithSalt _salt DescribeUserProfile' {..} =
    _salt `Prelude.hashWithSalt` userArn

instance Prelude.NFData DescribeUserProfile where
  rnf DescribeUserProfile' {..} = Prelude.rnf userArn

instance Data.ToHeaders DescribeUserProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "CodeStar_20170419.DescribeUserProfile" ::
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
          [Prelude.Just ("userArn" Data..= userArn)]
      )

instance Data.ToPath DescribeUserProfile where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeUserProfile where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeUserProfileResponse' smart constructor.
data DescribeUserProfileResponse = DescribeUserProfileResponse'
  { -- | The display name shown for the user in AWS CodeStar projects. For
    -- example, this could be set to both first and last name (\"Mary Major\")
    -- or a single name (\"Mary\"). The display name is also used to generate
    -- the initial icon associated with the user in AWS CodeStar projects. If
    -- spaces are included in the display name, the first character that
    -- appears after the space will be used as the second character in the user
    -- initial icon. The initial icon displays a maximum of two characters, so
    -- a display name with more than one space (for example \"Mary Jane
    -- Major\") would generate an initial icon using the first character and
    -- the first character after the space (\"MJ\", not \"MM\").
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The email address for the user. Optional.
    emailAddress :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The SSH public key associated with the user. This SSH public key is
    -- associated with the user profile, and can be used in conjunction with
    -- the associated private key for access to project resources, such as
    -- Amazon EC2 instances, if a project owner grants remote access to those
    -- resources.
    sshPublicKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the user.
    userArn :: Prelude.Text,
    -- | The date and time when the user profile was created in AWS CodeStar, in
    -- timestamp format.
    createdTimestamp :: Data.POSIX,
    -- | The date and time when the user profile was last modified, in timestamp
    -- format.
    lastModifiedTimestamp :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeUserProfileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'sshPublicKey', 'describeUserProfileResponse_sshPublicKey' - The SSH public key associated with the user. This SSH public key is
-- associated with the user profile, and can be used in conjunction with
-- the associated private key for access to project resources, such as
-- Amazon EC2 instances, if a project owner grants remote access to those
-- resources.
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
      { displayName =
          Prelude.Nothing,
        emailAddress = Prelude.Nothing,
        sshPublicKey = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        userArn = pUserArn_,
        createdTimestamp =
          Data._Time Lens.# pCreatedTimestamp_,
        lastModifiedTimestamp =
          Data._Time Lens.# pLastModifiedTimestamp_
      }

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
describeUserProfileResponse_displayName = Lens.lens (\DescribeUserProfileResponse' {displayName} -> displayName) (\s@DescribeUserProfileResponse' {} a -> s {displayName = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The email address for the user. Optional.
describeUserProfileResponse_emailAddress :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_emailAddress = Lens.lens (\DescribeUserProfileResponse' {emailAddress} -> emailAddress) (\s@DescribeUserProfileResponse' {} a -> s {emailAddress = a} :: DescribeUserProfileResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The SSH public key associated with the user. This SSH public key is
-- associated with the user profile, and can be used in conjunction with
-- the associated private key for access to project resources, such as
-- Amazon EC2 instances, if a project owner grants remote access to those
-- resources.
describeUserProfileResponse_sshPublicKey :: Lens.Lens' DescribeUserProfileResponse (Prelude.Maybe Prelude.Text)
describeUserProfileResponse_sshPublicKey = Lens.lens (\DescribeUserProfileResponse' {sshPublicKey} -> sshPublicKey) (\s@DescribeUserProfileResponse' {} a -> s {sshPublicKey = a} :: DescribeUserProfileResponse)

-- | The response's http status code.
describeUserProfileResponse_httpStatus :: Lens.Lens' DescribeUserProfileResponse Prelude.Int
describeUserProfileResponse_httpStatus = Lens.lens (\DescribeUserProfileResponse' {httpStatus} -> httpStatus) (\s@DescribeUserProfileResponse' {} a -> s {httpStatus = a} :: DescribeUserProfileResponse)

-- | The Amazon Resource Name (ARN) of the user.
describeUserProfileResponse_userArn :: Lens.Lens' DescribeUserProfileResponse Prelude.Text
describeUserProfileResponse_userArn = Lens.lens (\DescribeUserProfileResponse' {userArn} -> userArn) (\s@DescribeUserProfileResponse' {} a -> s {userArn = a} :: DescribeUserProfileResponse)

-- | The date and time when the user profile was created in AWS CodeStar, in
-- timestamp format.
describeUserProfileResponse_createdTimestamp :: Lens.Lens' DescribeUserProfileResponse Prelude.UTCTime
describeUserProfileResponse_createdTimestamp = Lens.lens (\DescribeUserProfileResponse' {createdTimestamp} -> createdTimestamp) (\s@DescribeUserProfileResponse' {} a -> s {createdTimestamp = a} :: DescribeUserProfileResponse) Prelude.. Data._Time

-- | The date and time when the user profile was last modified, in timestamp
-- format.
describeUserProfileResponse_lastModifiedTimestamp :: Lens.Lens' DescribeUserProfileResponse Prelude.UTCTime
describeUserProfileResponse_lastModifiedTimestamp = Lens.lens (\DescribeUserProfileResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeUserProfileResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeUserProfileResponse) Prelude.. Data._Time

instance Prelude.NFData DescribeUserProfileResponse where
  rnf DescribeUserProfileResponse' {..} =
    Prelude.rnf displayName `Prelude.seq`
      Prelude.rnf emailAddress `Prelude.seq`
        Prelude.rnf sshPublicKey `Prelude.seq`
          Prelude.rnf httpStatus `Prelude.seq`
            Prelude.rnf userArn `Prelude.seq`
              Prelude.rnf createdTimestamp `Prelude.seq`
                Prelude.rnf lastModifiedTimestamp
