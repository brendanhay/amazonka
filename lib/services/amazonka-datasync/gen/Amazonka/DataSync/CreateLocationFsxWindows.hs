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
-- Module      : Amazonka.DataSync.CreateLocationFsxWindows
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for an Amazon FSx for Windows File Server file
-- system.
module Amazonka.DataSync.CreateLocationFsxWindows
  ( -- * Creating a Request
    CreateLocationFsxWindows (..),
    newCreateLocationFsxWindows,

    -- * Request Lenses
    createLocationFsxWindows_domain,
    createLocationFsxWindows_subdirectory,
    createLocationFsxWindows_tags,
    createLocationFsxWindows_fsxFilesystemArn,
    createLocationFsxWindows_securityGroupArns,
    createLocationFsxWindows_user,
    createLocationFsxWindows_password,

    -- * Destructuring the Response
    CreateLocationFsxWindowsResponse (..),
    newCreateLocationFsxWindowsResponse,

    -- * Response Lenses
    createLocationFsxWindowsResponse_locationArn,
    createLocationFsxWindowsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateLocationFsxWindows' smart constructor.
data CreateLocationFsxWindows = CreateLocationFsxWindows'
  { -- | Specifies the name of the Windows domain that the FSx for Windows File
    -- Server belongs to.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specifies a mount path for your file system using forward slashes. This
    -- is where DataSync reads or writes data (depending on if this is a source
    -- or destination location).
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | Specifies labels that help you categorize, filter, and search for your
    -- Amazon Web Services resources. We recommend creating at least a name tag
    -- for your location.
    tags :: Prelude.Maybe [TagListEntry],
    -- | Specifies the Amazon Resource Name (ARN) for the FSx for Windows File
    -- Server file system.
    fsxFilesystemArn :: Prelude.Text,
    -- | Specifies the ARNs of the security groups that provide access to your
    -- file system\'s preferred subnet.
    --
    -- If you choose a security group that doesn\'t allow connections from
    -- within itself, do one of the following:
    --
    -- -   Configure the security group to allow it to communicate within
    --     itself.
    --
    -- -   Choose a different security group that can communicate with the
    --     mount target\'s security group.
    securityGroupArns :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies the user who has the permissions to access files and folders
    -- in the file system.
    --
    -- For information about choosing a user name that ensures sufficient
    -- permissions to files, folders, and metadata, see
    -- <create-fsx-location.html#FSxWuser user>.
    user :: Prelude.Text,
    -- | Specifies the password of the user who has the permissions to access
    -- files and folders in the file system.
    password :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationFsxWindows' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createLocationFsxWindows_domain' - Specifies the name of the Windows domain that the FSx for Windows File
-- Server belongs to.
--
-- 'subdirectory', 'createLocationFsxWindows_subdirectory' - Specifies a mount path for your file system using forward slashes. This
-- is where DataSync reads or writes data (depending on if this is a source
-- or destination location).
--
-- 'tags', 'createLocationFsxWindows_tags' - Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources. We recommend creating at least a name tag
-- for your location.
--
-- 'fsxFilesystemArn', 'createLocationFsxWindows_fsxFilesystemArn' - Specifies the Amazon Resource Name (ARN) for the FSx for Windows File
-- Server file system.
--
-- 'securityGroupArns', 'createLocationFsxWindows_securityGroupArns' - Specifies the ARNs of the security groups that provide access to your
-- file system\'s preferred subnet.
--
-- If you choose a security group that doesn\'t allow connections from
-- within itself, do one of the following:
--
-- -   Configure the security group to allow it to communicate within
--     itself.
--
-- -   Choose a different security group that can communicate with the
--     mount target\'s security group.
--
-- 'user', 'createLocationFsxWindows_user' - Specifies the user who has the permissions to access files and folders
-- in the file system.
--
-- For information about choosing a user name that ensures sufficient
-- permissions to files, folders, and metadata, see
-- <create-fsx-location.html#FSxWuser user>.
--
-- 'password', 'createLocationFsxWindows_password' - Specifies the password of the user who has the permissions to access
-- files and folders in the file system.
newCreateLocationFsxWindows ::
  -- | 'fsxFilesystemArn'
  Prelude.Text ->
  -- | 'securityGroupArns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'user'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  CreateLocationFsxWindows
newCreateLocationFsxWindows
  pFsxFilesystemArn_
  pSecurityGroupArns_
  pUser_
  pPassword_ =
    CreateLocationFsxWindows'
      { domain = Prelude.Nothing,
        subdirectory = Prelude.Nothing,
        tags = Prelude.Nothing,
        fsxFilesystemArn = pFsxFilesystemArn_,
        securityGroupArns =
          Lens.coerced Lens.# pSecurityGroupArns_,
        user = pUser_,
        password = Data._Sensitive Lens.# pPassword_
      }

-- | Specifies the name of the Windows domain that the FSx for Windows File
-- Server belongs to.
createLocationFsxWindows_domain :: Lens.Lens' CreateLocationFsxWindows (Prelude.Maybe Prelude.Text)
createLocationFsxWindows_domain = Lens.lens (\CreateLocationFsxWindows' {domain} -> domain) (\s@CreateLocationFsxWindows' {} a -> s {domain = a} :: CreateLocationFsxWindows)

-- | Specifies a mount path for your file system using forward slashes. This
-- is where DataSync reads or writes data (depending on if this is a source
-- or destination location).
createLocationFsxWindows_subdirectory :: Lens.Lens' CreateLocationFsxWindows (Prelude.Maybe Prelude.Text)
createLocationFsxWindows_subdirectory = Lens.lens (\CreateLocationFsxWindows' {subdirectory} -> subdirectory) (\s@CreateLocationFsxWindows' {} a -> s {subdirectory = a} :: CreateLocationFsxWindows)

-- | Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources. We recommend creating at least a name tag
-- for your location.
createLocationFsxWindows_tags :: Lens.Lens' CreateLocationFsxWindows (Prelude.Maybe [TagListEntry])
createLocationFsxWindows_tags = Lens.lens (\CreateLocationFsxWindows' {tags} -> tags) (\s@CreateLocationFsxWindows' {} a -> s {tags = a} :: CreateLocationFsxWindows) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Amazon Resource Name (ARN) for the FSx for Windows File
-- Server file system.
createLocationFsxWindows_fsxFilesystemArn :: Lens.Lens' CreateLocationFsxWindows Prelude.Text
createLocationFsxWindows_fsxFilesystemArn = Lens.lens (\CreateLocationFsxWindows' {fsxFilesystemArn} -> fsxFilesystemArn) (\s@CreateLocationFsxWindows' {} a -> s {fsxFilesystemArn = a} :: CreateLocationFsxWindows)

-- | Specifies the ARNs of the security groups that provide access to your
-- file system\'s preferred subnet.
--
-- If you choose a security group that doesn\'t allow connections from
-- within itself, do one of the following:
--
-- -   Configure the security group to allow it to communicate within
--     itself.
--
-- -   Choose a different security group that can communicate with the
--     mount target\'s security group.
createLocationFsxWindows_securityGroupArns :: Lens.Lens' CreateLocationFsxWindows (Prelude.NonEmpty Prelude.Text)
createLocationFsxWindows_securityGroupArns = Lens.lens (\CreateLocationFsxWindows' {securityGroupArns} -> securityGroupArns) (\s@CreateLocationFsxWindows' {} a -> s {securityGroupArns = a} :: CreateLocationFsxWindows) Prelude.. Lens.coerced

-- | Specifies the user who has the permissions to access files and folders
-- in the file system.
--
-- For information about choosing a user name that ensures sufficient
-- permissions to files, folders, and metadata, see
-- <create-fsx-location.html#FSxWuser user>.
createLocationFsxWindows_user :: Lens.Lens' CreateLocationFsxWindows Prelude.Text
createLocationFsxWindows_user = Lens.lens (\CreateLocationFsxWindows' {user} -> user) (\s@CreateLocationFsxWindows' {} a -> s {user = a} :: CreateLocationFsxWindows)

-- | Specifies the password of the user who has the permissions to access
-- files and folders in the file system.
createLocationFsxWindows_password :: Lens.Lens' CreateLocationFsxWindows Prelude.Text
createLocationFsxWindows_password = Lens.lens (\CreateLocationFsxWindows' {password} -> password) (\s@CreateLocationFsxWindows' {} a -> s {password = a} :: CreateLocationFsxWindows) Prelude.. Data._Sensitive

instance Core.AWSRequest CreateLocationFsxWindows where
  type
    AWSResponse CreateLocationFsxWindows =
      CreateLocationFsxWindowsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationFsxWindowsResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationFsxWindows where
  hashWithSalt _salt CreateLocationFsxWindows' {..} =
    _salt `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` fsxFilesystemArn
      `Prelude.hashWithSalt` securityGroupArns
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` password

instance Prelude.NFData CreateLocationFsxWindows where
  rnf CreateLocationFsxWindows' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf fsxFilesystemArn
      `Prelude.seq` Prelude.rnf securityGroupArns
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf password

instance Data.ToHeaders CreateLocationFsxWindows where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationFsxWindows" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationFsxWindows where
  toJSON CreateLocationFsxWindows' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("FsxFilesystemArn" Data..= fsxFilesystemArn),
            Prelude.Just
              ("SecurityGroupArns" Data..= securityGroupArns),
            Prelude.Just ("User" Data..= user),
            Prelude.Just ("Password" Data..= password)
          ]
      )

instance Data.ToPath CreateLocationFsxWindows where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationFsxWindows where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateLocationFsxWindowsResponse' smart constructor.
data CreateLocationFsxWindowsResponse = CreateLocationFsxWindowsResponse'
  { -- | The ARN of the FSx for Windows File Server file system location you
    -- created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationFsxWindowsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationFsxWindowsResponse_locationArn' - The ARN of the FSx for Windows File Server file system location you
-- created.
--
-- 'httpStatus', 'createLocationFsxWindowsResponse_httpStatus' - The response's http status code.
newCreateLocationFsxWindowsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationFsxWindowsResponse
newCreateLocationFsxWindowsResponse pHttpStatus_ =
  CreateLocationFsxWindowsResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the FSx for Windows File Server file system location you
-- created.
createLocationFsxWindowsResponse_locationArn :: Lens.Lens' CreateLocationFsxWindowsResponse (Prelude.Maybe Prelude.Text)
createLocationFsxWindowsResponse_locationArn = Lens.lens (\CreateLocationFsxWindowsResponse' {locationArn} -> locationArn) (\s@CreateLocationFsxWindowsResponse' {} a -> s {locationArn = a} :: CreateLocationFsxWindowsResponse)

-- | The response's http status code.
createLocationFsxWindowsResponse_httpStatus :: Lens.Lens' CreateLocationFsxWindowsResponse Prelude.Int
createLocationFsxWindowsResponse_httpStatus = Lens.lens (\CreateLocationFsxWindowsResponse' {httpStatus} -> httpStatus) (\s@CreateLocationFsxWindowsResponse' {} a -> s {httpStatus = a} :: CreateLocationFsxWindowsResponse)

instance
  Prelude.NFData
    CreateLocationFsxWindowsResponse
  where
  rnf CreateLocationFsxWindowsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
