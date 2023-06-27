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
-- Module      : Amazonka.DataSync.CreateLocationSmb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a Server Message Block (SMB) file server that
-- DataSync can access for a transfer. For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html Creating an SMB location>.
module Amazonka.DataSync.CreateLocationSmb
  ( -- * Creating a Request
    CreateLocationSmb (..),
    newCreateLocationSmb,

    -- * Request Lenses
    createLocationSmb_domain,
    createLocationSmb_mountOptions,
    createLocationSmb_tags,
    createLocationSmb_subdirectory,
    createLocationSmb_serverHostname,
    createLocationSmb_user,
    createLocationSmb_password,
    createLocationSmb_agentArns,

    -- * Destructuring the Response
    CreateLocationSmbResponse (..),
    newCreateLocationSmbResponse,

    -- * Response Lenses
    createLocationSmbResponse_locationArn,
    createLocationSmbResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateLocationSmbRequest
--
-- /See:/ 'newCreateLocationSmb' smart constructor.
data CreateLocationSmb = CreateLocationSmb'
  { -- | Specifies the Windows domain name that your SMB file server belongs to.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
    -- for SMB locations.
    domain :: Prelude.Maybe Prelude.Text,
    -- | Specifies the version of the SMB protocol that DataSync uses to access
    -- your SMB file server.
    mountOptions :: Prelude.Maybe SmbMountOptions,
    -- | Specifies labels that help you categorize, filter, and search for your
    -- Amazon Web Services resources. We recommend creating at least a name tag
    -- for your location.
    tags :: Prelude.Maybe [TagListEntry],
    -- | Specifies the name of the share exported by your SMB file server where
    -- DataSync will read or write data. You can include a subdirectory in the
    -- share path (for example, @\/path\/to\/subdirectory@). Make sure that
    -- other SMB clients in your network can also mount this path.
    --
    -- To copy all data in the specified subdirectory, DataSync must be able to
    -- mount the SMB share and access all of its data. For more information,
    -- see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
    -- for SMB locations.
    subdirectory :: Prelude.Text,
    -- | Specifies the Domain Name Service (DNS) name or IP address of the SMB
    -- file server that your DataSync agent will mount.
    --
    -- You can\'t specify an IP version 6 (IPv6) address.
    serverHostname :: Prelude.Text,
    -- | Specifies the user name that can mount your SMB file server and has
    -- permission to access the files and folders involved in your transfer.
    --
    -- For information about choosing a user with the right level of access for
    -- your transfer, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
    -- for SMB locations.
    user :: Prelude.Text,
    -- | Specifies the password of the user who can mount your SMB file server
    -- and has permission to access the files and folders involved in your
    -- transfer.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
    -- for SMB locations.
    password :: Data.Sensitive Prelude.Text,
    -- | Specifies the DataSync agent (or agents) which you want to connect to
    -- your SMB file server. You specify an agent by using its Amazon Resource
    -- Name (ARN).
    agentArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationSmb' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'domain', 'createLocationSmb_domain' - Specifies the Windows domain name that your SMB file server belongs to.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
--
-- 'mountOptions', 'createLocationSmb_mountOptions' - Specifies the version of the SMB protocol that DataSync uses to access
-- your SMB file server.
--
-- 'tags', 'createLocationSmb_tags' - Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources. We recommend creating at least a name tag
-- for your location.
--
-- 'subdirectory', 'createLocationSmb_subdirectory' - Specifies the name of the share exported by your SMB file server where
-- DataSync will read or write data. You can include a subdirectory in the
-- share path (for example, @\/path\/to\/subdirectory@). Make sure that
-- other SMB clients in your network can also mount this path.
--
-- To copy all data in the specified subdirectory, DataSync must be able to
-- mount the SMB share and access all of its data. For more information,
-- see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
--
-- 'serverHostname', 'createLocationSmb_serverHostname' - Specifies the Domain Name Service (DNS) name or IP address of the SMB
-- file server that your DataSync agent will mount.
--
-- You can\'t specify an IP version 6 (IPv6) address.
--
-- 'user', 'createLocationSmb_user' - Specifies the user name that can mount your SMB file server and has
-- permission to access the files and folders involved in your transfer.
--
-- For information about choosing a user with the right level of access for
-- your transfer, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
--
-- 'password', 'createLocationSmb_password' - Specifies the password of the user who can mount your SMB file server
-- and has permission to access the files and folders involved in your
-- transfer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
--
-- 'agentArns', 'createLocationSmb_agentArns' - Specifies the DataSync agent (or agents) which you want to connect to
-- your SMB file server. You specify an agent by using its Amazon Resource
-- Name (ARN).
newCreateLocationSmb ::
  -- | 'subdirectory'
  Prelude.Text ->
  -- | 'serverHostname'
  Prelude.Text ->
  -- | 'user'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  -- | 'agentArns'
  Prelude.NonEmpty Prelude.Text ->
  CreateLocationSmb
newCreateLocationSmb
  pSubdirectory_
  pServerHostname_
  pUser_
  pPassword_
  pAgentArns_ =
    CreateLocationSmb'
      { domain = Prelude.Nothing,
        mountOptions = Prelude.Nothing,
        tags = Prelude.Nothing,
        subdirectory = pSubdirectory_,
        serverHostname = pServerHostname_,
        user = pUser_,
        password = Data._Sensitive Lens.# pPassword_,
        agentArns = Lens.coerced Lens.# pAgentArns_
      }

-- | Specifies the Windows domain name that your SMB file server belongs to.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
createLocationSmb_domain :: Lens.Lens' CreateLocationSmb (Prelude.Maybe Prelude.Text)
createLocationSmb_domain = Lens.lens (\CreateLocationSmb' {domain} -> domain) (\s@CreateLocationSmb' {} a -> s {domain = a} :: CreateLocationSmb)

-- | Specifies the version of the SMB protocol that DataSync uses to access
-- your SMB file server.
createLocationSmb_mountOptions :: Lens.Lens' CreateLocationSmb (Prelude.Maybe SmbMountOptions)
createLocationSmb_mountOptions = Lens.lens (\CreateLocationSmb' {mountOptions} -> mountOptions) (\s@CreateLocationSmb' {} a -> s {mountOptions = a} :: CreateLocationSmb)

-- | Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources. We recommend creating at least a name tag
-- for your location.
createLocationSmb_tags :: Lens.Lens' CreateLocationSmb (Prelude.Maybe [TagListEntry])
createLocationSmb_tags = Lens.lens (\CreateLocationSmb' {tags} -> tags) (\s@CreateLocationSmb' {} a -> s {tags = a} :: CreateLocationSmb) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the name of the share exported by your SMB file server where
-- DataSync will read or write data. You can include a subdirectory in the
-- share path (for example, @\/path\/to\/subdirectory@). Make sure that
-- other SMB clients in your network can also mount this path.
--
-- To copy all data in the specified subdirectory, DataSync must be able to
-- mount the SMB share and access all of its data. For more information,
-- see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
createLocationSmb_subdirectory :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_subdirectory = Lens.lens (\CreateLocationSmb' {subdirectory} -> subdirectory) (\s@CreateLocationSmb' {} a -> s {subdirectory = a} :: CreateLocationSmb)

-- | Specifies the Domain Name Service (DNS) name or IP address of the SMB
-- file server that your DataSync agent will mount.
--
-- You can\'t specify an IP version 6 (IPv6) address.
createLocationSmb_serverHostname :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_serverHostname = Lens.lens (\CreateLocationSmb' {serverHostname} -> serverHostname) (\s@CreateLocationSmb' {} a -> s {serverHostname = a} :: CreateLocationSmb)

-- | Specifies the user name that can mount your SMB file server and has
-- permission to access the files and folders involved in your transfer.
--
-- For information about choosing a user with the right level of access for
-- your transfer, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
createLocationSmb_user :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_user = Lens.lens (\CreateLocationSmb' {user} -> user) (\s@CreateLocationSmb' {} a -> s {user = a} :: CreateLocationSmb)

-- | Specifies the password of the user who can mount your SMB file server
-- and has permission to access the files and folders involved in your
-- transfer.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html#configuring-smb-permissions required permissions>
-- for SMB locations.
createLocationSmb_password :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_password = Lens.lens (\CreateLocationSmb' {password} -> password) (\s@CreateLocationSmb' {} a -> s {password = a} :: CreateLocationSmb) Prelude.. Data._Sensitive

-- | Specifies the DataSync agent (or agents) which you want to connect to
-- your SMB file server. You specify an agent by using its Amazon Resource
-- Name (ARN).
createLocationSmb_agentArns :: Lens.Lens' CreateLocationSmb (Prelude.NonEmpty Prelude.Text)
createLocationSmb_agentArns = Lens.lens (\CreateLocationSmb' {agentArns} -> agentArns) (\s@CreateLocationSmb' {} a -> s {agentArns = a} :: CreateLocationSmb) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLocationSmb where
  type
    AWSResponse CreateLocationSmb =
      CreateLocationSmbResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationSmbResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationSmb where
  hashWithSalt _salt CreateLocationSmb' {..} =
    _salt
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` mountOptions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` serverHostname
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` agentArns

instance Prelude.NFData CreateLocationSmb where
  rnf CreateLocationSmb' {..} =
    Prelude.rnf domain
      `Prelude.seq` Prelude.rnf mountOptions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf serverHostname
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf agentArns

instance Data.ToHeaders CreateLocationSmb where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationSmb" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationSmb where
  toJSON CreateLocationSmb' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Domain" Data..=) Prelude.<$> domain,
            ("MountOptions" Data..=) Prelude.<$> mountOptions,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Subdirectory" Data..= subdirectory),
            Prelude.Just
              ("ServerHostname" Data..= serverHostname),
            Prelude.Just ("User" Data..= user),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("AgentArns" Data..= agentArns)
          ]
      )

instance Data.ToPath CreateLocationSmb where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationSmb where
  toQuery = Prelude.const Prelude.mempty

-- | CreateLocationSmbResponse
--
-- /See:/ 'newCreateLocationSmbResponse' smart constructor.
data CreateLocationSmbResponse = CreateLocationSmbResponse'
  { -- | The ARN of the SMB location that you created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationSmbResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationSmbResponse_locationArn' - The ARN of the SMB location that you created.
--
-- 'httpStatus', 'createLocationSmbResponse_httpStatus' - The response's http status code.
newCreateLocationSmbResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationSmbResponse
newCreateLocationSmbResponse pHttpStatus_ =
  CreateLocationSmbResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the SMB location that you created.
createLocationSmbResponse_locationArn :: Lens.Lens' CreateLocationSmbResponse (Prelude.Maybe Prelude.Text)
createLocationSmbResponse_locationArn = Lens.lens (\CreateLocationSmbResponse' {locationArn} -> locationArn) (\s@CreateLocationSmbResponse' {} a -> s {locationArn = a} :: CreateLocationSmbResponse)

-- | The response's http status code.
createLocationSmbResponse_httpStatus :: Lens.Lens' CreateLocationSmbResponse Prelude.Int
createLocationSmbResponse_httpStatus = Lens.lens (\CreateLocationSmbResponse' {httpStatus} -> httpStatus) (\s@CreateLocationSmbResponse' {} a -> s {httpStatus = a} :: CreateLocationSmbResponse)

instance Prelude.NFData CreateLocationSmbResponse where
  rnf CreateLocationSmbResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
