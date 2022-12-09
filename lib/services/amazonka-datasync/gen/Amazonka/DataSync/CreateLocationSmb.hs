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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a file system on a Server Message Block (SMB) server that can be
-- read from or written to.
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
  { -- | The name of the Windows domain that the SMB server belongs to.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The mount options used by DataSync to access the SMB server.
    mountOptions :: Prelude.Maybe SmbMountOptions,
    -- | The key-value pair that represents the tag that you want to add to the
    -- location. The value can be an empty string. We recommend using tags to
    -- name your resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The subdirectory in the SMB file system that is used to read data from
    -- the SMB source location or write data to the SMB destination. The SMB
    -- path should be a path that\'s exported by the SMB server, or a
    -- subdirectory of that path. The path should be such that it can be
    -- mounted by other SMB clients in your network.
    --
    -- @Subdirectory@ must be specified with forward slashes. For example,
    -- @\/path\/to\/folder@.
    --
    -- To transfer all the data in the folder you specified, DataSync needs to
    -- have permissions to mount the SMB share, as well as to access all the
    -- data in that share. To ensure this, either ensure that the
    -- user\/password specified belongs to the user who can mount the share,
    -- and who has the appropriate permissions for all of the files and
    -- directories that you want DataSync to access, or use credentials of a
    -- member of the Backup Operators group to mount the share. Doing either
    -- enables the agent to access the data. For the agent to access
    -- directories, you must additionally enable all execute access.
    subdirectory :: Prelude.Text,
    -- | The name of the SMB server. This value is the IP address or Domain Name
    -- Service (DNS) name of the SMB server. An agent that is installed
    -- on-premises uses this hostname to mount the SMB server in a network.
    --
    -- This name must either be DNS-compliant or must be an IP version 4 (IPv4)
    -- address.
    serverHostname :: Prelude.Text,
    -- | The user who can mount the share, has the permissions to access files
    -- and folders in the SMB share.
    --
    -- For information about choosing a user name that ensures sufficient
    -- permissions to files, folders, and metadata, see the
    -- <create-smb-location.html#SMBuser User setting> for SMB locations.
    user :: Prelude.Text,
    -- | The password of the user who can mount the share, has the permissions to
    -- access files and folders in the SMB share.
    password :: Data.Sensitive Prelude.Text,
    -- | The Amazon Resource Names (ARNs) of agents to use for a Simple Message
    -- Block (SMB) location.
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
-- 'domain', 'createLocationSmb_domain' - The name of the Windows domain that the SMB server belongs to.
--
-- 'mountOptions', 'createLocationSmb_mountOptions' - The mount options used by DataSync to access the SMB server.
--
-- 'tags', 'createLocationSmb_tags' - The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
--
-- 'subdirectory', 'createLocationSmb_subdirectory' - The subdirectory in the SMB file system that is used to read data from
-- the SMB source location or write data to the SMB destination. The SMB
-- path should be a path that\'s exported by the SMB server, or a
-- subdirectory of that path. The path should be such that it can be
-- mounted by other SMB clients in your network.
--
-- @Subdirectory@ must be specified with forward slashes. For example,
-- @\/path\/to\/folder@.
--
-- To transfer all the data in the folder you specified, DataSync needs to
-- have permissions to mount the SMB share, as well as to access all the
-- data in that share. To ensure this, either ensure that the
-- user\/password specified belongs to the user who can mount the share,
-- and who has the appropriate permissions for all of the files and
-- directories that you want DataSync to access, or use credentials of a
-- member of the Backup Operators group to mount the share. Doing either
-- enables the agent to access the data. For the agent to access
-- directories, you must additionally enable all execute access.
--
-- 'serverHostname', 'createLocationSmb_serverHostname' - The name of the SMB server. This value is the IP address or Domain Name
-- Service (DNS) name of the SMB server. An agent that is installed
-- on-premises uses this hostname to mount the SMB server in a network.
--
-- This name must either be DNS-compliant or must be an IP version 4 (IPv4)
-- address.
--
-- 'user', 'createLocationSmb_user' - The user who can mount the share, has the permissions to access files
-- and folders in the SMB share.
--
-- For information about choosing a user name that ensures sufficient
-- permissions to files, folders, and metadata, see the
-- <create-smb-location.html#SMBuser User setting> for SMB locations.
--
-- 'password', 'createLocationSmb_password' - The password of the user who can mount the share, has the permissions to
-- access files and folders in the SMB share.
--
-- 'agentArns', 'createLocationSmb_agentArns' - The Amazon Resource Names (ARNs) of agents to use for a Simple Message
-- Block (SMB) location.
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

-- | The name of the Windows domain that the SMB server belongs to.
createLocationSmb_domain :: Lens.Lens' CreateLocationSmb (Prelude.Maybe Prelude.Text)
createLocationSmb_domain = Lens.lens (\CreateLocationSmb' {domain} -> domain) (\s@CreateLocationSmb' {} a -> s {domain = a} :: CreateLocationSmb)

-- | The mount options used by DataSync to access the SMB server.
createLocationSmb_mountOptions :: Lens.Lens' CreateLocationSmb (Prelude.Maybe SmbMountOptions)
createLocationSmb_mountOptions = Lens.lens (\CreateLocationSmb' {mountOptions} -> mountOptions) (\s@CreateLocationSmb' {} a -> s {mountOptions = a} :: CreateLocationSmb)

-- | The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
createLocationSmb_tags :: Lens.Lens' CreateLocationSmb (Prelude.Maybe [TagListEntry])
createLocationSmb_tags = Lens.lens (\CreateLocationSmb' {tags} -> tags) (\s@CreateLocationSmb' {} a -> s {tags = a} :: CreateLocationSmb) Prelude.. Lens.mapping Lens.coerced

-- | The subdirectory in the SMB file system that is used to read data from
-- the SMB source location or write data to the SMB destination. The SMB
-- path should be a path that\'s exported by the SMB server, or a
-- subdirectory of that path. The path should be such that it can be
-- mounted by other SMB clients in your network.
--
-- @Subdirectory@ must be specified with forward slashes. For example,
-- @\/path\/to\/folder@.
--
-- To transfer all the data in the folder you specified, DataSync needs to
-- have permissions to mount the SMB share, as well as to access all the
-- data in that share. To ensure this, either ensure that the
-- user\/password specified belongs to the user who can mount the share,
-- and who has the appropriate permissions for all of the files and
-- directories that you want DataSync to access, or use credentials of a
-- member of the Backup Operators group to mount the share. Doing either
-- enables the agent to access the data. For the agent to access
-- directories, you must additionally enable all execute access.
createLocationSmb_subdirectory :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_subdirectory = Lens.lens (\CreateLocationSmb' {subdirectory} -> subdirectory) (\s@CreateLocationSmb' {} a -> s {subdirectory = a} :: CreateLocationSmb)

-- | The name of the SMB server. This value is the IP address or Domain Name
-- Service (DNS) name of the SMB server. An agent that is installed
-- on-premises uses this hostname to mount the SMB server in a network.
--
-- This name must either be DNS-compliant or must be an IP version 4 (IPv4)
-- address.
createLocationSmb_serverHostname :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_serverHostname = Lens.lens (\CreateLocationSmb' {serverHostname} -> serverHostname) (\s@CreateLocationSmb' {} a -> s {serverHostname = a} :: CreateLocationSmb)

-- | The user who can mount the share, has the permissions to access files
-- and folders in the SMB share.
--
-- For information about choosing a user name that ensures sufficient
-- permissions to files, folders, and metadata, see the
-- <create-smb-location.html#SMBuser User setting> for SMB locations.
createLocationSmb_user :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_user = Lens.lens (\CreateLocationSmb' {user} -> user) (\s@CreateLocationSmb' {} a -> s {user = a} :: CreateLocationSmb)

-- | The password of the user who can mount the share, has the permissions to
-- access files and folders in the SMB share.
createLocationSmb_password :: Lens.Lens' CreateLocationSmb Prelude.Text
createLocationSmb_password = Lens.lens (\CreateLocationSmb' {password} -> password) (\s@CreateLocationSmb' {} a -> s {password = a} :: CreateLocationSmb) Prelude.. Data._Sensitive

-- | The Amazon Resource Names (ARNs) of agents to use for a Simple Message
-- Block (SMB) location.
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
    _salt `Prelude.hashWithSalt` domain
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
  { -- | The Amazon Resource Name (ARN) of the source SMB file system location
    -- that is created.
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
-- 'locationArn', 'createLocationSmbResponse_locationArn' - The Amazon Resource Name (ARN) of the source SMB file system location
-- that is created.
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

-- | The Amazon Resource Name (ARN) of the source SMB file system location
-- that is created.
createLocationSmbResponse_locationArn :: Lens.Lens' CreateLocationSmbResponse (Prelude.Maybe Prelude.Text)
createLocationSmbResponse_locationArn = Lens.lens (\CreateLocationSmbResponse' {locationArn} -> locationArn) (\s@CreateLocationSmbResponse' {} a -> s {locationArn = a} :: CreateLocationSmbResponse)

-- | The response's http status code.
createLocationSmbResponse_httpStatus :: Lens.Lens' CreateLocationSmbResponse Prelude.Int
createLocationSmbResponse_httpStatus = Lens.lens (\CreateLocationSmbResponse' {httpStatus} -> httpStatus) (\s@CreateLocationSmbResponse' {} a -> s {httpStatus = a} :: CreateLocationSmbResponse)

instance Prelude.NFData CreateLocationSmbResponse where
  rnf CreateLocationSmbResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
