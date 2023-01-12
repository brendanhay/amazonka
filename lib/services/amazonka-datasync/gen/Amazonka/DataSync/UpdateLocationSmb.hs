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
-- Module      : Amazonka.DataSync.UpdateLocationSmb
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates some of the parameters of a previously created location for
-- Server Message Block (SMB) file system access. For information about
-- creating an SMB location, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-smb-location.html Creating a location for SMB>.
module Amazonka.DataSync.UpdateLocationSmb
  ( -- * Creating a Request
    UpdateLocationSmb (..),
    newUpdateLocationSmb,

    -- * Request Lenses
    updateLocationSmb_agentArns,
    updateLocationSmb_domain,
    updateLocationSmb_mountOptions,
    updateLocationSmb_password,
    updateLocationSmb_subdirectory,
    updateLocationSmb_user,
    updateLocationSmb_locationArn,

    -- * Destructuring the Response
    UpdateLocationSmbResponse (..),
    newUpdateLocationSmbResponse,

    -- * Response Lenses
    updateLocationSmbResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLocationSmb' smart constructor.
data UpdateLocationSmb = UpdateLocationSmb'
  { -- | The Amazon Resource Names (ARNs) of agents to use for a Simple Message
    -- Block (SMB) location.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The name of the Windows domain that the SMB server belongs to.
    domain :: Prelude.Maybe Prelude.Text,
    mountOptions :: Prelude.Maybe SmbMountOptions,
    -- | The password of the user who can mount the share has the permissions to
    -- access files and folders in the SMB share.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The subdirectory in the SMB file system that is used to read data from
    -- the SMB source location or write data to the SMB destination. The SMB
    -- path should be a path that\'s exported by the SMB server, or a
    -- subdirectory of that path. The path should be such that it can be
    -- mounted by other SMB clients in your network.
    --
    -- @Subdirectory@ must be specified with forward slashes. For example,
    -- @\/path\/to\/folder@.
    --
    -- To transfer all the data in the folder that you specified, DataSync must
    -- have permissions to mount the SMB share and to access all the data in
    -- that share. To ensure this, do either of the following:
    --
    -- -   Ensure that the user\/password specified belongs to the user who can
    --     mount the share and who has the appropriate permissions for all of
    --     the files and directories that you want DataSync to access.
    --
    -- -   Use credentials of a member of the Backup Operators group to mount
    --     the share.
    --
    -- Doing either of these options enables the agent to access the data. For
    -- the agent to access directories, you must also enable all execute
    -- access.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | The user who can mount the share has the permissions to access files and
    -- folders in the SMB share.
    user :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the SMB location to update.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLocationSmb' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'updateLocationSmb_agentArns' - The Amazon Resource Names (ARNs) of agents to use for a Simple Message
-- Block (SMB) location.
--
-- 'domain', 'updateLocationSmb_domain' - The name of the Windows domain that the SMB server belongs to.
--
-- 'mountOptions', 'updateLocationSmb_mountOptions' - Undocumented member.
--
-- 'password', 'updateLocationSmb_password' - The password of the user who can mount the share has the permissions to
-- access files and folders in the SMB share.
--
-- 'subdirectory', 'updateLocationSmb_subdirectory' - The subdirectory in the SMB file system that is used to read data from
-- the SMB source location or write data to the SMB destination. The SMB
-- path should be a path that\'s exported by the SMB server, or a
-- subdirectory of that path. The path should be such that it can be
-- mounted by other SMB clients in your network.
--
-- @Subdirectory@ must be specified with forward slashes. For example,
-- @\/path\/to\/folder@.
--
-- To transfer all the data in the folder that you specified, DataSync must
-- have permissions to mount the SMB share and to access all the data in
-- that share. To ensure this, do either of the following:
--
-- -   Ensure that the user\/password specified belongs to the user who can
--     mount the share and who has the appropriate permissions for all of
--     the files and directories that you want DataSync to access.
--
-- -   Use credentials of a member of the Backup Operators group to mount
--     the share.
--
-- Doing either of these options enables the agent to access the data. For
-- the agent to access directories, you must also enable all execute
-- access.
--
-- 'user', 'updateLocationSmb_user' - The user who can mount the share has the permissions to access files and
-- folders in the SMB share.
--
-- 'locationArn', 'updateLocationSmb_locationArn' - The Amazon Resource Name (ARN) of the SMB location to update.
newUpdateLocationSmb ::
  -- | 'locationArn'
  Prelude.Text ->
  UpdateLocationSmb
newUpdateLocationSmb pLocationArn_ =
  UpdateLocationSmb'
    { agentArns = Prelude.Nothing,
      domain = Prelude.Nothing,
      mountOptions = Prelude.Nothing,
      password = Prelude.Nothing,
      subdirectory = Prelude.Nothing,
      user = Prelude.Nothing,
      locationArn = pLocationArn_
    }

-- | The Amazon Resource Names (ARNs) of agents to use for a Simple Message
-- Block (SMB) location.
updateLocationSmb_agentArns :: Lens.Lens' UpdateLocationSmb (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLocationSmb_agentArns = Lens.lens (\UpdateLocationSmb' {agentArns} -> agentArns) (\s@UpdateLocationSmb' {} a -> s {agentArns = a} :: UpdateLocationSmb) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Windows domain that the SMB server belongs to.
updateLocationSmb_domain :: Lens.Lens' UpdateLocationSmb (Prelude.Maybe Prelude.Text)
updateLocationSmb_domain = Lens.lens (\UpdateLocationSmb' {domain} -> domain) (\s@UpdateLocationSmb' {} a -> s {domain = a} :: UpdateLocationSmb)

-- | Undocumented member.
updateLocationSmb_mountOptions :: Lens.Lens' UpdateLocationSmb (Prelude.Maybe SmbMountOptions)
updateLocationSmb_mountOptions = Lens.lens (\UpdateLocationSmb' {mountOptions} -> mountOptions) (\s@UpdateLocationSmb' {} a -> s {mountOptions = a} :: UpdateLocationSmb)

-- | The password of the user who can mount the share has the permissions to
-- access files and folders in the SMB share.
updateLocationSmb_password :: Lens.Lens' UpdateLocationSmb (Prelude.Maybe Prelude.Text)
updateLocationSmb_password = Lens.lens (\UpdateLocationSmb' {password} -> password) (\s@UpdateLocationSmb' {} a -> s {password = a} :: UpdateLocationSmb) Prelude.. Lens.mapping Data._Sensitive

-- | The subdirectory in the SMB file system that is used to read data from
-- the SMB source location or write data to the SMB destination. The SMB
-- path should be a path that\'s exported by the SMB server, or a
-- subdirectory of that path. The path should be such that it can be
-- mounted by other SMB clients in your network.
--
-- @Subdirectory@ must be specified with forward slashes. For example,
-- @\/path\/to\/folder@.
--
-- To transfer all the data in the folder that you specified, DataSync must
-- have permissions to mount the SMB share and to access all the data in
-- that share. To ensure this, do either of the following:
--
-- -   Ensure that the user\/password specified belongs to the user who can
--     mount the share and who has the appropriate permissions for all of
--     the files and directories that you want DataSync to access.
--
-- -   Use credentials of a member of the Backup Operators group to mount
--     the share.
--
-- Doing either of these options enables the agent to access the data. For
-- the agent to access directories, you must also enable all execute
-- access.
updateLocationSmb_subdirectory :: Lens.Lens' UpdateLocationSmb (Prelude.Maybe Prelude.Text)
updateLocationSmb_subdirectory = Lens.lens (\UpdateLocationSmb' {subdirectory} -> subdirectory) (\s@UpdateLocationSmb' {} a -> s {subdirectory = a} :: UpdateLocationSmb)

-- | The user who can mount the share has the permissions to access files and
-- folders in the SMB share.
updateLocationSmb_user :: Lens.Lens' UpdateLocationSmb (Prelude.Maybe Prelude.Text)
updateLocationSmb_user = Lens.lens (\UpdateLocationSmb' {user} -> user) (\s@UpdateLocationSmb' {} a -> s {user = a} :: UpdateLocationSmb)

-- | The Amazon Resource Name (ARN) of the SMB location to update.
updateLocationSmb_locationArn :: Lens.Lens' UpdateLocationSmb Prelude.Text
updateLocationSmb_locationArn = Lens.lens (\UpdateLocationSmb' {locationArn} -> locationArn) (\s@UpdateLocationSmb' {} a -> s {locationArn = a} :: UpdateLocationSmb)

instance Core.AWSRequest UpdateLocationSmb where
  type
    AWSResponse UpdateLocationSmb =
      UpdateLocationSmbResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLocationSmbResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLocationSmb where
  hashWithSalt _salt UpdateLocationSmb' {..} =
    _salt `Prelude.hashWithSalt` agentArns
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` mountOptions
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` user
      `Prelude.hashWithSalt` locationArn

instance Prelude.NFData UpdateLocationSmb where
  rnf UpdateLocationSmb' {..} =
    Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf mountOptions
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf user
      `Prelude.seq` Prelude.rnf locationArn

instance Data.ToHeaders UpdateLocationSmb where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.UpdateLocationSmb" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLocationSmb where
  toJSON UpdateLocationSmb' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentArns" Data..=) Prelude.<$> agentArns,
            ("Domain" Data..=) Prelude.<$> domain,
            ("MountOptions" Data..=) Prelude.<$> mountOptions,
            ("Password" Data..=) Prelude.<$> password,
            ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("User" Data..=) Prelude.<$> user,
            Prelude.Just ("LocationArn" Data..= locationArn)
          ]
      )

instance Data.ToPath UpdateLocationSmb where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateLocationSmb where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLocationSmbResponse' smart constructor.
data UpdateLocationSmbResponse = UpdateLocationSmbResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLocationSmbResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLocationSmbResponse_httpStatus' - The response's http status code.
newUpdateLocationSmbResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLocationSmbResponse
newUpdateLocationSmbResponse pHttpStatus_ =
  UpdateLocationSmbResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLocationSmbResponse_httpStatus :: Lens.Lens' UpdateLocationSmbResponse Prelude.Int
updateLocationSmbResponse_httpStatus = Lens.lens (\UpdateLocationSmbResponse' {httpStatus} -> httpStatus) (\s@UpdateLocationSmbResponse' {} a -> s {httpStatus = a} :: UpdateLocationSmbResponse)

instance Prelude.NFData UpdateLocationSmbResponse where
  rnf UpdateLocationSmbResponse' {..} =
    Prelude.rnf httpStatus
