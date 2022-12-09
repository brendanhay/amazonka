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
-- Module      : Amazonka.DataSync.CreateLocationNfs
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines a file system on a Network File System (NFS) server that can be
-- read from or written to.
module Amazonka.DataSync.CreateLocationNfs
  ( -- * Creating a Request
    CreateLocationNfs (..),
    newCreateLocationNfs,

    -- * Request Lenses
    createLocationNfs_mountOptions,
    createLocationNfs_tags,
    createLocationNfs_subdirectory,
    createLocationNfs_serverHostname,
    createLocationNfs_onPremConfig,

    -- * Destructuring the Response
    CreateLocationNfsResponse (..),
    newCreateLocationNfsResponse,

    -- * Response Lenses
    createLocationNfsResponse_locationArn,
    createLocationNfsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateLocationNfsRequest
--
-- /See:/ 'newCreateLocationNfs' smart constructor.
data CreateLocationNfs = CreateLocationNfs'
  { -- | The NFS mount options that DataSync can use to mount your NFS share.
    mountOptions :: Prelude.Maybe NfsMountOptions,
    -- | The key-value pair that represents the tag that you want to add to the
    -- location. The value can be an empty string. We recommend using tags to
    -- name your resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The subdirectory in the NFS file system that is used to read data from
    -- the NFS source location or write data to the NFS destination. The NFS
    -- path should be a path that\'s exported by the NFS server, or a
    -- subdirectory of that path. The path should be such that it can be
    -- mounted by other NFS clients in your network.
    --
    -- To see all the paths exported by your NFS server, run
    -- \"@showmount -e nfs-server-name@\" from an NFS client that has access to
    -- your server. You can specify any directory that appears in the results,
    -- and any subdirectory of that directory. Ensure that the NFS export is
    -- accessible without Kerberos authentication.
    --
    -- To transfer all the data in the folder you specified, DataSync needs to
    -- have permissions to read all the data. To ensure this, either configure
    -- the NFS export with @no_root_squash,@ or ensure that the permissions for
    -- all of the files that you want DataSync allow read access for all users.
    -- Doing either enables the agent to read the files. For the agent to
    -- access directories, you must additionally enable all execute access.
    --
    -- If you are copying data to or from your Snowcone device, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
    -- for more information.
    --
    -- For information about NFS export configuration, see 18.7. The
    -- \/etc\/exports Configuration File in the Red Hat Enterprise Linux
    -- documentation.
    subdirectory :: Prelude.Text,
    -- | The name of the NFS server. This value is the IP address or Domain Name
    -- Service (DNS) name of the NFS server. An agent that is installed
    -- on-premises uses this hostname to mount the NFS server in a network.
    --
    -- If you are copying data to or from your Snowcone device, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
    -- for more information.
    --
    -- This name must either be DNS-compliant or must be an IP version 4 (IPv4)
    -- address.
    serverHostname :: Prelude.Text,
    -- | Contains a list of Amazon Resource Names (ARNs) of agents that are used
    -- to connect to an NFS server.
    --
    -- If you are copying data to or from your Snowcone device, see
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
    -- for more information.
    onPremConfig :: OnPremConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationNfs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'mountOptions', 'createLocationNfs_mountOptions' - The NFS mount options that DataSync can use to mount your NFS share.
--
-- 'tags', 'createLocationNfs_tags' - The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
--
-- 'subdirectory', 'createLocationNfs_subdirectory' - The subdirectory in the NFS file system that is used to read data from
-- the NFS source location or write data to the NFS destination. The NFS
-- path should be a path that\'s exported by the NFS server, or a
-- subdirectory of that path. The path should be such that it can be
-- mounted by other NFS clients in your network.
--
-- To see all the paths exported by your NFS server, run
-- \"@showmount -e nfs-server-name@\" from an NFS client that has access to
-- your server. You can specify any directory that appears in the results,
-- and any subdirectory of that directory. Ensure that the NFS export is
-- accessible without Kerberos authentication.
--
-- To transfer all the data in the folder you specified, DataSync needs to
-- have permissions to read all the data. To ensure this, either configure
-- the NFS export with @no_root_squash,@ or ensure that the permissions for
-- all of the files that you want DataSync allow read access for all users.
-- Doing either enables the agent to read the files. For the agent to
-- access directories, you must additionally enable all execute access.
--
-- If you are copying data to or from your Snowcone device, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
-- for more information.
--
-- For information about NFS export configuration, see 18.7. The
-- \/etc\/exports Configuration File in the Red Hat Enterprise Linux
-- documentation.
--
-- 'serverHostname', 'createLocationNfs_serverHostname' - The name of the NFS server. This value is the IP address or Domain Name
-- Service (DNS) name of the NFS server. An agent that is installed
-- on-premises uses this hostname to mount the NFS server in a network.
--
-- If you are copying data to or from your Snowcone device, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
-- for more information.
--
-- This name must either be DNS-compliant or must be an IP version 4 (IPv4)
-- address.
--
-- 'onPremConfig', 'createLocationNfs_onPremConfig' - Contains a list of Amazon Resource Names (ARNs) of agents that are used
-- to connect to an NFS server.
--
-- If you are copying data to or from your Snowcone device, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
-- for more information.
newCreateLocationNfs ::
  -- | 'subdirectory'
  Prelude.Text ->
  -- | 'serverHostname'
  Prelude.Text ->
  -- | 'onPremConfig'
  OnPremConfig ->
  CreateLocationNfs
newCreateLocationNfs
  pSubdirectory_
  pServerHostname_
  pOnPremConfig_ =
    CreateLocationNfs'
      { mountOptions = Prelude.Nothing,
        tags = Prelude.Nothing,
        subdirectory = pSubdirectory_,
        serverHostname = pServerHostname_,
        onPremConfig = pOnPremConfig_
      }

-- | The NFS mount options that DataSync can use to mount your NFS share.
createLocationNfs_mountOptions :: Lens.Lens' CreateLocationNfs (Prelude.Maybe NfsMountOptions)
createLocationNfs_mountOptions = Lens.lens (\CreateLocationNfs' {mountOptions} -> mountOptions) (\s@CreateLocationNfs' {} a -> s {mountOptions = a} :: CreateLocationNfs)

-- | The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
createLocationNfs_tags :: Lens.Lens' CreateLocationNfs (Prelude.Maybe [TagListEntry])
createLocationNfs_tags = Lens.lens (\CreateLocationNfs' {tags} -> tags) (\s@CreateLocationNfs' {} a -> s {tags = a} :: CreateLocationNfs) Prelude.. Lens.mapping Lens.coerced

-- | The subdirectory in the NFS file system that is used to read data from
-- the NFS source location or write data to the NFS destination. The NFS
-- path should be a path that\'s exported by the NFS server, or a
-- subdirectory of that path. The path should be such that it can be
-- mounted by other NFS clients in your network.
--
-- To see all the paths exported by your NFS server, run
-- \"@showmount -e nfs-server-name@\" from an NFS client that has access to
-- your server. You can specify any directory that appears in the results,
-- and any subdirectory of that directory. Ensure that the NFS export is
-- accessible without Kerberos authentication.
--
-- To transfer all the data in the folder you specified, DataSync needs to
-- have permissions to read all the data. To ensure this, either configure
-- the NFS export with @no_root_squash,@ or ensure that the permissions for
-- all of the files that you want DataSync allow read access for all users.
-- Doing either enables the agent to read the files. For the agent to
-- access directories, you must additionally enable all execute access.
--
-- If you are copying data to or from your Snowcone device, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
-- for more information.
--
-- For information about NFS export configuration, see 18.7. The
-- \/etc\/exports Configuration File in the Red Hat Enterprise Linux
-- documentation.
createLocationNfs_subdirectory :: Lens.Lens' CreateLocationNfs Prelude.Text
createLocationNfs_subdirectory = Lens.lens (\CreateLocationNfs' {subdirectory} -> subdirectory) (\s@CreateLocationNfs' {} a -> s {subdirectory = a} :: CreateLocationNfs)

-- | The name of the NFS server. This value is the IP address or Domain Name
-- Service (DNS) name of the NFS server. An agent that is installed
-- on-premises uses this hostname to mount the NFS server in a network.
--
-- If you are copying data to or from your Snowcone device, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
-- for more information.
--
-- This name must either be DNS-compliant or must be an IP version 4 (IPv4)
-- address.
createLocationNfs_serverHostname :: Lens.Lens' CreateLocationNfs Prelude.Text
createLocationNfs_serverHostname = Lens.lens (\CreateLocationNfs' {serverHostname} -> serverHostname) (\s@CreateLocationNfs' {} a -> s {serverHostname = a} :: CreateLocationNfs)

-- | Contains a list of Amazon Resource Names (ARNs) of agents that are used
-- to connect to an NFS server.
--
-- If you are copying data to or from your Snowcone device, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-nfs-location.html#nfs-on-snowcone NFS Server on Snowcone>
-- for more information.
createLocationNfs_onPremConfig :: Lens.Lens' CreateLocationNfs OnPremConfig
createLocationNfs_onPremConfig = Lens.lens (\CreateLocationNfs' {onPremConfig} -> onPremConfig) (\s@CreateLocationNfs' {} a -> s {onPremConfig = a} :: CreateLocationNfs)

instance Core.AWSRequest CreateLocationNfs where
  type
    AWSResponse CreateLocationNfs =
      CreateLocationNfsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationNfsResponse'
            Prelude.<$> (x Data..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationNfs where
  hashWithSalt _salt CreateLocationNfs' {..} =
    _salt `Prelude.hashWithSalt` mountOptions
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` serverHostname
      `Prelude.hashWithSalt` onPremConfig

instance Prelude.NFData CreateLocationNfs where
  rnf CreateLocationNfs' {..} =
    Prelude.rnf mountOptions
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf serverHostname
      `Prelude.seq` Prelude.rnf onPremConfig

instance Data.ToHeaders CreateLocationNfs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.CreateLocationNfs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateLocationNfs where
  toJSON CreateLocationNfs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MountOptions" Data..=) Prelude.<$> mountOptions,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Subdirectory" Data..= subdirectory),
            Prelude.Just
              ("ServerHostname" Data..= serverHostname),
            Prelude.Just ("OnPremConfig" Data..= onPremConfig)
          ]
      )

instance Data.ToPath CreateLocationNfs where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateLocationNfs where
  toQuery = Prelude.const Prelude.mempty

-- | CreateLocationNfsResponse
--
-- /See:/ 'newCreateLocationNfsResponse' smart constructor.
data CreateLocationNfsResponse = CreateLocationNfsResponse'
  { -- | The Amazon Resource Name (ARN) of the source NFS file system location
    -- that is created.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationNfsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationNfsResponse_locationArn' - The Amazon Resource Name (ARN) of the source NFS file system location
-- that is created.
--
-- 'httpStatus', 'createLocationNfsResponse_httpStatus' - The response's http status code.
newCreateLocationNfsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationNfsResponse
newCreateLocationNfsResponse pHttpStatus_ =
  CreateLocationNfsResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the source NFS file system location
-- that is created.
createLocationNfsResponse_locationArn :: Lens.Lens' CreateLocationNfsResponse (Prelude.Maybe Prelude.Text)
createLocationNfsResponse_locationArn = Lens.lens (\CreateLocationNfsResponse' {locationArn} -> locationArn) (\s@CreateLocationNfsResponse' {} a -> s {locationArn = a} :: CreateLocationNfsResponse)

-- | The response's http status code.
createLocationNfsResponse_httpStatus :: Lens.Lens' CreateLocationNfsResponse Prelude.Int
createLocationNfsResponse_httpStatus = Lens.lens (\CreateLocationNfsResponse' {httpStatus} -> httpStatus) (\s@CreateLocationNfsResponse' {} a -> s {httpStatus = a} :: CreateLocationNfsResponse)

instance Prelude.NFData CreateLocationNfsResponse where
  rnf CreateLocationNfsResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
