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
-- Module      : Network.AWS.Transfer.UpdateAccess
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allows you to update parameters for the access specified in the
-- @ServerID@ and @ExternalID@ parameters.
module Network.AWS.Transfer.UpdateAccess
  ( -- * Creating a Request
    UpdateAccess (..),
    newUpdateAccess,

    -- * Request Lenses
    updateAccess_homeDirectoryType,
    updateAccess_posixProfile,
    updateAccess_homeDirectoryMappings,
    updateAccess_role,
    updateAccess_policy,
    updateAccess_homeDirectory,
    updateAccess_serverId,
    updateAccess_externalId,

    -- * Destructuring the Response
    UpdateAccessResponse (..),
    newUpdateAccessResponse,

    -- * Response Lenses
    updateAccessResponse_httpStatus,
    updateAccessResponse_serverId,
    updateAccessResponse_externalId,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Transfer.Types

-- | /See:/ 'newUpdateAccess' smart constructor.
data UpdateAccess = UpdateAccess'
  { -- | The type of landing directory (folder) you want your users\' home
    -- directory to be when they log into the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
    posixProfile :: Prelude.Maybe PosixProfile,
    -- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
    -- paths and keys should be visible to your user and how you want to make
    -- them visible. You must specify the @Entry@ and @Target@ pair, where
    -- @Entry@ shows how the path is made visible and @Target@ is the actual
    -- Amazon S3 or Amazon EFS path. If you only specify a target, it is
    -- displayed as is. You also must ensure that your Amazon Web Services
    -- Identity and Access Management (IAM) role provides access to paths in
    -- @Target@. This value can only be set when @HomeDirectoryType@ is set to
    -- /LOGICAL/.
    --
    -- The following is an @Entry@ and @Target@ pair example.
    --
    -- @[ { \"Entry\": \"\/directory1\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
    --
    -- In most cases, you can use this value instead of the session policy to
    -- lock down your user to the designated home directory (\"@chroot@\"). To
    -- do this, you can set @Entry@ to @\/@ and set @Target@ to the
    -- @HomeDirectory@ parameter value.
    --
    -- The following is an @Entry@ and @Target@ pair example for @chroot@.
    --
    -- @[ { \"Entry:\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
    --
    -- If the target of a logical directory entry does not exist in Amazon S3
    -- or EFS, the entry is ignored. As a workaround, you can use the Amazon S3
    -- API or EFS API to create 0 byte objects as place holders for your
    -- directory. If using the CLI, use the @s3api@ or @efsapi@ call instead of
    -- @s3@ or @efs@ so you can use the put-object operation. For example, you
    -- use the following:
    -- @aws s3api put-object --bucket bucketname --key path\/to\/folder\/@.
    -- Make sure that the end of the key name ends in a @\/@ for it to be
    -- considered a folder.
    homeDirectoryMappings :: Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry),
    -- | Specifies the Amazon Resource Name (ARN) of the IAM role that controls
    -- your users\' access to your Amazon S3 bucket or EFS file system. The
    -- policies attached to this role determine the level of access that you
    -- want to provide your users when transferring files into and out of your
    -- Amazon S3 bucket or EFS file system. The IAM role should also contain a
    -- trust relationship that allows the server to access your resources when
    -- servicing your users\' transfer requests.
    role' :: Prelude.Maybe Prelude.Text,
    -- | A session policy for your user so that you can use the same IAM role
    -- across multiple users. This policy scopes down user access to portions
    -- of their Amazon S3 bucket. Variables that you can use inside this policy
    -- include @${Transfer:UserName}@, @${Transfer:HomeDirectory}@, and
    -- @${Transfer:HomeBucket}@.
    --
    -- This only applies when the domain of @ServerId@ is S3. EFS does not use
    -- session policies.
    --
    -- For session policies, Amazon Web Services Transfer Family stores the
    -- policy as a JSON blob, instead of the Amazon Resource Name (ARN) of the
    -- policy. You save the policy as a JSON blob and pass it in the @Policy@
    -- argument.
    --
    -- For an example of a session policy, see
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
    -- in the /Amazon Web ServicesSecurity Token Service API Reference/.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text,
    -- | A system-assigned unique identifier for a server instance. This is the
    -- specific server that you added your user to.
    serverId :: Prelude.Text,
    -- | A unique identifier that is required to identify specific groups within
    -- your directory. The users of the group that you associate have access to
    -- your Amazon S3 or Amazon EFS resources over the enabled protocols using
    -- Amazon Web Services Transfer Family. If you know the group name, you can
    -- view the SID values by running the following command using Windows
    -- PowerShell.
    --
    -- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
    --
    -- In that command, replace /YourGroupName/ with the name of your Active
    -- Directory group.
    --
    -- The regex used to validate this parameter is a string of characters
    -- consisting of uppercase and lowercase alphanumeric characters with no
    -- spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeDirectoryType', 'updateAccess_homeDirectoryType' - The type of landing directory (folder) you want your users\' home
-- directory to be when they log into the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or EFS paths visible to your users.
--
-- 'posixProfile', 'updateAccess_posixProfile' - Undocumented member.
--
-- 'homeDirectoryMappings', 'updateAccess_homeDirectoryMappings' - Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Amazon Web Services
-- Identity and Access Management (IAM) role provides access to paths in
-- @Target@. This value can only be set when @HomeDirectoryType@ is set to
-- /LOGICAL/.
--
-- The following is an @Entry@ and @Target@ pair example.
--
-- @[ { \"Entry\": \"\/directory1\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- In most cases, you can use this value instead of the session policy to
-- lock down your user to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to @\/@ and set @Target@ to the
-- @HomeDirectory@ parameter value.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry:\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- If the target of a logical directory entry does not exist in Amazon S3
-- or EFS, the entry is ignored. As a workaround, you can use the Amazon S3
-- API or EFS API to create 0 byte objects as place holders for your
-- directory. If using the CLI, use the @s3api@ or @efsapi@ call instead of
-- @s3@ or @efs@ so you can use the put-object operation. For example, you
-- use the following:
-- @aws s3api put-object --bucket bucketname --key path\/to\/folder\/@.
-- Make sure that the end of the key name ends in a @\/@ for it to be
-- considered a folder.
--
-- 'role'', 'updateAccess_role' - Specifies the Amazon Resource Name (ARN) of the IAM role that controls
-- your users\' access to your Amazon S3 bucket or EFS file system. The
-- policies attached to this role determine the level of access that you
-- want to provide your users when transferring files into and out of your
-- Amazon S3 bucket or EFS file system. The IAM role should also contain a
-- trust relationship that allows the server to access your resources when
-- servicing your users\' transfer requests.
--
-- 'policy', 'updateAccess_policy' - A session policy for your user so that you can use the same IAM role
-- across multiple users. This policy scopes down user access to portions
-- of their Amazon S3 bucket. Variables that you can use inside this policy
-- include @${Transfer:UserName}@, @${Transfer:HomeDirectory}@, and
-- @${Transfer:HomeBucket}@.
--
-- This only applies when the domain of @ServerId@ is S3. EFS does not use
-- session policies.
--
-- For session policies, Amazon Web Services Transfer Family stores the
-- policy as a JSON blob, instead of the Amazon Resource Name (ARN) of the
-- policy. You save the policy as a JSON blob and pass it in the @Policy@
-- argument.
--
-- For an example of a session policy, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Amazon Web ServicesSecurity Token Service API Reference/.
--
-- 'homeDirectory', 'updateAccess_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'serverId', 'updateAccess_serverId' - A system-assigned unique identifier for a server instance. This is the
-- specific server that you added your user to.
--
-- 'externalId', 'updateAccess_externalId' - A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Amazon Web Services Transfer Family. If you know the group name, you can
-- view the SID values by running the following command using Windows
-- PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of uppercase and lowercase alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
newUpdateAccess ::
  -- | 'serverId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  UpdateAccess
newUpdateAccess pServerId_ pExternalId_ =
  UpdateAccess'
    { homeDirectoryType = Prelude.Nothing,
      posixProfile = Prelude.Nothing,
      homeDirectoryMappings = Prelude.Nothing,
      role' = Prelude.Nothing,
      policy = Prelude.Nothing,
      homeDirectory = Prelude.Nothing,
      serverId = pServerId_,
      externalId = pExternalId_
    }

-- | The type of landing directory (folder) you want your users\' home
-- directory to be when they log into the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or EFS paths visible to your users.
updateAccess_homeDirectoryType :: Lens.Lens' UpdateAccess (Prelude.Maybe HomeDirectoryType)
updateAccess_homeDirectoryType = Lens.lens (\UpdateAccess' {homeDirectoryType} -> homeDirectoryType) (\s@UpdateAccess' {} a -> s {homeDirectoryType = a} :: UpdateAccess)

-- | Undocumented member.
updateAccess_posixProfile :: Lens.Lens' UpdateAccess (Prelude.Maybe PosixProfile)
updateAccess_posixProfile = Lens.lens (\UpdateAccess' {posixProfile} -> posixProfile) (\s@UpdateAccess' {} a -> s {posixProfile = a} :: UpdateAccess)

-- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Amazon Web Services
-- Identity and Access Management (IAM) role provides access to paths in
-- @Target@. This value can only be set when @HomeDirectoryType@ is set to
-- /LOGICAL/.
--
-- The following is an @Entry@ and @Target@ pair example.
--
-- @[ { \"Entry\": \"\/directory1\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- In most cases, you can use this value instead of the session policy to
-- lock down your user to the designated home directory (\"@chroot@\"). To
-- do this, you can set @Entry@ to @\/@ and set @Target@ to the
-- @HomeDirectory@ parameter value.
--
-- The following is an @Entry@ and @Target@ pair example for @chroot@.
--
-- @[ { \"Entry:\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- If the target of a logical directory entry does not exist in Amazon S3
-- or EFS, the entry is ignored. As a workaround, you can use the Amazon S3
-- API or EFS API to create 0 byte objects as place holders for your
-- directory. If using the CLI, use the @s3api@ or @efsapi@ call instead of
-- @s3@ or @efs@ so you can use the put-object operation. For example, you
-- use the following:
-- @aws s3api put-object --bucket bucketname --key path\/to\/folder\/@.
-- Make sure that the end of the key name ends in a @\/@ for it to be
-- considered a folder.
updateAccess_homeDirectoryMappings :: Lens.Lens' UpdateAccess (Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry))
updateAccess_homeDirectoryMappings = Lens.lens (\UpdateAccess' {homeDirectoryMappings} -> homeDirectoryMappings) (\s@UpdateAccess' {} a -> s {homeDirectoryMappings = a} :: UpdateAccess) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the Amazon Resource Name (ARN) of the IAM role that controls
-- your users\' access to your Amazon S3 bucket or EFS file system. The
-- policies attached to this role determine the level of access that you
-- want to provide your users when transferring files into and out of your
-- Amazon S3 bucket or EFS file system. The IAM role should also contain a
-- trust relationship that allows the server to access your resources when
-- servicing your users\' transfer requests.
updateAccess_role :: Lens.Lens' UpdateAccess (Prelude.Maybe Prelude.Text)
updateAccess_role = Lens.lens (\UpdateAccess' {role'} -> role') (\s@UpdateAccess' {} a -> s {role' = a} :: UpdateAccess)

-- | A session policy for your user so that you can use the same IAM role
-- across multiple users. This policy scopes down user access to portions
-- of their Amazon S3 bucket. Variables that you can use inside this policy
-- include @${Transfer:UserName}@, @${Transfer:HomeDirectory}@, and
-- @${Transfer:HomeBucket}@.
--
-- This only applies when the domain of @ServerId@ is S3. EFS does not use
-- session policies.
--
-- For session policies, Amazon Web Services Transfer Family stores the
-- policy as a JSON blob, instead of the Amazon Resource Name (ARN) of the
-- policy. You save the policy as a JSON blob and pass it in the @Policy@
-- argument.
--
-- For an example of a session policy, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Amazon Web ServicesSecurity Token Service API Reference/.
updateAccess_policy :: Lens.Lens' UpdateAccess (Prelude.Maybe Prelude.Text)
updateAccess_policy = Lens.lens (\UpdateAccess' {policy} -> policy) (\s@UpdateAccess' {} a -> s {policy = a} :: UpdateAccess)

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
updateAccess_homeDirectory :: Lens.Lens' UpdateAccess (Prelude.Maybe Prelude.Text)
updateAccess_homeDirectory = Lens.lens (\UpdateAccess' {homeDirectory} -> homeDirectory) (\s@UpdateAccess' {} a -> s {homeDirectory = a} :: UpdateAccess)

-- | A system-assigned unique identifier for a server instance. This is the
-- specific server that you added your user to.
updateAccess_serverId :: Lens.Lens' UpdateAccess Prelude.Text
updateAccess_serverId = Lens.lens (\UpdateAccess' {serverId} -> serverId) (\s@UpdateAccess' {} a -> s {serverId = a} :: UpdateAccess)

-- | A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Amazon Web Services Transfer Family. If you know the group name, you can
-- view the SID values by running the following command using Windows
-- PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regex used to validate this parameter is a string of characters
-- consisting of uppercase and lowercase alphanumeric characters with no
-- spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
updateAccess_externalId :: Lens.Lens' UpdateAccess Prelude.Text
updateAccess_externalId = Lens.lens (\UpdateAccess' {externalId} -> externalId) (\s@UpdateAccess' {} a -> s {externalId = a} :: UpdateAccess)

instance Core.AWSRequest UpdateAccess where
  type AWSResponse UpdateAccess = UpdateAccessResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "ServerId")
            Prelude.<*> (x Core..:> "ExternalId")
      )

instance Prelude.Hashable UpdateAccess

instance Prelude.NFData UpdateAccess

instance Core.ToHeaders UpdateAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "TransferService.UpdateAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateAccess where
  toJSON UpdateAccess' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("HomeDirectoryType" Core..=)
              Prelude.<$> homeDirectoryType,
            ("PosixProfile" Core..=) Prelude.<$> posixProfile,
            ("HomeDirectoryMappings" Core..=)
              Prelude.<$> homeDirectoryMappings,
            ("Role" Core..=) Prelude.<$> role',
            ("Policy" Core..=) Prelude.<$> policy,
            ("HomeDirectory" Core..=) Prelude.<$> homeDirectory,
            Prelude.Just ("ServerId" Core..= serverId),
            Prelude.Just ("ExternalId" Core..= externalId)
          ]
      )

instance Core.ToPath UpdateAccess where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateAccessResponse' smart constructor.
data UpdateAccessResponse = UpdateAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ID of the server that the user is attached to.
    serverId :: Prelude.Text,
    -- | The external ID of the group whose users have access to your Amazon S3
    -- or Amazon EFS resources over the enabled protocols using Amazon Web
    -- ServicesTransfer Family.
    externalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateAccessResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'updateAccessResponse_serverId' - The ID of the server that the user is attached to.
--
-- 'externalId', 'updateAccessResponse_externalId' - The external ID of the group whose users have access to your Amazon S3
-- or Amazon EFS resources over the enabled protocols using Amazon Web
-- ServicesTransfer Family.
newUpdateAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  UpdateAccessResponse
newUpdateAccessResponse
  pHttpStatus_
  pServerId_
  pExternalId_ =
    UpdateAccessResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        externalId = pExternalId_
      }

-- | The response's http status code.
updateAccessResponse_httpStatus :: Lens.Lens' UpdateAccessResponse Prelude.Int
updateAccessResponse_httpStatus = Lens.lens (\UpdateAccessResponse' {httpStatus} -> httpStatus) (\s@UpdateAccessResponse' {} a -> s {httpStatus = a} :: UpdateAccessResponse)

-- | The ID of the server that the user is attached to.
updateAccessResponse_serverId :: Lens.Lens' UpdateAccessResponse Prelude.Text
updateAccessResponse_serverId = Lens.lens (\UpdateAccessResponse' {serverId} -> serverId) (\s@UpdateAccessResponse' {} a -> s {serverId = a} :: UpdateAccessResponse)

-- | The external ID of the group whose users have access to your Amazon S3
-- or Amazon EFS resources over the enabled protocols using Amazon Web
-- ServicesTransfer Family.
updateAccessResponse_externalId :: Lens.Lens' UpdateAccessResponse Prelude.Text
updateAccessResponse_externalId = Lens.lens (\UpdateAccessResponse' {externalId} -> externalId) (\s@UpdateAccessResponse' {} a -> s {externalId = a} :: UpdateAccessResponse)

instance Prelude.NFData UpdateAccessResponse
