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
-- Module      : Amazonka.Transfer.CreateAccess
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by administrators to choose which groups in the directory should
-- have access to upload and download files over the enabled protocols
-- using Transfer Family. For example, a Microsoft Active Directory might
-- contain 50,000 users, but only a small fraction might need the ability
-- to transfer files to the server. An administrator can use @CreateAccess@
-- to limit the access to the correct set of users who need this ability.
module Amazonka.Transfer.CreateAccess
  ( -- * Creating a Request
    CreateAccess (..),
    newCreateAccess,

    -- * Request Lenses
    createAccess_homeDirectory,
    createAccess_homeDirectoryMappings,
    createAccess_homeDirectoryType,
    createAccess_policy,
    createAccess_posixProfile,
    createAccess_role,
    createAccess_serverId,
    createAccess_externalId,

    -- * Destructuring the Response
    CreateAccessResponse (..),
    newCreateAccessResponse,

    -- * Response Lenses
    createAccessResponse_httpStatus,
    createAccessResponse_serverId,
    createAccessResponse_externalId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Transfer.Types

-- | /See:/ 'newCreateAccess' smart constructor.
data CreateAccess = CreateAccess'
  { -- | The landing directory (folder) for a user when they log in to the server
    -- using the client.
    --
    -- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
    homeDirectory :: Prelude.Maybe Prelude.Text,
    -- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
    -- paths and keys should be visible to your user and how you want to make
    -- them visible. You must specify the @Entry@ and @Target@ pair, where
    -- @Entry@ shows how the path is made visible and @Target@ is the actual
    -- Amazon S3 or Amazon EFS path. If you only specify a target, it is
    -- displayed as is. You also must ensure that your Identity and Access
    -- Management (IAM) role provides access to paths in @Target@. This value
    -- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
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
    -- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
    homeDirectoryMappings :: Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry),
    -- | The type of landing directory (folder) that you want your users\' home
    -- directory to be when they log in to the server. If you set it to @PATH@,
    -- the user will see the absolute Amazon S3 bucket or EFS paths as is in
    -- their file transfer protocol clients. If you set it @LOGICAL@, you need
    -- to provide mappings in the @HomeDirectoryMappings@ for how you want to
    -- make Amazon S3 or Amazon EFS paths visible to your users.
    homeDirectoryType :: Prelude.Maybe HomeDirectoryType,
    -- | A session policy for your user so that you can use the same Identity and
    -- Access Management (IAM) role across multiple users. This policy scopes
    -- down a user\'s access to portions of their Amazon S3 bucket. Variables
    -- that you can use inside this policy include @${Transfer:UserName}@,
    -- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
    --
    -- This policy applies only when the domain of @ServerId@ is Amazon S3.
    -- Amazon EFS does not use session policies.
    --
    -- For session policies, Transfer Family stores the policy as a JSON blob,
    -- instead of the Amazon Resource Name (ARN) of the policy. You save the
    -- policy as a JSON blob and pass it in the @Policy@ argument.
    --
    -- For an example of a session policy, see
    -- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
    -- in the /Security Token Service API Reference/.
    policy :: Prelude.Maybe Prelude.Text,
    posixProfile :: Prelude.Maybe PosixProfile,
    -- | The Amazon Resource Name (ARN) of the Identity and Access Management
    -- (IAM) role that controls your users\' access to your Amazon S3 bucket or
    -- Amazon EFS file system. The policies attached to this role determine the
    -- level of access that you want to provide your users when transferring
    -- files into and out of your Amazon S3 bucket or Amazon EFS file system.
    -- The IAM role should also contain a trust relationship that allows the
    -- server to access your resources when servicing your users\' transfer
    -- requests.
    role' :: Prelude.Text,
    -- | A system-assigned unique identifier for a server instance. This is the
    -- specific server that you added your user to.
    serverId :: Prelude.Text,
    -- | A unique identifier that is required to identify specific groups within
    -- your directory. The users of the group that you associate have access to
    -- your Amazon S3 or Amazon EFS resources over the enabled protocols using
    -- Transfer Family. If you know the group name, you can view the SID values
    -- by running the following command using Windows PowerShell.
    --
    -- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
    --
    -- In that command, replace /YourGroupName/ with the name of your Active
    -- Directory group.
    --
    -- The regular expression used to validate this parameter is a string of
    -- characters consisting of uppercase and lowercase alphanumeric characters
    -- with no spaces. You can also include underscores or any of the following
    -- characters: =,.\@:\/-
    externalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccess' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'homeDirectory', 'createAccess_homeDirectory' - The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
--
-- 'homeDirectoryMappings', 'createAccess_homeDirectoryMappings' - Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Identity and Access
-- Management (IAM) role provides access to paths in @Target@. This value
-- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
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
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
--
-- 'homeDirectoryType', 'createAccess_homeDirectoryType' - The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
--
-- 'policy', 'createAccess_policy' - A session policy for your user so that you can use the same Identity and
-- Access Management (IAM) role across multiple users. This policy scopes
-- down a user\'s access to portions of their Amazon S3 bucket. Variables
-- that you can use inside this policy include @${Transfer:UserName}@,
-- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
--
-- This policy applies only when the domain of @ServerId@ is Amazon S3.
-- Amazon EFS does not use session policies.
--
-- For session policies, Transfer Family stores the policy as a JSON blob,
-- instead of the Amazon Resource Name (ARN) of the policy. You save the
-- policy as a JSON blob and pass it in the @Policy@ argument.
--
-- For an example of a session policy, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Security Token Service API Reference/.
--
-- 'posixProfile', 'createAccess_posixProfile' - Undocumented member.
--
-- 'role'', 'createAccess_role' - The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
--
-- 'serverId', 'createAccess_serverId' - A system-assigned unique identifier for a server instance. This is the
-- specific server that you added your user to.
--
-- 'externalId', 'createAccess_externalId' - A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family. If you know the group name, you can view the SID values
-- by running the following command using Windows PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regular expression used to validate this parameter is a string of
-- characters consisting of uppercase and lowercase alphanumeric characters
-- with no spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
newCreateAccess ::
  -- | 'role''
  Prelude.Text ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  CreateAccess
newCreateAccess pRole_ pServerId_ pExternalId_ =
  CreateAccess'
    { homeDirectory = Prelude.Nothing,
      homeDirectoryMappings = Prelude.Nothing,
      homeDirectoryType = Prelude.Nothing,
      policy = Prelude.Nothing,
      posixProfile = Prelude.Nothing,
      role' = pRole_,
      serverId = pServerId_,
      externalId = pExternalId_
    }

-- | The landing directory (folder) for a user when they log in to the server
-- using the client.
--
-- A @HomeDirectory@ example is @\/bucket_name\/home\/mydirectory@.
createAccess_homeDirectory :: Lens.Lens' CreateAccess (Prelude.Maybe Prelude.Text)
createAccess_homeDirectory = Lens.lens (\CreateAccess' {homeDirectory} -> homeDirectory) (\s@CreateAccess' {} a -> s {homeDirectory = a} :: CreateAccess)

-- | Logical directory mappings that specify what Amazon S3 or Amazon EFS
-- paths and keys should be visible to your user and how you want to make
-- them visible. You must specify the @Entry@ and @Target@ pair, where
-- @Entry@ shows how the path is made visible and @Target@ is the actual
-- Amazon S3 or Amazon EFS path. If you only specify a target, it is
-- displayed as is. You also must ensure that your Identity and Access
-- Management (IAM) role provides access to paths in @Target@. This value
-- can be set only when @HomeDirectoryType@ is set to /LOGICAL/.
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
-- @[ { \"Entry\": \"\/\", \"Target\": \"\/bucket_name\/home\/mydirectory\" } ]@
createAccess_homeDirectoryMappings :: Lens.Lens' CreateAccess (Prelude.Maybe (Prelude.NonEmpty HomeDirectoryMapEntry))
createAccess_homeDirectoryMappings = Lens.lens (\CreateAccess' {homeDirectoryMappings} -> homeDirectoryMappings) (\s@CreateAccess' {} a -> s {homeDirectoryMappings = a} :: CreateAccess) Prelude.. Lens.mapping Lens.coerced

-- | The type of landing directory (folder) that you want your users\' home
-- directory to be when they log in to the server. If you set it to @PATH@,
-- the user will see the absolute Amazon S3 bucket or EFS paths as is in
-- their file transfer protocol clients. If you set it @LOGICAL@, you need
-- to provide mappings in the @HomeDirectoryMappings@ for how you want to
-- make Amazon S3 or Amazon EFS paths visible to your users.
createAccess_homeDirectoryType :: Lens.Lens' CreateAccess (Prelude.Maybe HomeDirectoryType)
createAccess_homeDirectoryType = Lens.lens (\CreateAccess' {homeDirectoryType} -> homeDirectoryType) (\s@CreateAccess' {} a -> s {homeDirectoryType = a} :: CreateAccess)

-- | A session policy for your user so that you can use the same Identity and
-- Access Management (IAM) role across multiple users. This policy scopes
-- down a user\'s access to portions of their Amazon S3 bucket. Variables
-- that you can use inside this policy include @${Transfer:UserName}@,
-- @${Transfer:HomeDirectory}@, and @${Transfer:HomeBucket}@.
--
-- This policy applies only when the domain of @ServerId@ is Amazon S3.
-- Amazon EFS does not use session policies.
--
-- For session policies, Transfer Family stores the policy as a JSON blob,
-- instead of the Amazon Resource Name (ARN) of the policy. You save the
-- policy as a JSON blob and pass it in the @Policy@ argument.
--
-- For an example of a session policy, see
-- <https://docs.aws.amazon.com/transfer/latest/userguide/session-policy.html Example session policy>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/STS/latest/APIReference/API_AssumeRole.html AssumeRole>
-- in the /Security Token Service API Reference/.
createAccess_policy :: Lens.Lens' CreateAccess (Prelude.Maybe Prelude.Text)
createAccess_policy = Lens.lens (\CreateAccess' {policy} -> policy) (\s@CreateAccess' {} a -> s {policy = a} :: CreateAccess)

-- | Undocumented member.
createAccess_posixProfile :: Lens.Lens' CreateAccess (Prelude.Maybe PosixProfile)
createAccess_posixProfile = Lens.lens (\CreateAccess' {posixProfile} -> posixProfile) (\s@CreateAccess' {} a -> s {posixProfile = a} :: CreateAccess)

-- | The Amazon Resource Name (ARN) of the Identity and Access Management
-- (IAM) role that controls your users\' access to your Amazon S3 bucket or
-- Amazon EFS file system. The policies attached to this role determine the
-- level of access that you want to provide your users when transferring
-- files into and out of your Amazon S3 bucket or Amazon EFS file system.
-- The IAM role should also contain a trust relationship that allows the
-- server to access your resources when servicing your users\' transfer
-- requests.
createAccess_role :: Lens.Lens' CreateAccess Prelude.Text
createAccess_role = Lens.lens (\CreateAccess' {role'} -> role') (\s@CreateAccess' {} a -> s {role' = a} :: CreateAccess)

-- | A system-assigned unique identifier for a server instance. This is the
-- specific server that you added your user to.
createAccess_serverId :: Lens.Lens' CreateAccess Prelude.Text
createAccess_serverId = Lens.lens (\CreateAccess' {serverId} -> serverId) (\s@CreateAccess' {} a -> s {serverId = a} :: CreateAccess)

-- | A unique identifier that is required to identify specific groups within
-- your directory. The users of the group that you associate have access to
-- your Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family. If you know the group name, you can view the SID values
-- by running the following command using Windows PowerShell.
--
-- @Get-ADGroup -Filter {samAccountName -like \"YourGroupName*\"} -Properties * | Select SamAccountName,ObjectSid@
--
-- In that command, replace /YourGroupName/ with the name of your Active
-- Directory group.
--
-- The regular expression used to validate this parameter is a string of
-- characters consisting of uppercase and lowercase alphanumeric characters
-- with no spaces. You can also include underscores or any of the following
-- characters: =,.\@:\/-
createAccess_externalId :: Lens.Lens' CreateAccess Prelude.Text
createAccess_externalId = Lens.lens (\CreateAccess' {externalId} -> externalId) (\s@CreateAccess' {} a -> s {externalId = a} :: CreateAccess)

instance Core.AWSRequest CreateAccess where
  type AWSResponse CreateAccess = CreateAccessResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateAccessResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "ServerId")
            Prelude.<*> (x Data..:> "ExternalId")
      )

instance Prelude.Hashable CreateAccess where
  hashWithSalt _salt CreateAccess' {..} =
    _salt `Prelude.hashWithSalt` homeDirectory
      `Prelude.hashWithSalt` homeDirectoryMappings
      `Prelude.hashWithSalt` homeDirectoryType
      `Prelude.hashWithSalt` policy
      `Prelude.hashWithSalt` posixProfile
      `Prelude.hashWithSalt` role'
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` externalId

instance Prelude.NFData CreateAccess where
  rnf CreateAccess' {..} =
    Prelude.rnf homeDirectory
      `Prelude.seq` Prelude.rnf homeDirectoryMappings
      `Prelude.seq` Prelude.rnf homeDirectoryType
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf posixProfile
      `Prelude.seq` Prelude.rnf role'
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf externalId

instance Data.ToHeaders CreateAccess where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "TransferService.CreateAccess" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateAccess where
  toJSON CreateAccess' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("HomeDirectory" Data..=) Prelude.<$> homeDirectory,
            ("HomeDirectoryMappings" Data..=)
              Prelude.<$> homeDirectoryMappings,
            ("HomeDirectoryType" Data..=)
              Prelude.<$> homeDirectoryType,
            ("Policy" Data..=) Prelude.<$> policy,
            ("PosixProfile" Data..=) Prelude.<$> posixProfile,
            Prelude.Just ("Role" Data..= role'),
            Prelude.Just ("ServerId" Data..= serverId),
            Prelude.Just ("ExternalId" Data..= externalId)
          ]
      )

instance Data.ToPath CreateAccess where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateAccess where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateAccessResponse' smart constructor.
data CreateAccessResponse = CreateAccessResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The identifier of the server that the user is attached to.
    serverId :: Prelude.Text,
    -- | The external identifier of the group whose users have access to your
    -- Amazon S3 or Amazon EFS resources over the enabled protocols using
    -- Transfer Family.
    externalId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateAccessResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createAccessResponse_httpStatus' - The response's http status code.
--
-- 'serverId', 'createAccessResponse_serverId' - The identifier of the server that the user is attached to.
--
-- 'externalId', 'createAccessResponse_externalId' - The external identifier of the group whose users have access to your
-- Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family.
newCreateAccessResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'serverId'
  Prelude.Text ->
  -- | 'externalId'
  Prelude.Text ->
  CreateAccessResponse
newCreateAccessResponse
  pHttpStatus_
  pServerId_
  pExternalId_ =
    CreateAccessResponse'
      { httpStatus = pHttpStatus_,
        serverId = pServerId_,
        externalId = pExternalId_
      }

-- | The response's http status code.
createAccessResponse_httpStatus :: Lens.Lens' CreateAccessResponse Prelude.Int
createAccessResponse_httpStatus = Lens.lens (\CreateAccessResponse' {httpStatus} -> httpStatus) (\s@CreateAccessResponse' {} a -> s {httpStatus = a} :: CreateAccessResponse)

-- | The identifier of the server that the user is attached to.
createAccessResponse_serverId :: Lens.Lens' CreateAccessResponse Prelude.Text
createAccessResponse_serverId = Lens.lens (\CreateAccessResponse' {serverId} -> serverId) (\s@CreateAccessResponse' {} a -> s {serverId = a} :: CreateAccessResponse)

-- | The external identifier of the group whose users have access to your
-- Amazon S3 or Amazon EFS resources over the enabled protocols using
-- Transfer Family.
createAccessResponse_externalId :: Lens.Lens' CreateAccessResponse Prelude.Text
createAccessResponse_externalId = Lens.lens (\CreateAccessResponse' {externalId} -> externalId) (\s@CreateAccessResponse' {} a -> s {externalId = a} :: CreateAccessResponse)

instance Prelude.NFData CreateAccessResponse where
  rnf CreateAccessResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf externalId
