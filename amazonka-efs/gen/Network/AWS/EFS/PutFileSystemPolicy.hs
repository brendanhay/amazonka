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
-- Module      : Network.AWS.EFS.PutFileSystemPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon EFS @FileSystemPolicy@ to an Amazon EFS file system. A
-- file system policy is an IAM resource-based policy and can contain
-- multiple policy statements. A file system always has exactly one file
-- system policy, which can be the default policy or an explicit policy set
-- or updated using this API operation. When an explicit policy is set, it
-- overrides the default policy. For more information about the default
-- file system policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/iam-access-control-nfs-efs.html#default-filesystempolicy Default EFS File System Policy>.
--
-- This operation requires permissions for the
-- @elasticfilesystem:PutFileSystemPolicy@ action.
module Network.AWS.EFS.PutFileSystemPolicy
  ( -- * Creating a Request
    PutFileSystemPolicy (..),
    newPutFileSystemPolicy,

    -- * Request Lenses
    putFileSystemPolicy_bypassPolicyLockoutSafetyCheck,
    putFileSystemPolicy_fileSystemId,
    putFileSystemPolicy_policy,

    -- * Destructuring the Response
    FileSystemPolicyDescription (..),
    newFileSystemPolicyDescription,

    -- * Response Lenses
    fileSystemPolicyDescription_fileSystemId,
    fileSystemPolicyDescription_policy,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutFileSystemPolicy' smart constructor.
data PutFileSystemPolicy = PutFileSystemPolicy'
  { -- | (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@
    -- lockout safety check. The policy lockout safety check determines whether
    -- the policy in the request will prevent the principal making the request
    -- will be locked out from making future @PutFileSystemPolicy@ requests on
    -- the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only
    -- when you intend to prevent the principal that is making the request from
    -- making a subsequent @PutFileSystemPolicy@ request on the file system.
    -- The default value is False.
    bypassPolicyLockoutSafetyCheck :: Core.Maybe Core.Bool,
    -- | The ID of the EFS file system that you want to create or update the
    -- @FileSystemPolicy@ for.
    fileSystemId :: Core.Text,
    -- | The @FileSystemPolicy@ that you\'re creating. Accepts a JSON formatted
    -- policy definition. To find out more about the elements that make up a
    -- file system policy, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies>.
    policy :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutFileSystemPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassPolicyLockoutSafetyCheck', 'putFileSystemPolicy_bypassPolicyLockoutSafetyCheck' - (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@
-- lockout safety check. The policy lockout safety check determines whether
-- the policy in the request will prevent the principal making the request
-- will be locked out from making future @PutFileSystemPolicy@ requests on
-- the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only
-- when you intend to prevent the principal that is making the request from
-- making a subsequent @PutFileSystemPolicy@ request on the file system.
-- The default value is False.
--
-- 'fileSystemId', 'putFileSystemPolicy_fileSystemId' - The ID of the EFS file system that you want to create or update the
-- @FileSystemPolicy@ for.
--
-- 'policy', 'putFileSystemPolicy_policy' - The @FileSystemPolicy@ that you\'re creating. Accepts a JSON formatted
-- policy definition. To find out more about the elements that make up a
-- file system policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies>.
newPutFileSystemPolicy ::
  -- | 'fileSystemId'
  Core.Text ->
  -- | 'policy'
  Core.Text ->
  PutFileSystemPolicy
newPutFileSystemPolicy pFileSystemId_ pPolicy_ =
  PutFileSystemPolicy'
    { bypassPolicyLockoutSafetyCheck =
        Core.Nothing,
      fileSystemId = pFileSystemId_,
      policy = pPolicy_
    }

-- | (Optional) A flag to indicate whether to bypass the @FileSystemPolicy@
-- lockout safety check. The policy lockout safety check determines whether
-- the policy in the request will prevent the principal making the request
-- will be locked out from making future @PutFileSystemPolicy@ requests on
-- the file system. Set @BypassPolicyLockoutSafetyCheck@ to @True@ only
-- when you intend to prevent the principal that is making the request from
-- making a subsequent @PutFileSystemPolicy@ request on the file system.
-- The default value is False.
putFileSystemPolicy_bypassPolicyLockoutSafetyCheck :: Lens.Lens' PutFileSystemPolicy (Core.Maybe Core.Bool)
putFileSystemPolicy_bypassPolicyLockoutSafetyCheck = Lens.lens (\PutFileSystemPolicy' {bypassPolicyLockoutSafetyCheck} -> bypassPolicyLockoutSafetyCheck) (\s@PutFileSystemPolicy' {} a -> s {bypassPolicyLockoutSafetyCheck = a} :: PutFileSystemPolicy)

-- | The ID of the EFS file system that you want to create or update the
-- @FileSystemPolicy@ for.
putFileSystemPolicy_fileSystemId :: Lens.Lens' PutFileSystemPolicy Core.Text
putFileSystemPolicy_fileSystemId = Lens.lens (\PutFileSystemPolicy' {fileSystemId} -> fileSystemId) (\s@PutFileSystemPolicy' {} a -> s {fileSystemId = a} :: PutFileSystemPolicy)

-- | The @FileSystemPolicy@ that you\'re creating. Accepts a JSON formatted
-- policy definition. To find out more about the elements that make up a
-- file system policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies>.
putFileSystemPolicy_policy :: Lens.Lens' PutFileSystemPolicy Core.Text
putFileSystemPolicy_policy = Lens.lens (\PutFileSystemPolicy' {policy} -> policy) (\s@PutFileSystemPolicy' {} a -> s {policy = a} :: PutFileSystemPolicy)

instance Core.AWSRequest PutFileSystemPolicy where
  type
    AWSResponse PutFileSystemPolicy =
      FileSystemPolicyDescription
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable PutFileSystemPolicy

instance Core.NFData PutFileSystemPolicy

instance Core.ToHeaders PutFileSystemPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToJSON PutFileSystemPolicy where
  toJSON PutFileSystemPolicy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Core..=)
              Core.<$> bypassPolicyLockoutSafetyCheck,
            Core.Just ("Policy" Core..= policy)
          ]
      )

instance Core.ToPath PutFileSystemPolicy where
  toPath PutFileSystemPolicy' {..} =
    Core.mconcat
      [ "/2015-02-01/file-systems/",
        Core.toBS fileSystemId,
        "/policy"
      ]

instance Core.ToQuery PutFileSystemPolicy where
  toQuery = Core.const Core.mempty
