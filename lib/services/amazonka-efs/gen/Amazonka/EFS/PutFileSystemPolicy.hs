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
-- Module      : Amazonka.EFS.PutFileSystemPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon EFS @FileSystemPolicy@ to an Amazon EFS file system. A
-- file system policy is an IAM resource-based policy and can contain
-- multiple policy statements. A file system always has exactly one file
-- system policy, which can be the default policy or an explicit policy set
-- or updated using this API operation. EFS file system policies have a
-- 20,000 character limit. When an explicit policy is set, it overrides the
-- default policy. For more information about the default file system
-- policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/iam-access-control-nfs-efs.html#default-filesystempolicy Default EFS File System Policy>.
--
-- EFS file system policies have a 20,000 character limit.
--
-- This operation requires permissions for the
-- @elasticfilesystem:PutFileSystemPolicy@ action.
module Amazonka.EFS.PutFileSystemPolicy
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutFileSystemPolicy' smart constructor.
data PutFileSystemPolicy = PutFileSystemPolicy'
  { -- | (Optional) A boolean that specifies whether or not to bypass the
    -- @FileSystemPolicy@ lockout safety check. The lockout safety check
    -- determines whether the policy in the request will lock out, or prevent,
    -- the IAM principal that is making the request from making future
    -- @PutFileSystemPolicy@ requests on this file system. Set
    -- @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to
    -- prevent the IAM principal that is making the request from making
    -- subsequent @PutFileSystemPolicy@ requests on this file system. The
    -- default value is @False@.
    bypassPolicyLockoutSafetyCheck :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the EFS file system that you want to create or update the
    -- @FileSystemPolicy@ for.
    fileSystemId :: Prelude.Text,
    -- | The @FileSystemPolicy@ that you\'re creating. Accepts a JSON formatted
    -- policy definition. EFS file system policies have a 20,000 character
    -- limit. To find out more about the elements that make up a file system
    -- policy, see
    -- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies>.
    policy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutFileSystemPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassPolicyLockoutSafetyCheck', 'putFileSystemPolicy_bypassPolicyLockoutSafetyCheck' - (Optional) A boolean that specifies whether or not to bypass the
-- @FileSystemPolicy@ lockout safety check. The lockout safety check
-- determines whether the policy in the request will lock out, or prevent,
-- the IAM principal that is making the request from making future
-- @PutFileSystemPolicy@ requests on this file system. Set
-- @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to
-- prevent the IAM principal that is making the request from making
-- subsequent @PutFileSystemPolicy@ requests on this file system. The
-- default value is @False@.
--
-- 'fileSystemId', 'putFileSystemPolicy_fileSystemId' - The ID of the EFS file system that you want to create or update the
-- @FileSystemPolicy@ for.
--
-- 'policy', 'putFileSystemPolicy_policy' - The @FileSystemPolicy@ that you\'re creating. Accepts a JSON formatted
-- policy definition. EFS file system policies have a 20,000 character
-- limit. To find out more about the elements that make up a file system
-- policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies>.
newPutFileSystemPolicy ::
  -- | 'fileSystemId'
  Prelude.Text ->
  -- | 'policy'
  Prelude.Text ->
  PutFileSystemPolicy
newPutFileSystemPolicy pFileSystemId_ pPolicy_ =
  PutFileSystemPolicy'
    { bypassPolicyLockoutSafetyCheck =
        Prelude.Nothing,
      fileSystemId = pFileSystemId_,
      policy = pPolicy_
    }

-- | (Optional) A boolean that specifies whether or not to bypass the
-- @FileSystemPolicy@ lockout safety check. The lockout safety check
-- determines whether the policy in the request will lock out, or prevent,
-- the IAM principal that is making the request from making future
-- @PutFileSystemPolicy@ requests on this file system. Set
-- @BypassPolicyLockoutSafetyCheck@ to @True@ only when you intend to
-- prevent the IAM principal that is making the request from making
-- subsequent @PutFileSystemPolicy@ requests on this file system. The
-- default value is @False@.
putFileSystemPolicy_bypassPolicyLockoutSafetyCheck :: Lens.Lens' PutFileSystemPolicy (Prelude.Maybe Prelude.Bool)
putFileSystemPolicy_bypassPolicyLockoutSafetyCheck = Lens.lens (\PutFileSystemPolicy' {bypassPolicyLockoutSafetyCheck} -> bypassPolicyLockoutSafetyCheck) (\s@PutFileSystemPolicy' {} a -> s {bypassPolicyLockoutSafetyCheck = a} :: PutFileSystemPolicy)

-- | The ID of the EFS file system that you want to create or update the
-- @FileSystemPolicy@ for.
putFileSystemPolicy_fileSystemId :: Lens.Lens' PutFileSystemPolicy Prelude.Text
putFileSystemPolicy_fileSystemId = Lens.lens (\PutFileSystemPolicy' {fileSystemId} -> fileSystemId) (\s@PutFileSystemPolicy' {} a -> s {fileSystemId = a} :: PutFileSystemPolicy)

-- | The @FileSystemPolicy@ that you\'re creating. Accepts a JSON formatted
-- policy definition. EFS file system policies have a 20,000 character
-- limit. To find out more about the elements that make up a file system
-- policy, see
-- <https://docs.aws.amazon.com/efs/latest/ug/access-control-overview.html#access-control-manage-access-intro-resource-policies EFS Resource-based Policies>.
putFileSystemPolicy_policy :: Lens.Lens' PutFileSystemPolicy Prelude.Text
putFileSystemPolicy_policy = Lens.lens (\PutFileSystemPolicy' {policy} -> policy) (\s@PutFileSystemPolicy' {} a -> s {policy = a} :: PutFileSystemPolicy)

instance Core.AWSRequest PutFileSystemPolicy where
  type
    AWSResponse PutFileSystemPolicy =
      FileSystemPolicyDescription
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Data.eitherParseJSON x)

instance Prelude.Hashable PutFileSystemPolicy where
  hashWithSalt _salt PutFileSystemPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` bypassPolicyLockoutSafetyCheck
      `Prelude.hashWithSalt` fileSystemId
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutFileSystemPolicy where
  rnf PutFileSystemPolicy' {..} =
    Prelude.rnf bypassPolicyLockoutSafetyCheck
      `Prelude.seq` Prelude.rnf fileSystemId
      `Prelude.seq` Prelude.rnf policy

instance Data.ToHeaders PutFileSystemPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON PutFileSystemPolicy where
  toJSON PutFileSystemPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BypassPolicyLockoutSafetyCheck" Data..=)
              Prelude.<$> bypassPolicyLockoutSafetyCheck,
            Prelude.Just ("Policy" Data..= policy)
          ]
      )

instance Data.ToPath PutFileSystemPolicy where
  toPath PutFileSystemPolicy' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Data.toBS fileSystemId,
        "/policy"
      ]

instance Data.ToQuery PutFileSystemPolicy where
  toQuery = Prelude.const Prelude.mempty
