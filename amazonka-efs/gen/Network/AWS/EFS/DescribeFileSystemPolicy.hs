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
-- Module      : Network.AWS.EFS.DescribeFileSystemPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @FileSystemPolicy@ for the specified EFS file system.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeFileSystemPolicy@ action.
module Network.AWS.EFS.DescribeFileSystemPolicy
  ( -- * Creating a Request
    DescribeFileSystemPolicy (..),
    newDescribeFileSystemPolicy,

    -- * Request Lenses
    describeFileSystemPolicy_fileSystemId,

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

-- | /See:/ 'newDescribeFileSystemPolicy' smart constructor.
data DescribeFileSystemPolicy = DescribeFileSystemPolicy'
  { -- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
    fileSystemId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeFileSystemPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'describeFileSystemPolicy_fileSystemId' - Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
newDescribeFileSystemPolicy ::
  -- | 'fileSystemId'
  Core.Text ->
  DescribeFileSystemPolicy
newDescribeFileSystemPolicy pFileSystemId_ =
  DescribeFileSystemPolicy'
    { fileSystemId =
        pFileSystemId_
    }

-- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
describeFileSystemPolicy_fileSystemId :: Lens.Lens' DescribeFileSystemPolicy Core.Text
describeFileSystemPolicy_fileSystemId = Lens.lens (\DescribeFileSystemPolicy' {fileSystemId} -> fileSystemId) (\s@DescribeFileSystemPolicy' {} a -> s {fileSystemId = a} :: DescribeFileSystemPolicy)

instance Core.AWSRequest DescribeFileSystemPolicy where
  type
    AWSResponse DescribeFileSystemPolicy =
      FileSystemPolicyDescription
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Core.Hashable DescribeFileSystemPolicy

instance Core.NFData DescribeFileSystemPolicy

instance Core.ToHeaders DescribeFileSystemPolicy where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeFileSystemPolicy where
  toPath DescribeFileSystemPolicy' {..} =
    Core.mconcat
      [ "/2015-02-01/file-systems/",
        Core.toBS fileSystemId,
        "/policy"
      ]

instance Core.ToQuery DescribeFileSystemPolicy where
  toQuery = Core.const Core.mempty
