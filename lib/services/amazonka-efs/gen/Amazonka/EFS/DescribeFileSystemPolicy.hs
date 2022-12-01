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
-- Module      : Amazonka.EFS.DescribeFileSystemPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @FileSystemPolicy@ for the specified EFS file system.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeFileSystemPolicy@ action.
module Amazonka.EFS.DescribeFileSystemPolicy
  ( -- * Creating a Request
    DescribeFileSystemPolicy (..),
    newDescribeFileSystemPolicy,

    -- * Request Lenses
    describeFileSystemPolicy_fileSystemId,

    -- * Destructuring the Response
    FileSystemPolicyDescription (..),
    newFileSystemPolicyDescription,

    -- * Response Lenses
    fileSystemPolicyDescription_policy,
    fileSystemPolicyDescription_fileSystemId,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.EFS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeFileSystemPolicy' smart constructor.
data DescribeFileSystemPolicy = DescribeFileSystemPolicy'
  { -- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DescribeFileSystemPolicy
newDescribeFileSystemPolicy pFileSystemId_ =
  DescribeFileSystemPolicy'
    { fileSystemId =
        pFileSystemId_
    }

-- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
describeFileSystemPolicy_fileSystemId :: Lens.Lens' DescribeFileSystemPolicy Prelude.Text
describeFileSystemPolicy_fileSystemId = Lens.lens (\DescribeFileSystemPolicy' {fileSystemId} -> fileSystemId) (\s@DescribeFileSystemPolicy' {} a -> s {fileSystemId = a} :: DescribeFileSystemPolicy)

instance Core.AWSRequest DescribeFileSystemPolicy where
  type
    AWSResponse DescribeFileSystemPolicy =
      FileSystemPolicyDescription
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable DescribeFileSystemPolicy where
  hashWithSalt _salt DescribeFileSystemPolicy' {..} =
    _salt `Prelude.hashWithSalt` fileSystemId

instance Prelude.NFData DescribeFileSystemPolicy where
  rnf DescribeFileSystemPolicy' {..} =
    Prelude.rnf fileSystemId

instance Core.ToHeaders DescribeFileSystemPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeFileSystemPolicy where
  toPath DescribeFileSystemPolicy' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Core.toBS fileSystemId,
        "/policy"
      ]

instance Core.ToQuery DescribeFileSystemPolicy where
  toQuery = Prelude.const Prelude.mempty
