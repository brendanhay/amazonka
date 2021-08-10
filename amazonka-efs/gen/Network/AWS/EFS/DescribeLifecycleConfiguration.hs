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
-- Module      : Network.AWS.EFS.DescribeLifecycleConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current @LifecycleConfiguration@ object for the specified
-- Amazon EFS file system. EFS lifecycle management uses the
-- @LifecycleConfiguration@ object to identify which files to move to the
-- EFS Infrequent Access (IA) storage class. For a file system without a
-- @LifecycleConfiguration@ object, the call returns an empty array in the
-- response.
--
-- This operation requires permissions for the
-- @elasticfilesystem:DescribeLifecycleConfiguration@ operation.
module Network.AWS.EFS.DescribeLifecycleConfiguration
  ( -- * Creating a Request
    DescribeLifecycleConfiguration (..),
    newDescribeLifecycleConfiguration,

    -- * Request Lenses
    describeLifecycleConfiguration_fileSystemId,

    -- * Destructuring the Response
    LifecycleConfigurationDescription (..),
    newLifecycleConfigurationDescription,

    -- * Response Lenses
    lifecycleConfigurationDescription_lifecyclePolicies,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLifecycleConfiguration' smart constructor.
data DescribeLifecycleConfiguration = DescribeLifecycleConfiguration'
  { -- | The ID of the file system whose @LifecycleConfiguration@ object you want
    -- to retrieve (String).
    fileSystemId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLifecycleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemId', 'describeLifecycleConfiguration_fileSystemId' - The ID of the file system whose @LifecycleConfiguration@ object you want
-- to retrieve (String).
newDescribeLifecycleConfiguration ::
  -- | 'fileSystemId'
  Prelude.Text ->
  DescribeLifecycleConfiguration
newDescribeLifecycleConfiguration pFileSystemId_ =
  DescribeLifecycleConfiguration'
    { fileSystemId =
        pFileSystemId_
    }

-- | The ID of the file system whose @LifecycleConfiguration@ object you want
-- to retrieve (String).
describeLifecycleConfiguration_fileSystemId :: Lens.Lens' DescribeLifecycleConfiguration Prelude.Text
describeLifecycleConfiguration_fileSystemId = Lens.lens (\DescribeLifecycleConfiguration' {fileSystemId} -> fileSystemId) (\s@DescribeLifecycleConfiguration' {} a -> s {fileSystemId = a} :: DescribeLifecycleConfiguration)

instance
  Core.AWSRequest
    DescribeLifecycleConfiguration
  where
  type
    AWSResponse DescribeLifecycleConfiguration =
      LifecycleConfigurationDescription
  request = Request.get defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance
  Prelude.Hashable
    DescribeLifecycleConfiguration

instance
  Prelude.NFData
    DescribeLifecycleConfiguration

instance
  Core.ToHeaders
    DescribeLifecycleConfiguration
  where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath DescribeLifecycleConfiguration where
  toPath DescribeLifecycleConfiguration' {..} =
    Prelude.mconcat
      [ "/2015-02-01/file-systems/",
        Core.toBS fileSystemId,
        "/lifecycle-configuration"
      ]

instance Core.ToQuery DescribeLifecycleConfiguration where
  toQuery = Prelude.const Prelude.mempty
