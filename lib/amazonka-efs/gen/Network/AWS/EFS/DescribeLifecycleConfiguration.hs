{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeLifecycleConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current @LifecycleConfiguration@ object for the specified Amazon EFS file system. EFS lifecycle management uses the @LifecycleConfiguration@ object to identify which files to move to the EFS Infrequent Access (IA) storage class. For a file system without a @LifecycleConfiguration@ object, the call returns an empty array in the response.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeLifecycleConfiguration@ operation.
module Network.AWS.EFS.DescribeLifecycleConfiguration
  ( -- * Creating a request
    DescribeLifecycleConfiguration (..),
    mkDescribeLifecycleConfiguration,

    -- ** Request lenses
    dlcFileSystemId,

    -- * Destructuring the response
    LifecycleConfigurationDescription (..),
    mkLifecycleConfigurationDescription,

    -- ** Response lenses
    lcdLifecyclePolicies,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeLifecycleConfiguration' smart constructor.
newtype DescribeLifecycleConfiguration = DescribeLifecycleConfiguration'
  { -- | The ID of the file system whose @LifecycleConfiguration@ object you want to retrieve (String).
    fileSystemId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeLifecycleConfiguration' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - The ID of the file system whose @LifecycleConfiguration@ object you want to retrieve (String).
mkDescribeLifecycleConfiguration ::
  -- | 'fileSystemId'
  Lude.Text ->
  DescribeLifecycleConfiguration
mkDescribeLifecycleConfiguration pFileSystemId_ =
  DescribeLifecycleConfiguration' {fileSystemId = pFileSystemId_}

-- | The ID of the file system whose @LifecycleConfiguration@ object you want to retrieve (String).
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcFileSystemId :: Lens.Lens' DescribeLifecycleConfiguration Lude.Text
dlcFileSystemId = Lens.lens (fileSystemId :: DescribeLifecycleConfiguration -> Lude.Text) (\s a -> s {fileSystemId = a} :: DescribeLifecycleConfiguration)
{-# DEPRECATED dlcFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest DescribeLifecycleConfiguration where
  type
    Rs DescribeLifecycleConfiguration =
      LifecycleConfigurationDescription
  request = Req.get efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeLifecycleConfiguration where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeLifecycleConfiguration where
  toPath DescribeLifecycleConfiguration' {..} =
    Lude.mconcat
      [ "/2015-02-01/file-systems/",
        Lude.toBS fileSystemId,
        "/lifecycle-configuration"
      ]

instance Lude.ToQuery DescribeLifecycleConfiguration where
  toQuery = Lude.const Lude.mempty
