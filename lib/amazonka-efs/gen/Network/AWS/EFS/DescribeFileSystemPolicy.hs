{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EFS.DescribeFileSystemPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the @FileSystemPolicy@ for the specified EFS file system.
--
-- This operation requires permissions for the @elasticfilesystem:DescribeFileSystemPolicy@ action.
module Network.AWS.EFS.DescribeFileSystemPolicy
  ( -- * Creating a request
    DescribeFileSystemPolicy (..),
    mkDescribeFileSystemPolicy,

    -- ** Request lenses
    dfspFileSystemId,

    -- * Destructuring the response
    FileSystemPolicyDescription (..),
    mkFileSystemPolicyDescription,

    -- ** Response lenses
    fspdFileSystemId,
    fspdPolicy,
  )
where

import Network.AWS.EFS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeFileSystemPolicy' smart constructor.
newtype DescribeFileSystemPolicy = DescribeFileSystemPolicy'
  { -- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
    fileSystemId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeFileSystemPolicy' with the minimum fields required to make a request.
--
-- * 'fileSystemId' - Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
mkDescribeFileSystemPolicy ::
  -- | 'fileSystemId'
  Lude.Text ->
  DescribeFileSystemPolicy
mkDescribeFileSystemPolicy pFileSystemId_ =
  DescribeFileSystemPolicy' {fileSystemId = pFileSystemId_}

-- | Specifies which EFS file system to retrieve the @FileSystemPolicy@ for.
--
-- /Note:/ Consider using 'fileSystemId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfspFileSystemId :: Lens.Lens' DescribeFileSystemPolicy Lude.Text
dfspFileSystemId = Lens.lens (fileSystemId :: DescribeFileSystemPolicy -> Lude.Text) (\s a -> s {fileSystemId = a} :: DescribeFileSystemPolicy)
{-# DEPRECATED dfspFileSystemId "Use generic-lens or generic-optics with 'fileSystemId' instead." #-}

instance Lude.AWSRequest DescribeFileSystemPolicy where
  type Rs DescribeFileSystemPolicy = FileSystemPolicyDescription
  request = Req.get efsService
  response = Res.receiveJSON (\s h x -> Lude.eitherParseJSON x)

instance Lude.ToHeaders DescribeFileSystemPolicy where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeFileSystemPolicy where
  toPath DescribeFileSystemPolicy' {..} =
    Lude.mconcat
      ["/2015-02-01/file-systems/", Lude.toBS fileSystemId, "/policy"]

instance Lude.ToQuery DescribeFileSystemPolicy where
  toQuery = Lude.const Lude.mempty
