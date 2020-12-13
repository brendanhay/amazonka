{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeNFSFileShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Network File System (NFS) file shares from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DescribeNFSFileShares
  ( -- * Creating a request
    DescribeNFSFileShares (..),
    mkDescribeNFSFileShares,

    -- ** Request lenses
    dnfsfsFileShareARNList,

    -- * Destructuring the response
    DescribeNFSFileSharesResponse (..),
    mkDescribeNFSFileSharesResponse,

    -- ** Response lenses
    dnfsfsrsNFSFileShareInfoList,
    dnfsfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DescribeNFSFileSharesInput
--
-- /See:/ 'mkDescribeNFSFileShares' smart constructor.
newtype DescribeNFSFileShares = DescribeNFSFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
    fileShareARNList :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNFSFileShares' with the minimum fields required to make a request.
--
-- * 'fileShareARNList' - An array containing the Amazon Resource Name (ARN) of each file share to be described.
mkDescribeNFSFileShares ::
  -- | 'fileShareARNList'
  Lude.NonEmpty Lude.Text ->
  DescribeNFSFileShares
mkDescribeNFSFileShares pFileShareARNList_ =
  DescribeNFSFileShares' {fileShareARNList = pFileShareARNList_}

-- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
--
-- /Note:/ Consider using 'fileShareARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfsfsFileShareARNList :: Lens.Lens' DescribeNFSFileShares (Lude.NonEmpty Lude.Text)
dnfsfsFileShareARNList = Lens.lens (fileShareARNList :: DescribeNFSFileShares -> Lude.NonEmpty Lude.Text) (\s a -> s {fileShareARNList = a} :: DescribeNFSFileShares)
{-# DEPRECATED dnfsfsFileShareARNList "Use generic-lens or generic-optics with 'fileShareARNList' instead." #-}

instance Lude.AWSRequest DescribeNFSFileShares where
  type Rs DescribeNFSFileShares = DescribeNFSFileSharesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeNFSFileSharesResponse'
            Lude.<$> (x Lude..?> "NFSFileShareInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeNFSFileShares where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeNFSFileShares" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeNFSFileShares where
  toJSON DescribeNFSFileShares' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("FileShareARNList" Lude..= fileShareARNList)]
      )

instance Lude.ToPath DescribeNFSFileShares where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeNFSFileShares where
  toQuery = Lude.const Lude.mempty

-- | DescribeNFSFileSharesOutput
--
-- /See:/ 'mkDescribeNFSFileSharesResponse' smart constructor.
data DescribeNFSFileSharesResponse = DescribeNFSFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    nFSFileShareInfoList :: Lude.Maybe [NFSFileShareInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeNFSFileSharesResponse' with the minimum fields required to make a request.
--
-- * 'nFSFileShareInfoList' - An array containing a description for each requested file share.
-- * 'responseStatus' - The response status code.
mkDescribeNFSFileSharesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeNFSFileSharesResponse
mkDescribeNFSFileSharesResponse pResponseStatus_ =
  DescribeNFSFileSharesResponse'
    { nFSFileShareInfoList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array containing a description for each requested file share.
--
-- /Note:/ Consider using 'nFSFileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfsfsrsNFSFileShareInfoList :: Lens.Lens' DescribeNFSFileSharesResponse (Lude.Maybe [NFSFileShareInfo])
dnfsfsrsNFSFileShareInfoList = Lens.lens (nFSFileShareInfoList :: DescribeNFSFileSharesResponse -> Lude.Maybe [NFSFileShareInfo]) (\s a -> s {nFSFileShareInfoList = a} :: DescribeNFSFileSharesResponse)
{-# DEPRECATED dnfsfsrsNFSFileShareInfoList "Use generic-lens or generic-optics with 'nFSFileShareInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dnfsfsrsResponseStatus :: Lens.Lens' DescribeNFSFileSharesResponse Lude.Int
dnfsfsrsResponseStatus = Lens.lens (responseStatus :: DescribeNFSFileSharesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeNFSFileSharesResponse)
{-# DEPRECATED dnfsfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
