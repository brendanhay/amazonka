{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeSMBFileShares
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a description for one or more Server Message Block (SMB) file shares from a file gateway. This operation is only supported for file gateways.
module Network.AWS.StorageGateway.DescribeSMBFileShares
  ( -- * Creating a request
    DescribeSMBFileShares (..),
    mkDescribeSMBFileShares,

    -- ** Request lenses
    dsmbfsFileShareARNList,

    -- * Destructuring the response
    DescribeSMBFileSharesResponse (..),
    mkDescribeSMBFileSharesResponse,

    -- ** Response lenses
    dsmbfsrsSMBFileShareInfoList,
    dsmbfsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DescribeSMBFileSharesInput
--
-- /See:/ 'mkDescribeSMBFileShares' smart constructor.
newtype DescribeSMBFileShares = DescribeSMBFileShares'
  { -- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
    fileShareARNList :: Lude.NonEmpty Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSMBFileShares' with the minimum fields required to make a request.
--
-- * 'fileShareARNList' - An array containing the Amazon Resource Name (ARN) of each file share to be described.
mkDescribeSMBFileShares ::
  -- | 'fileShareARNList'
  Lude.NonEmpty Lude.Text ->
  DescribeSMBFileShares
mkDescribeSMBFileShares pFileShareARNList_ =
  DescribeSMBFileShares' {fileShareARNList = pFileShareARNList_}

-- | An array containing the Amazon Resource Name (ARN) of each file share to be described.
--
-- /Note:/ Consider using 'fileShareARNList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsFileShareARNList :: Lens.Lens' DescribeSMBFileShares (Lude.NonEmpty Lude.Text)
dsmbfsFileShareARNList = Lens.lens (fileShareARNList :: DescribeSMBFileShares -> Lude.NonEmpty Lude.Text) (\s a -> s {fileShareARNList = a} :: DescribeSMBFileShares)
{-# DEPRECATED dsmbfsFileShareARNList "Use generic-lens or generic-optics with 'fileShareARNList' instead." #-}

instance Lude.AWSRequest DescribeSMBFileShares where
  type Rs DescribeSMBFileShares = DescribeSMBFileSharesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSMBFileSharesResponse'
            Lude.<$> (x Lude..?> "SMBFileShareInfoList" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSMBFileShares where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeSMBFileShares" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSMBFileShares where
  toJSON DescribeSMBFileShares' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("FileShareARNList" Lude..= fileShareARNList)]
      )

instance Lude.ToPath DescribeSMBFileShares where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSMBFileShares where
  toQuery = Lude.const Lude.mempty

-- | DescribeSMBFileSharesOutput
--
-- /See:/ 'mkDescribeSMBFileSharesResponse' smart constructor.
data DescribeSMBFileSharesResponse = DescribeSMBFileSharesResponse'
  { -- | An array containing a description for each requested file share.
    sMBFileShareInfoList :: Lude.Maybe [SMBFileShareInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSMBFileSharesResponse' with the minimum fields required to make a request.
--
-- * 'sMBFileShareInfoList' - An array containing a description for each requested file share.
-- * 'responseStatus' - The response status code.
mkDescribeSMBFileSharesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSMBFileSharesResponse
mkDescribeSMBFileSharesResponse pResponseStatus_ =
  DescribeSMBFileSharesResponse'
    { sMBFileShareInfoList =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array containing a description for each requested file share.
--
-- /Note:/ Consider using 'sMBFileShareInfoList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsrsSMBFileShareInfoList :: Lens.Lens' DescribeSMBFileSharesResponse (Lude.Maybe [SMBFileShareInfo])
dsmbfsrsSMBFileShareInfoList = Lens.lens (sMBFileShareInfoList :: DescribeSMBFileSharesResponse -> Lude.Maybe [SMBFileShareInfo]) (\s a -> s {sMBFileShareInfoList = a} :: DescribeSMBFileSharesResponse)
{-# DEPRECATED dsmbfsrsSMBFileShareInfoList "Use generic-lens or generic-optics with 'sMBFileShareInfoList' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsmbfsrsResponseStatus :: Lens.Lens' DescribeSMBFileSharesResponse Lude.Int
dsmbfsrsResponseStatus = Lens.lens (responseStatus :: DescribeSMBFileSharesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSMBFileSharesResponse)
{-# DEPRECATED dsmbfsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
