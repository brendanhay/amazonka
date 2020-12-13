{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.DescribeStorage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns account level backups storage size and provisional storage.
module Network.AWS.Redshift.DescribeStorage
  ( -- * Creating a request
    DescribeStorage (..),
    mkDescribeStorage,

    -- * Destructuring the response
    DescribeStorageResponse (..),
    mkDescribeStorageResponse,

    -- ** Response lenses
    dsrsTotalProvisionedStorageInMegaBytes,
    dsrsTotalBackupSizeInMegaBytes,
    dsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeStorage' smart constructor.
data DescribeStorage = DescribeStorage'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStorage' with the minimum fields required to make a request.
mkDescribeStorage ::
  DescribeStorage
mkDescribeStorage = DescribeStorage'

instance Lude.AWSRequest DescribeStorage where
  type Rs DescribeStorage = DescribeStorageResponse
  request = Req.postQuery redshiftService
  response =
    Res.receiveXMLWrapper
      "DescribeStorageResult"
      ( \s h x ->
          DescribeStorageResponse'
            Lude.<$> (x Lude..@? "TotalProvisionedStorageInMegaBytes")
            Lude.<*> (x Lude..@? "TotalBackupSizeInMegaBytes")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeStorage where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeStorage where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeStorage where
  toQuery =
    Lude.const
      ( Lude.mconcat
          [ "Action" Lude.=: ("DescribeStorage" :: Lude.ByteString),
            "Version" Lude.=: ("2012-12-01" :: Lude.ByteString)
          ]
      )

-- | /See:/ 'mkDescribeStorageResponse' smart constructor.
data DescribeStorageResponse = DescribeStorageResponse'
  { -- | The total amount of storage currently provisioned.
    totalProvisionedStorageInMegaBytes :: Lude.Maybe Lude.Double,
    -- | The total amount of storage currently used for snapshots.
    totalBackupSizeInMegaBytes :: Lude.Maybe Lude.Double,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeStorageResponse' with the minimum fields required to make a request.
--
-- * 'totalProvisionedStorageInMegaBytes' - The total amount of storage currently provisioned.
-- * 'totalBackupSizeInMegaBytes' - The total amount of storage currently used for snapshots.
-- * 'responseStatus' - The response status code.
mkDescribeStorageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeStorageResponse
mkDescribeStorageResponse pResponseStatus_ =
  DescribeStorageResponse'
    { totalProvisionedStorageInMegaBytes =
        Lude.Nothing,
      totalBackupSizeInMegaBytes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The total amount of storage currently provisioned.
--
-- /Note:/ Consider using 'totalProvisionedStorageInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsTotalProvisionedStorageInMegaBytes :: Lens.Lens' DescribeStorageResponse (Lude.Maybe Lude.Double)
dsrsTotalProvisionedStorageInMegaBytes = Lens.lens (totalProvisionedStorageInMegaBytes :: DescribeStorageResponse -> Lude.Maybe Lude.Double) (\s a -> s {totalProvisionedStorageInMegaBytes = a} :: DescribeStorageResponse)
{-# DEPRECATED dsrsTotalProvisionedStorageInMegaBytes "Use generic-lens or generic-optics with 'totalProvisionedStorageInMegaBytes' instead." #-}

-- | The total amount of storage currently used for snapshots.
--
-- /Note:/ Consider using 'totalBackupSizeInMegaBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsTotalBackupSizeInMegaBytes :: Lens.Lens' DescribeStorageResponse (Lude.Maybe Lude.Double)
dsrsTotalBackupSizeInMegaBytes = Lens.lens (totalBackupSizeInMegaBytes :: DescribeStorageResponse -> Lude.Maybe Lude.Double) (\s a -> s {totalBackupSizeInMegaBytes = a} :: DescribeStorageResponse)
{-# DEPRECATED dsrsTotalBackupSizeInMegaBytes "Use generic-lens or generic-optics with 'totalBackupSizeInMegaBytes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrsResponseStatus :: Lens.Lens' DescribeStorageResponse Lude.Int
dsrsResponseStatus = Lens.lens (responseStatus :: DescribeStorageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeStorageResponse)
{-# DEPRECATED dsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
