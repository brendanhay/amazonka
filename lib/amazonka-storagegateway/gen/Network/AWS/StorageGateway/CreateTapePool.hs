{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.CreateTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new custom tape pool. You can use custom tape pool to enable tape retention lock on tapes that are archived in the custom pool.
module Network.AWS.StorageGateway.CreateTapePool
  ( -- * Creating a request
    CreateTapePool (..),
    mkCreateTapePool,

    -- ** Request lenses
    ctpRetentionLockType,
    ctpRetentionLockTimeInDays,
    ctpPoolName,
    ctpStorageClass,
    ctpTags,

    -- * Destructuring the response
    CreateTapePoolResponse (..),
    mkCreateTapePoolResponse,

    -- ** Response lenses
    ctprsPoolARN,
    ctprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkCreateTapePool' smart constructor.
data CreateTapePool = CreateTapePool'
  { -- | Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
    retentionLockType :: Lude.Maybe RetentionLockType,
    -- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Lude.Maybe Lude.Natural,
    -- | The name of the new custom tape pool.
    poolName :: Lude.Text,
    -- | The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    storageClass :: TapeStorageClass,
    -- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTapePool' with the minimum fields required to make a request.
--
-- * 'retentionLockType' - Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
-- * 'retentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
-- * 'poolName' - The name of the new custom tape pool.
-- * 'storageClass' - The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
-- * 'tags' - A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
mkCreateTapePool ::
  -- | 'poolName'
  Lude.Text ->
  -- | 'storageClass'
  TapeStorageClass ->
  CreateTapePool
mkCreateTapePool pPoolName_ pStorageClass_ =
  CreateTapePool'
    { retentionLockType = Lude.Nothing,
      retentionLockTimeInDays = Lude.Nothing,
      poolName = pPoolName_,
      storageClass = pStorageClass_,
      tags = Lude.Nothing
    }

-- | Tape retention lock can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
--
-- /Note:/ Consider using 'retentionLockType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpRetentionLockType :: Lens.Lens' CreateTapePool (Lude.Maybe RetentionLockType)
ctpRetentionLockType = Lens.lens (retentionLockType :: CreateTapePool -> Lude.Maybe RetentionLockType) (\s a -> s {retentionLockType = a} :: CreateTapePool)
{-# DEPRECATED ctpRetentionLockType "Use generic-lens or generic-optics with 'retentionLockType' instead." #-}

-- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
--
-- /Note:/ Consider using 'retentionLockTimeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpRetentionLockTimeInDays :: Lens.Lens' CreateTapePool (Lude.Maybe Lude.Natural)
ctpRetentionLockTimeInDays = Lens.lens (retentionLockTimeInDays :: CreateTapePool -> Lude.Maybe Lude.Natural) (\s a -> s {retentionLockTimeInDays = a} :: CreateTapePool)
{-# DEPRECATED ctpRetentionLockTimeInDays "Use generic-lens or generic-optics with 'retentionLockTimeInDays' instead." #-}

-- | The name of the new custom tape pool.
--
-- /Note:/ Consider using 'poolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpPoolName :: Lens.Lens' CreateTapePool Lude.Text
ctpPoolName = Lens.lens (poolName :: CreateTapePool -> Lude.Text) (\s a -> s {poolName = a} :: CreateTapePool)
{-# DEPRECATED ctpPoolName "Use generic-lens or generic-optics with 'poolName' instead." #-}

-- | The storage class that is associated with the new custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpStorageClass :: Lens.Lens' CreateTapePool TapeStorageClass
ctpStorageClass = Lens.lens (storageClass :: CreateTapePool -> TapeStorageClass) (\s a -> s {storageClass = a} :: CreateTapePool)
{-# DEPRECATED ctpStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | A list of up to 50 tags that can be assigned to tape pool. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctpTags :: Lens.Lens' CreateTapePool (Lude.Maybe [Tag])
ctpTags = Lens.lens (tags :: CreateTapePool -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateTapePool)
{-# DEPRECATED ctpTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateTapePool where
  type Rs CreateTapePool = CreateTapePoolResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateTapePoolResponse'
            Lude.<$> (x Lude..?> "PoolARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateTapePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.CreateTapePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateTapePool where
  toJSON CreateTapePool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RetentionLockType" Lude..=) Lude.<$> retentionLockType,
            ("RetentionLockTimeInDays" Lude..=)
              Lude.<$> retentionLockTimeInDays,
            Lude.Just ("PoolName" Lude..= poolName),
            Lude.Just ("StorageClass" Lude..= storageClass),
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateTapePool where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateTapePool where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateTapePoolResponse' smart constructor.
data CreateTapePoolResponse = CreateTapePoolResponse'
  { -- | The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
    poolARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateTapePoolResponse' with the minimum fields required to make a request.
--
-- * 'poolARN' - The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
-- * 'responseStatus' - The response status code.
mkCreateTapePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateTapePoolResponse
mkCreateTapePoolResponse pResponseStatus_ =
  CreateTapePoolResponse'
    { poolARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique Amazon Resource Name (ARN) that represents the custom tape pool. Use the 'ListTapePools' operation to return a list of tape pools for your account and AWS Region.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprsPoolARN :: Lens.Lens' CreateTapePoolResponse (Lude.Maybe Lude.Text)
ctprsPoolARN = Lens.lens (poolARN :: CreateTapePoolResponse -> Lude.Maybe Lude.Text) (\s a -> s {poolARN = a} :: CreateTapePoolResponse)
{-# DEPRECATED ctprsPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ctprsResponseStatus :: Lens.Lens' CreateTapePoolResponse Lude.Int
ctprsResponseStatus = Lens.lens (responseStatus :: CreateTapePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateTapePoolResponse)
{-# DEPRECATED ctprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
