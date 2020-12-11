{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.AssignTapePool
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns a tape to a tape pool for archiving. The tape assigned to a pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the S3 storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
module Network.AWS.StorageGateway.AssignTapePool
  ( -- * Creating a request
    AssignTapePool (..),
    mkAssignTapePool,

    -- ** Request lenses
    atpBypassGovernanceRetention,
    atpTapeARN,
    atpPoolId,

    -- * Destructuring the response
    AssignTapePoolResponse (..),
    mkAssignTapePoolResponse,

    -- ** Response lenses
    atprsTapeARN,
    atprsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | /See:/ 'mkAssignTapePool' smart constructor.
data AssignTapePool = AssignTapePool'
  { bypassGovernanceRetention ::
      Lude.Maybe Lude.Bool,
    tapeARN :: Lude.Text,
    poolId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignTapePool' with the minimum fields required to make a request.
--
-- * 'bypassGovernanceRetention' - Set permissions to bypass governance retention. If the lock type of the archived tape is @Governance@ , the tape's archived age is not older than @RetentionLockInDays@ , and the user does not already have @BypassGovernanceRetention@ , setting this to TRUE enables the user to bypass the retention lock. This parameter is set to true by default for calls from the console.
--
-- Valid values: @TRUE@ | @FALSE@
-- * 'poolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'tapeARN' - The unique Amazon Resource Name (ARN) of the virtual tape that you want to add to the tape pool.
mkAssignTapePool ::
  -- | 'tapeARN'
  Lude.Text ->
  -- | 'poolId'
  Lude.Text ->
  AssignTapePool
mkAssignTapePool pTapeARN_ pPoolId_ =
  AssignTapePool'
    { bypassGovernanceRetention = Lude.Nothing,
      tapeARN = pTapeARN_,
      poolId = pPoolId_
    }

-- | Set permissions to bypass governance retention. If the lock type of the archived tape is @Governance@ , the tape's archived age is not older than @RetentionLockInDays@ , and the user does not already have @BypassGovernanceRetention@ , setting this to TRUE enables the user to bypass the retention lock. This parameter is set to true by default for calls from the console.
--
-- Valid values: @TRUE@ | @FALSE@
--
-- /Note:/ Consider using 'bypassGovernanceRetention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpBypassGovernanceRetention :: Lens.Lens' AssignTapePool (Lude.Maybe Lude.Bool)
atpBypassGovernanceRetention = Lens.lens (bypassGovernanceRetention :: AssignTapePool -> Lude.Maybe Lude.Bool) (\s a -> s {bypassGovernanceRetention = a} :: AssignTapePool)
{-# DEPRECATED atpBypassGovernanceRetention "Use generic-lens or generic-optics with 'bypassGovernanceRetention' instead." #-}

-- | The unique Amazon Resource Name (ARN) of the virtual tape that you want to add to the tape pool.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpTapeARN :: Lens.Lens' AssignTapePool Lude.Text
atpTapeARN = Lens.lens (tapeARN :: AssignTapePool -> Lude.Text) (\s a -> s {tapeARN = a} :: AssignTapePool)
{-# DEPRECATED atpTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atpPoolId :: Lens.Lens' AssignTapePool Lude.Text
atpPoolId = Lens.lens (poolId :: AssignTapePool -> Lude.Text) (\s a -> s {poolId = a} :: AssignTapePool)
{-# DEPRECATED atpPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

instance Lude.AWSRequest AssignTapePool where
  type Rs AssignTapePool = AssignTapePoolResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          AssignTapePoolResponse'
            Lude.<$> (x Lude..?> "TapeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AssignTapePool where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.AssignTapePool" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AssignTapePool where
  toJSON AssignTapePool' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("BypassGovernanceRetention" Lude..=)
              Lude.<$> bypassGovernanceRetention,
            Lude.Just ("TapeARN" Lude..= tapeARN),
            Lude.Just ("PoolId" Lude..= poolId)
          ]
      )

instance Lude.ToPath AssignTapePool where
  toPath = Lude.const "/"

instance Lude.ToQuery AssignTapePool where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAssignTapePoolResponse' smart constructor.
data AssignTapePoolResponse = AssignTapePoolResponse'
  { tapeARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssignTapePoolResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'tapeARN' - The unique Amazon Resource Names (ARN) of the virtual tape that was added to the tape pool.
mkAssignTapePoolResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AssignTapePoolResponse
mkAssignTapePoolResponse pResponseStatus_ =
  AssignTapePoolResponse'
    { tapeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The unique Amazon Resource Names (ARN) of the virtual tape that was added to the tape pool.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atprsTapeARN :: Lens.Lens' AssignTapePoolResponse (Lude.Maybe Lude.Text)
atprsTapeARN = Lens.lens (tapeARN :: AssignTapePoolResponse -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: AssignTapePoolResponse)
{-# DEPRECATED atprsTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atprsResponseStatus :: Lens.Lens' AssignTapePoolResponse Lude.Int
atprsResponseStatus = Lens.lens (responseStatus :: AssignTapePoolResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AssignTapePoolResponse)
{-# DEPRECATED atprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
