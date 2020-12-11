-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.InvalidationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.InvalidationSummary
  ( InvalidationSummary (..),

    -- * Smart constructor
    mkInvalidationSummary,

    -- * Lenses
    isId,
    isCreateTime,
    isStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A summary of an invalidation request.
--
-- /See:/ 'mkInvalidationSummary' smart constructor.
data InvalidationSummary = InvalidationSummary'
  { id :: Lude.Text,
    createTime :: Lude.ISO8601,
    status :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InvalidationSummary' with the minimum fields required to make a request.
--
-- * 'createTime' - The time that an invalidation request was created.
-- * 'id' - The unique ID for an invalidation request.
-- * 'status' - The status of an invalidation request.
mkInvalidationSummary ::
  -- | 'id'
  Lude.Text ->
  -- | 'createTime'
  Lude.ISO8601 ->
  -- | 'status'
  Lude.Text ->
  InvalidationSummary
mkInvalidationSummary pId_ pCreateTime_ pStatus_ =
  InvalidationSummary'
    { id = pId_,
      createTime = pCreateTime_,
      status = pStatus_
    }

-- | The unique ID for an invalidation request.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isId :: Lens.Lens' InvalidationSummary Lude.Text
isId = Lens.lens (id :: InvalidationSummary -> Lude.Text) (\s a -> s {id = a} :: InvalidationSummary)
{-# DEPRECATED isId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The time that an invalidation request was created.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isCreateTime :: Lens.Lens' InvalidationSummary Lude.ISO8601
isCreateTime = Lens.lens (createTime :: InvalidationSummary -> Lude.ISO8601) (\s a -> s {createTime = a} :: InvalidationSummary)
{-# DEPRECATED isCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

-- | The status of an invalidation request.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
isStatus :: Lens.Lens' InvalidationSummary Lude.Text
isStatus = Lens.lens (status :: InvalidationSummary -> Lude.Text) (\s a -> s {status = a} :: InvalidationSummary)
{-# DEPRECATED isStatus "Use generic-lens or generic-optics with 'status' instead." #-}

instance Lude.FromXML InvalidationSummary where
  parseXML x =
    InvalidationSummary'
      Lude.<$> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "CreateTime")
      Lude.<*> (x Lude..@ "Status")
