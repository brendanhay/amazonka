{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.Invalidation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.Invalidation
  ( Invalidation (..),

    -- * Smart constructor
    mkInvalidation,

    -- * Lenses
    iStatus,
    iInvalidationBatch,
    iId,
    iCreateTime,
  )
where

import Network.AWS.CloudFront.Types.InvalidationBatch
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An invalidation.
--
-- /See:/ 'mkInvalidation' smart constructor.
data Invalidation = Invalidation'
  { -- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
    status :: Lude.Text,
    -- | The current invalidation information for the batch request.
    invalidationBatch :: InvalidationBatch,
    -- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
    id :: Lude.Text,
    -- | The date and time the invalidation request was first made.
    createTime :: Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Invalidation' with the minimum fields required to make a request.
--
-- * 'status' - The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
-- * 'invalidationBatch' - The current invalidation information for the batch request.
-- * 'id' - The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
-- * 'createTime' - The date and time the invalidation request was first made.
mkInvalidation ::
  -- | 'status'
  Lude.Text ->
  -- | 'invalidationBatch'
  InvalidationBatch ->
  -- | 'id'
  Lude.Text ->
  -- | 'createTime'
  Lude.DateTime ->
  Invalidation
mkInvalidation pStatus_ pInvalidationBatch_ pId_ pCreateTime_ =
  Invalidation'
    { status = pStatus_,
      invalidationBatch = pInvalidationBatch_,
      id = pId_,
      createTime = pCreateTime_
    }

-- | The status of the invalidation request. When the invalidation batch is finished, the status is @Completed@ .
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iStatus :: Lens.Lens' Invalidation Lude.Text
iStatus = Lens.lens (status :: Invalidation -> Lude.Text) (\s a -> s {status = a} :: Invalidation)
{-# DEPRECATED iStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The current invalidation information for the batch request.
--
-- /Note:/ Consider using 'invalidationBatch' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iInvalidationBatch :: Lens.Lens' Invalidation InvalidationBatch
iInvalidationBatch = Lens.lens (invalidationBatch :: Invalidation -> InvalidationBatch) (\s a -> s {invalidationBatch = a} :: Invalidation)
{-# DEPRECATED iInvalidationBatch "Use generic-lens or generic-optics with 'invalidationBatch' instead." #-}

-- | The identifier for the invalidation request. For example: @IDFDVBD632BHDS5@ .
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iId :: Lens.Lens' Invalidation Lude.Text
iId = Lens.lens (id :: Invalidation -> Lude.Text) (\s a -> s {id = a} :: Invalidation)
{-# DEPRECATED iId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The date and time the invalidation request was first made.
--
-- /Note:/ Consider using 'createTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iCreateTime :: Lens.Lens' Invalidation Lude.DateTime
iCreateTime = Lens.lens (createTime :: Invalidation -> Lude.DateTime) (\s a -> s {createTime = a} :: Invalidation)
{-# DEPRECATED iCreateTime "Use generic-lens or generic-optics with 'createTime' instead." #-}

instance Lude.FromXML Invalidation where
  parseXML x =
    Invalidation'
      Lude.<$> (x Lude..@ "Status")
      Lude.<*> (x Lude..@ "InvalidationBatch")
      Lude.<*> (x Lude..@ "Id")
      Lude.<*> (x Lude..@ "CreateTime")
