-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchFailedResultModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchFailedResultModel
  ( BatchFailedResultModel (..),

    -- * Smart constructor
    mkBatchFailedResultModel,

    -- * Lenses
    bfrmARN,
    bfrmId,
    bfrmCode,
    bfrmMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details from a failed operation
--
-- /See:/ 'mkBatchFailedResultModel' smart constructor.
data BatchFailedResultModel = BatchFailedResultModel'
  { arn ::
      Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text,
    code :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchFailedResultModel' with the minimum fields required to make a request.
--
-- * 'arn' - ARN of the resource
-- * 'code' - Error code for the failed operation
-- * 'id' - ID of the resource
-- * 'message' - Error message for the failed operation
mkBatchFailedResultModel ::
  BatchFailedResultModel
mkBatchFailedResultModel =
  BatchFailedResultModel'
    { arn = Lude.Nothing,
      id = Lude.Nothing,
      code = Lude.Nothing,
      message = Lude.Nothing
    }

-- | ARN of the resource
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmARN :: Lens.Lens' BatchFailedResultModel (Lude.Maybe Lude.Text)
bfrmARN = Lens.lens (arn :: BatchFailedResultModel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: BatchFailedResultModel)
{-# DEPRECATED bfrmARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | ID of the resource
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmId :: Lens.Lens' BatchFailedResultModel (Lude.Maybe Lude.Text)
bfrmId = Lens.lens (id :: BatchFailedResultModel -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: BatchFailedResultModel)
{-# DEPRECATED bfrmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Error code for the failed operation
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmCode :: Lens.Lens' BatchFailedResultModel (Lude.Maybe Lude.Text)
bfrmCode = Lens.lens (code :: BatchFailedResultModel -> Lude.Maybe Lude.Text) (\s a -> s {code = a} :: BatchFailedResultModel)
{-# DEPRECATED bfrmCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | Error message for the failed operation
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmMessage :: Lens.Lens' BatchFailedResultModel (Lude.Maybe Lude.Text)
bfrmMessage = Lens.lens (message :: BatchFailedResultModel -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: BatchFailedResultModel)
{-# DEPRECATED bfrmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON BatchFailedResultModel where
  parseJSON =
    Lude.withObject
      "BatchFailedResultModel"
      ( \x ->
          BatchFailedResultModel'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "id")
            Lude.<*> (x Lude..:? "code")
            Lude.<*> (x Lude..:? "message")
      )
