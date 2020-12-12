{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
  ( BatchSuccessfulResultModel (..),

    -- * Smart constructor
    mkBatchSuccessfulResultModel,

    -- * Lenses
    bsrmState,
    bsrmARN,
    bsrmId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Details from a successful operation
--
-- /See:/ 'mkBatchSuccessfulResultModel' smart constructor.
data BatchSuccessfulResultModel = BatchSuccessfulResultModel'
  { state ::
      Lude.Maybe Lude.Text,
    arn :: Lude.Maybe Lude.Text,
    id :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchSuccessfulResultModel' with the minimum fields required to make a request.
--
-- * 'arn' - ARN of the resource
-- * 'id' - ID of the resource
-- * 'state' - Current state of the resource
mkBatchSuccessfulResultModel ::
  BatchSuccessfulResultModel
mkBatchSuccessfulResultModel =
  BatchSuccessfulResultModel'
    { state = Lude.Nothing,
      arn = Lude.Nothing,
      id = Lude.Nothing
    }

-- | Current state of the resource
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrmState :: Lens.Lens' BatchSuccessfulResultModel (Lude.Maybe Lude.Text)
bsrmState = Lens.lens (state :: BatchSuccessfulResultModel -> Lude.Maybe Lude.Text) (\s a -> s {state = a} :: BatchSuccessfulResultModel)
{-# DEPRECATED bsrmState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | ARN of the resource
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrmARN :: Lens.Lens' BatchSuccessfulResultModel (Lude.Maybe Lude.Text)
bsrmARN = Lens.lens (arn :: BatchSuccessfulResultModel -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: BatchSuccessfulResultModel)
{-# DEPRECATED bsrmARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | ID of the resource
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrmId :: Lens.Lens' BatchSuccessfulResultModel (Lude.Maybe Lude.Text)
bsrmId = Lens.lens (id :: BatchSuccessfulResultModel -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: BatchSuccessfulResultModel)
{-# DEPRECATED bsrmId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON BatchSuccessfulResultModel where
  parseJSON =
    Lude.withObject
      "BatchSuccessfulResultModel"
      ( \x ->
          BatchSuccessfulResultModel'
            Lude.<$> (x Lude..:? "state")
            Lude.<*> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "id")
      )
