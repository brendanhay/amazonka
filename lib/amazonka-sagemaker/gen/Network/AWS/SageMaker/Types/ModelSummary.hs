-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ModelSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelSummary
  ( ModelSummary (..),

    -- * Smart constructor
    mkModelSummary,

    -- * Lenses
    msModelName,
    msModelARN,
    msCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides summary information about a model.
--
-- /See:/ 'mkModelSummary' smart constructor.
data ModelSummary = ModelSummary'
  { modelName :: Lude.Text,
    modelARN :: Lude.Text,
    creationTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModelSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp that indicates when the model was created.
-- * 'modelARN' - The Amazon Resource Name (ARN) of the model.
-- * 'modelName' - The name of the model that you want a summary for.
mkModelSummary ::
  -- | 'modelName'
  Lude.Text ->
  -- | 'modelARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  ModelSummary
mkModelSummary pModelName_ pModelARN_ pCreationTime_ =
  ModelSummary'
    { modelName = pModelName_,
      modelARN = pModelARN_,
      creationTime = pCreationTime_
    }

-- | The name of the model that you want a summary for.
--
-- /Note:/ Consider using 'modelName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msModelName :: Lens.Lens' ModelSummary Lude.Text
msModelName = Lens.lens (modelName :: ModelSummary -> Lude.Text) (\s a -> s {modelName = a} :: ModelSummary)
{-# DEPRECATED msModelName "Use generic-lens or generic-optics with 'modelName' instead." #-}

-- | The Amazon Resource Name (ARN) of the model.
--
-- /Note:/ Consider using 'modelARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msModelARN :: Lens.Lens' ModelSummary Lude.Text
msModelARN = Lens.lens (modelARN :: ModelSummary -> Lude.Text) (\s a -> s {modelARN = a} :: ModelSummary)
{-# DEPRECATED msModelARN "Use generic-lens or generic-optics with 'modelARN' instead." #-}

-- | A timestamp that indicates when the model was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msCreationTime :: Lens.Lens' ModelSummary Lude.Timestamp
msCreationTime = Lens.lens (creationTime :: ModelSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: ModelSummary)
{-# DEPRECATED msCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Lude.FromJSON ModelSummary where
  parseJSON =
    Lude.withObject
      "ModelSummary"
      ( \x ->
          ModelSummary'
            Lude.<$> (x Lude..: "ModelName")
            Lude.<*> (x Lude..: "ModelArn")
            Lude.<*> (x Lude..: "CreationTime")
      )
