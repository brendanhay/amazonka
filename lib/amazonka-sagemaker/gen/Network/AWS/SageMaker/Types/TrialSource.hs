-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSource
  ( TrialSource (..),

    -- * Smart constructor
    mkTrialSource,

    -- * Lenses
    tsSourceType,
    tsSourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The source of the trial.
--
-- /See:/ 'mkTrialSource' smart constructor.
data TrialSource = TrialSource'
  { sourceType :: Lude.Maybe Lude.Text,
    sourceARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialSource' with the minimum fields required to make a request.
--
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the source.
-- * 'sourceType' - The source job type.
mkTrialSource ::
  -- | 'sourceARN'
  Lude.Text ->
  TrialSource
mkTrialSource pSourceARN_ =
  TrialSource' {sourceType = Lude.Nothing, sourceARN = pSourceARN_}

-- | The source job type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSourceType :: Lens.Lens' TrialSource (Lude.Maybe Lude.Text)
tsSourceType = Lens.lens (sourceType :: TrialSource -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: TrialSource)
{-# DEPRECATED tsSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSourceARN :: Lens.Lens' TrialSource Lude.Text
tsSourceARN = Lens.lens (sourceARN :: TrialSource -> Lude.Text) (\s a -> s {sourceARN = a} :: TrialSource)
{-# DEPRECATED tsSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

instance Lude.FromJSON TrialSource where
  parseJSON =
    Lude.withObject
      "TrialSource"
      ( \x ->
          TrialSource'
            Lude.<$> (x Lude..:? "SourceType") Lude.<*> (x Lude..: "SourceArn")
      )
