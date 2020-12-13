{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ExperimentSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ExperimentSource
  ( ExperimentSource (..),

    -- * Smart constructor
    mkExperimentSource,

    -- * Lenses
    esSourceType,
    esSourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The source of the experiment.
--
-- /See:/ 'mkExperimentSource' smart constructor.
data ExperimentSource = ExperimentSource'
  { -- | The source type.
    sourceType :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the source.
    sourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ExperimentSource' with the minimum fields required to make a request.
--
-- * 'sourceType' - The source type.
-- * 'sourceARN' - The Amazon Resource Name (ARN) of the source.
mkExperimentSource ::
  -- | 'sourceARN'
  Lude.Text ->
  ExperimentSource
mkExperimentSource pSourceARN_ =
  ExperimentSource'
    { sourceType = Lude.Nothing,
      sourceARN = pSourceARN_
    }

-- | The source type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceType :: Lens.Lens' ExperimentSource (Lude.Maybe Lude.Text)
esSourceType = Lens.lens (sourceType :: ExperimentSource -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: ExperimentSource)
{-# DEPRECATED esSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
esSourceARN :: Lens.Lens' ExperimentSource Lude.Text
esSourceARN = Lens.lens (sourceARN :: ExperimentSource -> Lude.Text) (\s a -> s {sourceARN = a} :: ExperimentSource)
{-# DEPRECATED esSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

instance Lude.FromJSON ExperimentSource where
  parseJSON =
    Lude.withObject
      "ExperimentSource"
      ( \x ->
          ExperimentSource'
            Lude.<$> (x Lude..:? "SourceType") Lude.<*> (x Lude..: "SourceArn")
      )
