{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSource
  ( TrialComponentSource (..),

    -- * Smart constructor
    mkTrialComponentSource,

    -- * Lenses
    tcsSourceType,
    tcsSourceARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon Resource Name (ARN) and job type of the source of a trial component.
--
-- /See:/ 'mkTrialComponentSource' smart constructor.
data TrialComponentSource = TrialComponentSource'
  { -- | The source job type.
    sourceType :: Lude.Maybe Lude.Text,
    -- | The source ARN.
    sourceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TrialComponentSource' with the minimum fields required to make a request.
--
-- * 'sourceType' - The source job type.
-- * 'sourceARN' - The source ARN.
mkTrialComponentSource ::
  -- | 'sourceARN'
  Lude.Text ->
  TrialComponentSource
mkTrialComponentSource pSourceARN_ =
  TrialComponentSource'
    { sourceType = Lude.Nothing,
      sourceARN = pSourceARN_
    }

-- | The source job type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsSourceType :: Lens.Lens' TrialComponentSource (Lude.Maybe Lude.Text)
tcsSourceType = Lens.lens (sourceType :: TrialComponentSource -> Lude.Maybe Lude.Text) (\s a -> s {sourceType = a} :: TrialComponentSource)
{-# DEPRECATED tcsSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

-- | The source ARN.
--
-- /Note:/ Consider using 'sourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsSourceARN :: Lens.Lens' TrialComponentSource Lude.Text
tcsSourceARN = Lens.lens (sourceARN :: TrialComponentSource -> Lude.Text) (\s a -> s {sourceARN = a} :: TrialComponentSource)
{-# DEPRECATED tcsSourceARN "Use generic-lens or generic-optics with 'sourceARN' instead." #-}

instance Lude.FromJSON TrialComponentSource where
  parseJSON =
    Lude.withObject
      "TrialComponentSource"
      ( \x ->
          TrialComponentSource'
            Lude.<$> (x Lude..:? "SourceType") Lude.<*> (x Lude..: "SourceArn")
      )
