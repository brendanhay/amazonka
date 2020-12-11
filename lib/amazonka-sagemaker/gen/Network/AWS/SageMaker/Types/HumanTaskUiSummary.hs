-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.HumanTaskUiSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.HumanTaskUiSummary
  ( HumanTaskUiSummary (..),

    -- * Smart constructor
    mkHumanTaskUiSummary,

    -- * Lenses
    htusHumanTaskUiName,
    htusHumanTaskUiARN,
    htusCreationTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Container for human task user interface information.
--
-- /See:/ 'mkHumanTaskUiSummary' smart constructor.
data HumanTaskUiSummary = HumanTaskUiSummary'
  { humanTaskUiName ::
      Lude.Text,
    humanTaskUiARN :: Lude.Text,
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

-- | Creates a value of 'HumanTaskUiSummary' with the minimum fields required to make a request.
--
-- * 'creationTime' - A timestamp when SageMaker created the human task user interface.
-- * 'humanTaskUiARN' - The Amazon Resource Name (ARN) of the human task user interface.
-- * 'humanTaskUiName' - The name of the human task user interface.
mkHumanTaskUiSummary ::
  -- | 'humanTaskUiName'
  Lude.Text ->
  -- | 'humanTaskUiARN'
  Lude.Text ->
  -- | 'creationTime'
  Lude.Timestamp ->
  HumanTaskUiSummary
mkHumanTaskUiSummary
  pHumanTaskUiName_
  pHumanTaskUiARN_
  pCreationTime_ =
    HumanTaskUiSummary'
      { humanTaskUiName = pHumanTaskUiName_,
        humanTaskUiARN = pHumanTaskUiARN_,
        creationTime = pCreationTime_
      }

-- | The name of the human task user interface.
--
-- /Note:/ Consider using 'humanTaskUiName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htusHumanTaskUiName :: Lens.Lens' HumanTaskUiSummary Lude.Text
htusHumanTaskUiName = Lens.lens (humanTaskUiName :: HumanTaskUiSummary -> Lude.Text) (\s a -> s {humanTaskUiName = a} :: HumanTaskUiSummary)
{-# DEPRECATED htusHumanTaskUiName "Use generic-lens or generic-optics with 'humanTaskUiName' instead." #-}

-- | The Amazon Resource Name (ARN) of the human task user interface.
--
-- /Note:/ Consider using 'humanTaskUiARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htusHumanTaskUiARN :: Lens.Lens' HumanTaskUiSummary Lude.Text
htusHumanTaskUiARN = Lens.lens (humanTaskUiARN :: HumanTaskUiSummary -> Lude.Text) (\s a -> s {humanTaskUiARN = a} :: HumanTaskUiSummary)
{-# DEPRECATED htusHumanTaskUiARN "Use generic-lens or generic-optics with 'humanTaskUiARN' instead." #-}

-- | A timestamp when SageMaker created the human task user interface.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
htusCreationTime :: Lens.Lens' HumanTaskUiSummary Lude.Timestamp
htusCreationTime = Lens.lens (creationTime :: HumanTaskUiSummary -> Lude.Timestamp) (\s a -> s {creationTime = a} :: HumanTaskUiSummary)
{-# DEPRECATED htusCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

instance Lude.FromJSON HumanTaskUiSummary where
  parseJSON =
    Lude.withObject
      "HumanTaskUiSummary"
      ( \x ->
          HumanTaskUiSummary'
            Lude.<$> (x Lude..: "HumanTaskUiName")
            Lude.<*> (x Lude..: "HumanTaskUiArn")
            Lude.<*> (x Lude..: "CreationTime")
      )
