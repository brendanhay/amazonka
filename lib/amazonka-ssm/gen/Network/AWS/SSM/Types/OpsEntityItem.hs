{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OpsEntityItem
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsEntityItem
  ( OpsEntityItem (..),

    -- * Smart constructor
    mkOpsEntityItem,

    -- * Lenses
    oeiContent,
    oeiCaptureTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The OpsItem summaries result item.
--
-- /See:/ 'mkOpsEntityItem' smart constructor.
data OpsEntityItem = OpsEntityItem'
  { -- | The detailed data content for an OpsItem summaries result item.
    content :: Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)],
    -- | The time OpsItem data was captured.
    captureTime :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OpsEntityItem' with the minimum fields required to make a request.
--
-- * 'content' - The detailed data content for an OpsItem summaries result item.
-- * 'captureTime' - The time OpsItem data was captured.
mkOpsEntityItem ::
  OpsEntityItem
mkOpsEntityItem =
  OpsEntityItem'
    { content = Lude.Nothing,
      captureTime = Lude.Nothing
    }

-- | The detailed data content for an OpsItem summaries result item.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeiContent :: Lens.Lens' OpsEntityItem (Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)])
oeiContent = Lens.lens (content :: OpsEntityItem -> Lude.Maybe [Lude.HashMap Lude.Text (Lude.Text)]) (\s a -> s {content = a} :: OpsEntityItem)
{-# DEPRECATED oeiContent "Use generic-lens or generic-optics with 'content' instead." #-}

-- | The time OpsItem data was captured.
--
-- /Note:/ Consider using 'captureTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
oeiCaptureTime :: Lens.Lens' OpsEntityItem (Lude.Maybe Lude.Text)
oeiCaptureTime = Lens.lens (captureTime :: OpsEntityItem -> Lude.Maybe Lude.Text) (\s a -> s {captureTime = a} :: OpsEntityItem)
{-# DEPRECATED oeiCaptureTime "Use generic-lens or generic-optics with 'captureTime' instead." #-}

instance Lude.FromJSON OpsEntityItem where
  parseJSON =
    Lude.withObject
      "OpsEntityItem"
      ( \x ->
          OpsEntityItem'
            Lude.<$> (x Lude..:? "Content" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "CaptureTime")
      )
