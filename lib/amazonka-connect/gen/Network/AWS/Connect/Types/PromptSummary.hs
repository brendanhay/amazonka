{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PromptSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PromptSummary
  ( PromptSummary (..),

    -- * Smart constructor
    mkPromptSummary,

    -- * Lenses
    psARN,
    psName,
    psId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the prompt.
--
-- /See:/ 'mkPromptSummary' smart constructor.
data PromptSummary = PromptSummary'
  { arn :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'PromptSummary' with the minimum fields required to make a request.
--
-- * 'arn' - The Amazon Resource Name (ARN) of the prompt.
-- * 'id' - The identifier of the prompt.
-- * 'name' - The name of the prompt.
mkPromptSummary ::
  PromptSummary
mkPromptSummary =
  PromptSummary'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      id = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the prompt.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psARN :: Lens.Lens' PromptSummary (Lude.Maybe Lude.Text)
psARN = Lens.lens (arn :: PromptSummary -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: PromptSummary)
{-# DEPRECATED psARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The name of the prompt.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PromptSummary (Lude.Maybe Lude.Text)
psName = Lens.lens (name :: PromptSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: PromptSummary)
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The identifier of the prompt.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psId :: Lens.Lens' PromptSummary (Lude.Maybe Lude.Text)
psId = Lens.lens (id :: PromptSummary -> Lude.Maybe Lude.Text) (\s a -> s {id = a} :: PromptSummary)
{-# DEPRECATED psId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Lude.FromJSON PromptSummary where
  parseJSON =
    Lude.withObject
      "PromptSummary"
      ( \x ->
          PromptSummary'
            Lude.<$> (x Lude..:? "Arn")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Id")
      )
