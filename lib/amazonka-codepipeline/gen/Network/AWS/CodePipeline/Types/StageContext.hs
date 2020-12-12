{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.StageContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.StageContext
  ( StageContext (..),

    -- * Smart constructor
    mkStageContext,

    -- * Lenses
    scName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents information about a stage to a job worker.
--
-- /See:/ 'mkStageContext' smart constructor.
newtype StageContext = StageContext' {name :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StageContext' with the minimum fields required to make a request.
--
-- * 'name' - The name of the stage.
mkStageContext ::
  StageContext
mkStageContext = StageContext' {name = Lude.Nothing}

-- | The name of the stage.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' StageContext (Lude.Maybe Lude.Text)
scName = Lens.lens (name :: StageContext -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: StageContext)
{-# DEPRECATED scName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON StageContext where
  parseJSON =
    Lude.withObject
      "StageContext"
      (\x -> StageContext' Lude.<$> (x Lude..:? "name"))
