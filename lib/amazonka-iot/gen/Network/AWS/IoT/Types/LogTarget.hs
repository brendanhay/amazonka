{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LogTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LogTarget
  ( LogTarget (..),

    -- * Smart constructor
    mkLogTarget,

    -- * Lenses
    ltTargetName,
    ltTargetType,
  )
where

import Network.AWS.IoT.Types.LogTargetType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A log target.
--
-- /See:/ 'mkLogTarget' smart constructor.
data LogTarget = LogTarget'
  { targetName :: Lude.Maybe Lude.Text,
    targetType :: LogTargetType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LogTarget' with the minimum fields required to make a request.
--
-- * 'targetName' - The target name.
-- * 'targetType' - The target type.
mkLogTarget ::
  -- | 'targetType'
  LogTargetType ->
  LogTarget
mkLogTarget pTargetType_ =
  LogTarget' {targetName = Lude.Nothing, targetType = pTargetType_}

-- | The target name.
--
-- /Note:/ Consider using 'targetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetName :: Lens.Lens' LogTarget (Lude.Maybe Lude.Text)
ltTargetName = Lens.lens (targetName :: LogTarget -> Lude.Maybe Lude.Text) (\s a -> s {targetName = a} :: LogTarget)
{-# DEPRECATED ltTargetName "Use generic-lens or generic-optics with 'targetName' instead." #-}

-- | The target type.
--
-- /Note:/ Consider using 'targetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltTargetType :: Lens.Lens' LogTarget LogTargetType
ltTargetType = Lens.lens (targetType :: LogTarget -> LogTargetType) (\s a -> s {targetType = a} :: LogTarget)
{-# DEPRECATED ltTargetType "Use generic-lens or generic-optics with 'targetType' instead." #-}

instance Lude.FromJSON LogTarget where
  parseJSON =
    Lude.withObject
      "LogTarget"
      ( \x ->
          LogTarget'
            Lude.<$> (x Lude..:? "targetName") Lude.<*> (x Lude..: "targetType")
      )

instance Lude.ToJSON LogTarget where
  toJSON LogTarget' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("targetName" Lude..=) Lude.<$> targetName,
            Lude.Just ("targetType" Lude..= targetType)
          ]
      )
