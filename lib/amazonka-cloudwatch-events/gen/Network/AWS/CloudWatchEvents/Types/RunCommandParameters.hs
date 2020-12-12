{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RunCommandParameters
  ( RunCommandParameters (..),

    -- * Smart constructor
    mkRunCommandParameters,

    -- * Lenses
    rcpRunCommandTargets,
  )
where

import Network.AWS.CloudWatchEvents.Types.RunCommandTarget
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This parameter contains the criteria (either InstanceIds or a tag) used to specify which EC2 instances are to be sent the command.
--
-- /See:/ 'mkRunCommandParameters' smart constructor.
newtype RunCommandParameters = RunCommandParameters'
  { runCommandTargets ::
      Lude.NonEmpty RunCommandTarget
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RunCommandParameters' with the minimum fields required to make a request.
--
-- * 'runCommandTargets' - Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
mkRunCommandParameters ::
  -- | 'runCommandTargets'
  Lude.NonEmpty RunCommandTarget ->
  RunCommandParameters
mkRunCommandParameters pRunCommandTargets_ =
  RunCommandParameters' {runCommandTargets = pRunCommandTargets_}

-- | Currently, we support including only one RunCommandTarget block, which specifies either an array of InstanceIds or a tag.
--
-- /Note:/ Consider using 'runCommandTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rcpRunCommandTargets :: Lens.Lens' RunCommandParameters (Lude.NonEmpty RunCommandTarget)
rcpRunCommandTargets = Lens.lens (runCommandTargets :: RunCommandParameters -> Lude.NonEmpty RunCommandTarget) (\s a -> s {runCommandTargets = a} :: RunCommandParameters)
{-# DEPRECATED rcpRunCommandTargets "Use generic-lens or generic-optics with 'runCommandTargets' instead." #-}

instance Lude.FromJSON RunCommandParameters where
  parseJSON =
    Lude.withObject
      "RunCommandParameters"
      ( \x ->
          RunCommandParameters' Lude.<$> (x Lude..: "RunCommandTargets")
      )

instance Lude.ToJSON RunCommandParameters where
  toJSON RunCommandParameters' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("RunCommandTargets" Lude..= runCommandTargets)]
      )
