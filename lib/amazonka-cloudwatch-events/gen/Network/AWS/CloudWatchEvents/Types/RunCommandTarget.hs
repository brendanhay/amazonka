{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.RunCommandTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudWatchEvents.Types.RunCommandTarget
  ( RunCommandTarget (..),

    -- * Smart constructor
    mkRunCommandTarget,

    -- * Lenses
    rctKey,
    rctValues,
  )
where

import qualified Network.AWS.CloudWatchEvents.Types.Key as Types
import qualified Network.AWS.CloudWatchEvents.Types.RunCommandTargetValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about the EC2 instances that are to be sent the command, specified as key-value pairs. Each @RunCommandTarget@ block can include only one key, but this key may specify multiple values.
--
-- /See:/ 'mkRunCommandTarget' smart constructor.
data RunCommandTarget = RunCommandTarget'
  { -- | Can be either @tag:@ /tag-key/ or @InstanceIds@ .
    key :: Types.Key,
    -- | If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
    values :: Core.NonEmpty Types.RunCommandTargetValue
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'RunCommandTarget' value with any optional fields omitted.
mkRunCommandTarget ::
  -- | 'key'
  Types.Key ->
  -- | 'values'
  Core.NonEmpty Types.RunCommandTargetValue ->
  RunCommandTarget
mkRunCommandTarget key values = RunCommandTarget' {key, values}

-- | Can be either @tag:@ /tag-key/ or @InstanceIds@ .
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctKey :: Lens.Lens' RunCommandTarget Types.Key
rctKey = Lens.field @"key"
{-# DEPRECATED rctKey "Use generic-lens or generic-optics with 'key' instead." #-}

-- | If @Key@ is @tag:@ /tag-key/ , @Values@ is a list of tag values. If @Key@ is @InstanceIds@ , @Values@ is a list of Amazon EC2 instance IDs.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rctValues :: Lens.Lens' RunCommandTarget (Core.NonEmpty Types.RunCommandTargetValue)
rctValues = Lens.field @"values"
{-# DEPRECATED rctValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Core.FromJSON RunCommandTarget where
  toJSON RunCommandTarget {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )

instance Core.FromJSON RunCommandTarget where
  parseJSON =
    Core.withObject "RunCommandTarget" Core.$
      \x ->
        RunCommandTarget'
          Core.<$> (x Core..: "Key") Core.<*> (x Core..: "Values")
