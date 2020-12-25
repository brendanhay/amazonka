{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.Types.ApplicationSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AutoScalingPlans.Types.ApplicationSource
  ( ApplicationSource (..),

    -- * Smart constructor
    mkApplicationSource,

    -- * Lenses
    asCloudFormationStackARN,
    asTagFilters,
  )
where

import qualified Network.AWS.AutoScalingPlans.Types.TagFilter as Types
import qualified Network.AWS.AutoScalingPlans.Types.XmlString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an application source.
--
-- /See:/ 'mkApplicationSource' smart constructor.
data ApplicationSource = ApplicationSource'
  { -- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
    cloudFormationStackARN :: Core.Maybe Types.XmlString,
    -- | A set of tags (up to 50).
    tagFilters :: Core.Maybe [Types.TagFilter]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationSource' value with any optional fields omitted.
mkApplicationSource ::
  ApplicationSource
mkApplicationSource =
  ApplicationSource'
    { cloudFormationStackARN = Core.Nothing,
      tagFilters = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of a AWS CloudFormation stack.
--
-- /Note:/ Consider using 'cloudFormationStackARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCloudFormationStackARN :: Lens.Lens' ApplicationSource (Core.Maybe Types.XmlString)
asCloudFormationStackARN = Lens.field @"cloudFormationStackARN"
{-# DEPRECATED asCloudFormationStackARN "Use generic-lens or generic-optics with 'cloudFormationStackARN' instead." #-}

-- | A set of tags (up to 50).
--
-- /Note:/ Consider using 'tagFilters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asTagFilters :: Lens.Lens' ApplicationSource (Core.Maybe [Types.TagFilter])
asTagFilters = Lens.field @"tagFilters"
{-# DEPRECATED asTagFilters "Use generic-lens or generic-optics with 'tagFilters' instead." #-}

instance Core.FromJSON ApplicationSource where
  toJSON ApplicationSource {..} =
    Core.object
      ( Core.catMaybes
          [ ("CloudFormationStackARN" Core..=)
              Core.<$> cloudFormationStackARN,
            ("TagFilters" Core..=) Core.<$> tagFilters
          ]
      )

instance Core.FromJSON ApplicationSource where
  parseJSON =
    Core.withObject "ApplicationSource" Core.$
      \x ->
        ApplicationSource'
          Core.<$> (x Core..:? "CloudFormationStackARN")
          Core.<*> (x Core..:? "TagFilters")
