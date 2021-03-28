{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.XRay.Types.Group
  ( Group (..)
  -- * Smart constructor
  , mkGroup
  -- * Lenses
  , gFilterExpression
  , gGroupARN
  , gGroupName
  , gInsightsConfiguration
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.XRay.Types.InsightsConfiguration as Types

-- | Details and metadata for a group.
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { filterExpression :: Core.Maybe Core.Text
    -- ^ The filter expression defining the parameters to include traces.
  , groupARN :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the group generated based on the GroupName.
  , groupName :: Core.Maybe Core.Text
    -- ^ The unique case-sensitive name of the group.
  , insightsConfiguration :: Core.Maybe Types.InsightsConfiguration
    -- ^ The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications through Amazon EventBridge for the group.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Group' value with any optional fields omitted.
mkGroup
    :: Group
mkGroup
  = Group'{filterExpression = Core.Nothing, groupARN = Core.Nothing,
           groupName = Core.Nothing, insightsConfiguration = Core.Nothing}

-- | The filter expression defining the parameters to include traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gFilterExpression :: Lens.Lens' Group (Core.Maybe Core.Text)
gFilterExpression = Lens.field @"filterExpression"
{-# INLINEABLE gFilterExpression #-}
{-# DEPRECATED filterExpression "Use generic-lens or generic-optics with 'filterExpression' instead"  #-}

-- | The Amazon Resource Name (ARN) of the group generated based on the GroupName.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupARN :: Lens.Lens' Group (Core.Maybe Core.Text)
gGroupARN = Lens.field @"groupARN"
{-# INLINEABLE gGroupARN #-}
{-# DEPRECATED groupARN "Use generic-lens or generic-optics with 'groupARN' instead"  #-}

-- | The unique case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupName :: Lens.Lens' Group (Core.Maybe Core.Text)
gGroupName = Lens.field @"groupName"
{-# INLINEABLE gGroupName #-}
{-# DEPRECATED groupName "Use generic-lens or generic-optics with 'groupName' instead"  #-}

-- | The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications through Amazon EventBridge for the group.
--
--
--
-- /Note:/ Consider using 'insightsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gInsightsConfiguration :: Lens.Lens' Group (Core.Maybe Types.InsightsConfiguration)
gInsightsConfiguration = Lens.field @"insightsConfiguration"
{-# INLINEABLE gInsightsConfiguration #-}
{-# DEPRECATED insightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead"  #-}

instance Core.FromJSON Group where
        parseJSON
          = Core.withObject "Group" Core.$
              \ x ->
                Group' Core.<$>
                  (x Core..:? "FilterExpression") Core.<*> x Core..:? "GroupARN"
                    Core.<*> x Core..:? "GroupName"
                    Core.<*> x Core..:? "InsightsConfiguration"
