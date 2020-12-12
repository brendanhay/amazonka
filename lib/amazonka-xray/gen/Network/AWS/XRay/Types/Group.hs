{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Group
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Group
  ( Group (..),

    -- * Smart constructor
    mkGroup,

    -- * Lenses
    gFilterExpression,
    gInsightsConfiguration,
    gGroupARN,
    gGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.InsightsConfiguration

-- | Details and metadata for a group.
--
-- /See:/ 'mkGroup' smart constructor.
data Group = Group'
  { filterExpression :: Lude.Maybe Lude.Text,
    insightsConfiguration :: Lude.Maybe InsightsConfiguration,
    groupARN :: Lude.Maybe Lude.Text,
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Group' with the minimum fields required to make a request.
--
-- * 'filterExpression' - The filter expression defining the parameters to include traces.
-- * 'groupARN' - The Amazon Resource Name (ARN) of the group generated based on the GroupName.
-- * 'groupName' - The unique case-sensitive name of the group.
-- * 'insightsConfiguration' - The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotifcationsEnabled boolean can be set to true to enable insights notifications through Amazon EventBridge for the group.
mkGroup ::
  Group
mkGroup =
  Group'
    { filterExpression = Lude.Nothing,
      insightsConfiguration = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The filter expression defining the parameters to include traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gFilterExpression :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gFilterExpression = Lens.lens (filterExpression :: Group -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: Group)
{-# DEPRECATED gFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

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
gInsightsConfiguration :: Lens.Lens' Group (Lude.Maybe InsightsConfiguration)
gInsightsConfiguration = Lens.lens (insightsConfiguration :: Group -> Lude.Maybe InsightsConfiguration) (\s a -> s {insightsConfiguration = a} :: Group)
{-# DEPRECATED gInsightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead." #-}

-- | The Amazon Resource Name (ARN) of the group generated based on the GroupName.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupARN :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gGroupARN = Lens.lens (groupARN :: Group -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: Group)
{-# DEPRECATED gGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The unique case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gGroupName :: Lens.Lens' Group (Lude.Maybe Lude.Text)
gGroupName = Lens.lens (groupName :: Group -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: Group)
{-# DEPRECATED gGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON Group where
  parseJSON =
    Lude.withObject
      "Group"
      ( \x ->
          Group'
            Lude.<$> (x Lude..:? "FilterExpression")
            Lude.<*> (x Lude..:? "InsightsConfiguration")
            Lude.<*> (x Lude..:? "GroupARN")
            Lude.<*> (x Lude..:? "GroupName")
      )
