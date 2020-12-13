{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.GroupSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.GroupSummary
  ( GroupSummary (..),

    -- * Smart constructor
    mkGroupSummary,

    -- * Lenses
    gsFilterExpression,
    gsInsightsConfiguration,
    gsGroupARN,
    gsGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.InsightsConfiguration

-- | Details for a group without metadata.
--
-- /See:/ 'mkGroupSummary' smart constructor.
data GroupSummary = GroupSummary'
  { -- | The filter expression defining the parameters to include traces.
    filterExpression :: Lude.Maybe Lude.Text,
    -- | The structure containing configurations related to insights.
    --
    --
    --     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
    --
    --
    --     * The NotificationsEnabled boolean can be set to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
    insightsConfiguration :: Lude.Maybe InsightsConfiguration,
    -- | The ARN of the group generated based on the GroupName.
    groupARN :: Lude.Maybe Lude.Text,
    -- | The unique case-sensitive name of the group.
    groupName :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GroupSummary' with the minimum fields required to make a request.
--
-- * 'filterExpression' - The filter expression defining the parameters to include traces.
-- * 'insightsConfiguration' - The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotificationsEnabled boolean can be set to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
--
-- * 'groupARN' - The ARN of the group generated based on the GroupName.
-- * 'groupName' - The unique case-sensitive name of the group.
mkGroupSummary ::
  GroupSummary
mkGroupSummary =
  GroupSummary'
    { filterExpression = Lude.Nothing,
      insightsConfiguration = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing
    }

-- | The filter expression defining the parameters to include traces.
--
-- /Note:/ Consider using 'filterExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsFilterExpression :: Lens.Lens' GroupSummary (Lude.Maybe Lude.Text)
gsFilterExpression = Lens.lens (filterExpression :: GroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {filterExpression = a} :: GroupSummary)
{-# DEPRECATED gsFilterExpression "Use generic-lens or generic-optics with 'filterExpression' instead." #-}

-- | The structure containing configurations related to insights.
--
--
--     * The InsightsEnabled boolean can be set to true to enable insights for the group or false to disable insights for the group.
--
--
--     * The NotificationsEnabled boolean can be set to true to enable insights notifications. Notifications can only be enabled on a group with InsightsEnabled set to true.
--
--
--
-- /Note:/ Consider using 'insightsConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsInsightsConfiguration :: Lens.Lens' GroupSummary (Lude.Maybe InsightsConfiguration)
gsInsightsConfiguration = Lens.lens (insightsConfiguration :: GroupSummary -> Lude.Maybe InsightsConfiguration) (\s a -> s {insightsConfiguration = a} :: GroupSummary)
{-# DEPRECATED gsInsightsConfiguration "Use generic-lens or generic-optics with 'insightsConfiguration' instead." #-}

-- | The ARN of the group generated based on the GroupName.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGroupARN :: Lens.Lens' GroupSummary (Lude.Maybe Lude.Text)
gsGroupARN = Lens.lens (groupARN :: GroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GroupSummary)
{-# DEPRECATED gsGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The unique case-sensitive name of the group.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gsGroupName :: Lens.Lens' GroupSummary (Lude.Maybe Lude.Text)
gsGroupName = Lens.lens (groupName :: GroupSummary -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GroupSummary)
{-# DEPRECATED gsGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

instance Lude.FromJSON GroupSummary where
  parseJSON =
    Lude.withObject
      "GroupSummary"
      ( \x ->
          GroupSummary'
            Lude.<$> (x Lude..:? "FilterExpression")
            Lude.<*> (x Lude..:? "InsightsConfiguration")
            Lude.<*> (x Lude..:? "GroupARN")
            Lude.<*> (x Lude..:? "GroupName")
      )
