{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.Group
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.Group where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.InsightsConfiguration

-- | Details and metadata for a group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The unique case-sensitive name of the group.
    groupName :: Core.Maybe Core.Text,
    -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the group or false to disable insights for the group.
    --
    -- -   The NotifcationsEnabled boolean can be set to true to enable
    --     insights notifications through Amazon EventBridge for the group.
    insightsConfiguration :: Core.Maybe InsightsConfiguration,
    -- | The filter expression defining the parameters to include traces.
    filterExpression :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the group generated based on the
    -- GroupName.
    groupARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupName', 'group_groupName' - The unique case-sensitive name of the group.
--
-- 'insightsConfiguration', 'group_insightsConfiguration' - The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications through Amazon EventBridge for the group.
--
-- 'filterExpression', 'group_filterExpression' - The filter expression defining the parameters to include traces.
--
-- 'groupARN', 'group_groupARN' - The Amazon Resource Name (ARN) of the group generated based on the
-- GroupName.
newGroup ::
  Group
newGroup =
  Group'
    { groupName = Core.Nothing,
      insightsConfiguration = Core.Nothing,
      filterExpression = Core.Nothing,
      groupARN = Core.Nothing
    }

-- | The unique case-sensitive name of the group.
group_groupName :: Lens.Lens' Group (Core.Maybe Core.Text)
group_groupName = Lens.lens (\Group' {groupName} -> groupName) (\s@Group' {} a -> s {groupName = a} :: Group)

-- | The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotifcationsEnabled boolean can be set to true to enable
--     insights notifications through Amazon EventBridge for the group.
group_insightsConfiguration :: Lens.Lens' Group (Core.Maybe InsightsConfiguration)
group_insightsConfiguration = Lens.lens (\Group' {insightsConfiguration} -> insightsConfiguration) (\s@Group' {} a -> s {insightsConfiguration = a} :: Group)

-- | The filter expression defining the parameters to include traces.
group_filterExpression :: Lens.Lens' Group (Core.Maybe Core.Text)
group_filterExpression = Lens.lens (\Group' {filterExpression} -> filterExpression) (\s@Group' {} a -> s {filterExpression = a} :: Group)

-- | The Amazon Resource Name (ARN) of the group generated based on the
-- GroupName.
group_groupARN :: Lens.Lens' Group (Core.Maybe Core.Text)
group_groupARN = Lens.lens (\Group' {groupARN} -> groupARN) (\s@Group' {} a -> s {groupARN = a} :: Group)

instance Core.FromJSON Group where
  parseJSON =
    Core.withObject
      "Group"
      ( \x ->
          Group'
            Core.<$> (x Core..:? "GroupName")
            Core.<*> (x Core..:? "InsightsConfiguration")
            Core.<*> (x Core..:? "FilterExpression")
            Core.<*> (x Core..:? "GroupARN")
      )

instance Core.Hashable Group

instance Core.NFData Group
