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
-- Module      : Amazonka.XRay.Types.GroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.GroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.InsightsConfiguration

-- | Details for a group without metadata.
--
-- /See:/ 'newGroupSummary' smart constructor.
data GroupSummary = GroupSummary'
  { -- | The filter expression defining the parameters to include traces.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the group generated based on the GroupName.
    groupARN :: Prelude.Maybe Prelude.Text,
    -- | The unique case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the group or false to disable insights for the group.
    --
    -- -   The NotificationsEnabled boolean can be set to true to enable
    --     insights notifications. Notifications can only be enabled on a group
    --     with InsightsEnabled set to true.
    insightsConfiguration :: Prelude.Maybe InsightsConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterExpression', 'groupSummary_filterExpression' - The filter expression defining the parameters to include traces.
--
-- 'groupARN', 'groupSummary_groupARN' - The ARN of the group generated based on the GroupName.
--
-- 'groupName', 'groupSummary_groupName' - The unique case-sensitive name of the group.
--
-- 'insightsConfiguration', 'groupSummary_insightsConfiguration' - The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotificationsEnabled boolean can be set to true to enable
--     insights notifications. Notifications can only be enabled on a group
--     with InsightsEnabled set to true.
newGroupSummary ::
  GroupSummary
newGroupSummary =
  GroupSummary'
    { filterExpression = Prelude.Nothing,
      groupARN = Prelude.Nothing,
      groupName = Prelude.Nothing,
      insightsConfiguration = Prelude.Nothing
    }

-- | The filter expression defining the parameters to include traces.
groupSummary_filterExpression :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_filterExpression = Lens.lens (\GroupSummary' {filterExpression} -> filterExpression) (\s@GroupSummary' {} a -> s {filterExpression = a} :: GroupSummary)

-- | The ARN of the group generated based on the GroupName.
groupSummary_groupARN :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_groupARN = Lens.lens (\GroupSummary' {groupARN} -> groupARN) (\s@GroupSummary' {} a -> s {groupARN = a} :: GroupSummary)

-- | The unique case-sensitive name of the group.
groupSummary_groupName :: Lens.Lens' GroupSummary (Prelude.Maybe Prelude.Text)
groupSummary_groupName = Lens.lens (\GroupSummary' {groupName} -> groupName) (\s@GroupSummary' {} a -> s {groupName = a} :: GroupSummary)

-- | The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotificationsEnabled boolean can be set to true to enable
--     insights notifications. Notifications can only be enabled on a group
--     with InsightsEnabled set to true.
groupSummary_insightsConfiguration :: Lens.Lens' GroupSummary (Prelude.Maybe InsightsConfiguration)
groupSummary_insightsConfiguration = Lens.lens (\GroupSummary' {insightsConfiguration} -> insightsConfiguration) (\s@GroupSummary' {} a -> s {insightsConfiguration = a} :: GroupSummary)

instance Data.FromJSON GroupSummary where
  parseJSON =
    Data.withObject
      "GroupSummary"
      ( \x ->
          GroupSummary'
            Prelude.<$> (x Data..:? "FilterExpression")
            Prelude.<*> (x Data..:? "GroupARN")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "InsightsConfiguration")
      )

instance Prelude.Hashable GroupSummary where
  hashWithSalt _salt GroupSummary' {..} =
    _salt `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` groupARN
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` insightsConfiguration

instance Prelude.NFData GroupSummary where
  rnf GroupSummary' {..} =
    Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf groupARN
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf insightsConfiguration
