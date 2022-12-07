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
-- Module      : Amazonka.XRay.Types.Group
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.Group where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.InsightsConfiguration

-- | Details and metadata for a group.
--
-- /See:/ 'newGroup' smart constructor.
data Group = Group'
  { -- | The structure containing configurations related to insights.
    --
    -- -   The InsightsEnabled boolean can be set to true to enable insights
    --     for the group or false to disable insights for the group.
    --
    -- -   The NotificationsEnabled boolean can be set to true to enable
    --     insights notifications through Amazon EventBridge for the group.
    insightsConfiguration :: Prelude.Maybe InsightsConfiguration,
    -- | The filter expression defining the parameters to include traces.
    filterExpression :: Prelude.Maybe Prelude.Text,
    -- | The unique case-sensitive name of the group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the group generated based on the
    -- GroupName.
    groupARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Group' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'insightsConfiguration', 'group_insightsConfiguration' - The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotificationsEnabled boolean can be set to true to enable
--     insights notifications through Amazon EventBridge for the group.
--
-- 'filterExpression', 'group_filterExpression' - The filter expression defining the parameters to include traces.
--
-- 'groupName', 'group_groupName' - The unique case-sensitive name of the group.
--
-- 'groupARN', 'group_groupARN' - The Amazon Resource Name (ARN) of the group generated based on the
-- GroupName.
newGroup ::
  Group
newGroup =
  Group'
    { insightsConfiguration = Prelude.Nothing,
      filterExpression = Prelude.Nothing,
      groupName = Prelude.Nothing,
      groupARN = Prelude.Nothing
    }

-- | The structure containing configurations related to insights.
--
-- -   The InsightsEnabled boolean can be set to true to enable insights
--     for the group or false to disable insights for the group.
--
-- -   The NotificationsEnabled boolean can be set to true to enable
--     insights notifications through Amazon EventBridge for the group.
group_insightsConfiguration :: Lens.Lens' Group (Prelude.Maybe InsightsConfiguration)
group_insightsConfiguration = Lens.lens (\Group' {insightsConfiguration} -> insightsConfiguration) (\s@Group' {} a -> s {insightsConfiguration = a} :: Group)

-- | The filter expression defining the parameters to include traces.
group_filterExpression :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_filterExpression = Lens.lens (\Group' {filterExpression} -> filterExpression) (\s@Group' {} a -> s {filterExpression = a} :: Group)

-- | The unique case-sensitive name of the group.
group_groupName :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_groupName = Lens.lens (\Group' {groupName} -> groupName) (\s@Group' {} a -> s {groupName = a} :: Group)

-- | The Amazon Resource Name (ARN) of the group generated based on the
-- GroupName.
group_groupARN :: Lens.Lens' Group (Prelude.Maybe Prelude.Text)
group_groupARN = Lens.lens (\Group' {groupARN} -> groupARN) (\s@Group' {} a -> s {groupARN = a} :: Group)

instance Data.FromJSON Group where
  parseJSON =
    Data.withObject
      "Group"
      ( \x ->
          Group'
            Prelude.<$> (x Data..:? "InsightsConfiguration")
            Prelude.<*> (x Data..:? "FilterExpression")
            Prelude.<*> (x Data..:? "GroupName")
            Prelude.<*> (x Data..:? "GroupARN")
      )

instance Prelude.Hashable Group where
  hashWithSalt _salt Group' {..} =
    _salt `Prelude.hashWithSalt` insightsConfiguration
      `Prelude.hashWithSalt` filterExpression
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupARN

instance Prelude.NFData Group where
  rnf Group' {..} =
    Prelude.rnf insightsConfiguration
      `Prelude.seq` Prelude.rnf filterExpression
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupARN
