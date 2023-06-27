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
-- Module      : Amazonka.QuickSight.Types.TopicRefreshDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicRefreshDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.TopicRefreshStatus

-- | The details about the refresh of a topic.
--
-- /See:/ 'newTopicRefreshDetails' smart constructor.
data TopicRefreshDetails = TopicRefreshDetails'
  { -- | The Amazon Resource Name (ARN) of the topic refresh.
    refreshArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the refresh, which occurs as a result of topic creation or
    -- topic update.
    refreshId :: Prelude.Maybe Prelude.Text,
    -- | The status of the refresh job that indicates whether the job is still
    -- running, completed successfully, or failed.
    refreshStatus :: Prelude.Maybe TopicRefreshStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicRefreshDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'refreshArn', 'topicRefreshDetails_refreshArn' - The Amazon Resource Name (ARN) of the topic refresh.
--
-- 'refreshId', 'topicRefreshDetails_refreshId' - The ID of the refresh, which occurs as a result of topic creation or
-- topic update.
--
-- 'refreshStatus', 'topicRefreshDetails_refreshStatus' - The status of the refresh job that indicates whether the job is still
-- running, completed successfully, or failed.
newTopicRefreshDetails ::
  TopicRefreshDetails
newTopicRefreshDetails =
  TopicRefreshDetails'
    { refreshArn = Prelude.Nothing,
      refreshId = Prelude.Nothing,
      refreshStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the topic refresh.
topicRefreshDetails_refreshArn :: Lens.Lens' TopicRefreshDetails (Prelude.Maybe Prelude.Text)
topicRefreshDetails_refreshArn = Lens.lens (\TopicRefreshDetails' {refreshArn} -> refreshArn) (\s@TopicRefreshDetails' {} a -> s {refreshArn = a} :: TopicRefreshDetails)

-- | The ID of the refresh, which occurs as a result of topic creation or
-- topic update.
topicRefreshDetails_refreshId :: Lens.Lens' TopicRefreshDetails (Prelude.Maybe Prelude.Text)
topicRefreshDetails_refreshId = Lens.lens (\TopicRefreshDetails' {refreshId} -> refreshId) (\s@TopicRefreshDetails' {} a -> s {refreshId = a} :: TopicRefreshDetails)

-- | The status of the refresh job that indicates whether the job is still
-- running, completed successfully, or failed.
topicRefreshDetails_refreshStatus :: Lens.Lens' TopicRefreshDetails (Prelude.Maybe TopicRefreshStatus)
topicRefreshDetails_refreshStatus = Lens.lens (\TopicRefreshDetails' {refreshStatus} -> refreshStatus) (\s@TopicRefreshDetails' {} a -> s {refreshStatus = a} :: TopicRefreshDetails)

instance Data.FromJSON TopicRefreshDetails where
  parseJSON =
    Data.withObject
      "TopicRefreshDetails"
      ( \x ->
          TopicRefreshDetails'
            Prelude.<$> (x Data..:? "RefreshArn")
            Prelude.<*> (x Data..:? "RefreshId")
            Prelude.<*> (x Data..:? "RefreshStatus")
      )

instance Prelude.Hashable TopicRefreshDetails where
  hashWithSalt _salt TopicRefreshDetails' {..} =
    _salt
      `Prelude.hashWithSalt` refreshArn
      `Prelude.hashWithSalt` refreshId
      `Prelude.hashWithSalt` refreshStatus

instance Prelude.NFData TopicRefreshDetails where
  rnf TopicRefreshDetails' {..} =
    Prelude.rnf refreshArn
      `Prelude.seq` Prelude.rnf refreshId
      `Prelude.seq` Prelude.rnf refreshStatus
