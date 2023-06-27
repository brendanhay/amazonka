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
-- Module      : Amazonka.QuickSight.Types.TopicSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A topic summary.
--
-- /See:/ 'newTopicSummary' smart constructor.
data TopicSummary = TopicSummary'
  { -- | The Amazon Resource Name (ARN) of the topic.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the topic.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID for the topic. This ID is unique per Amazon Web Services Region
    -- for each Amazon Web Services account.
    topicId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'topicSummary_arn' - The Amazon Resource Name (ARN) of the topic.
--
-- 'name', 'topicSummary_name' - The name of the topic.
--
-- 'topicId', 'topicSummary_topicId' - The ID for the topic. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
newTopicSummary ::
  TopicSummary
newTopicSummary =
  TopicSummary'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      topicId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the topic.
topicSummary_arn :: Lens.Lens' TopicSummary (Prelude.Maybe Prelude.Text)
topicSummary_arn = Lens.lens (\TopicSummary' {arn} -> arn) (\s@TopicSummary' {} a -> s {arn = a} :: TopicSummary)

-- | The name of the topic.
topicSummary_name :: Lens.Lens' TopicSummary (Prelude.Maybe Prelude.Text)
topicSummary_name = Lens.lens (\TopicSummary' {name} -> name) (\s@TopicSummary' {} a -> s {name = a} :: TopicSummary)

-- | The ID for the topic. This ID is unique per Amazon Web Services Region
-- for each Amazon Web Services account.
topicSummary_topicId :: Lens.Lens' TopicSummary (Prelude.Maybe Prelude.Text)
topicSummary_topicId = Lens.lens (\TopicSummary' {topicId} -> topicId) (\s@TopicSummary' {} a -> s {topicId = a} :: TopicSummary)

instance Data.FromJSON TopicSummary where
  parseJSON =
    Data.withObject
      "TopicSummary"
      ( \x ->
          TopicSummary'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "TopicId")
      )

instance Prelude.Hashable TopicSummary where
  hashWithSalt _salt TopicSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` topicId

instance Prelude.NFData TopicSummary where
  rnf TopicSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf topicId
