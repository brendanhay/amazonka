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
-- Module      : Amazonka.QuickSight.Types.TopicDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TopicDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DatasetMetadata

-- | A structure that describes the details of a topic, such as its name,
-- description, and associated data sets.
--
-- /See:/ 'newTopicDetails' smart constructor.
data TopicDetails = TopicDetails'
  { -- | The data sets that the topic is associated with.
    dataSets :: Prelude.Maybe [DatasetMetadata],
    -- | The description of the topic.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the topic.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TopicDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSets', 'topicDetails_dataSets' - The data sets that the topic is associated with.
--
-- 'description', 'topicDetails_description' - The description of the topic.
--
-- 'name', 'topicDetails_name' - The name of the topic.
newTopicDetails ::
  TopicDetails
newTopicDetails =
  TopicDetails'
    { dataSets = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The data sets that the topic is associated with.
topicDetails_dataSets :: Lens.Lens' TopicDetails (Prelude.Maybe [DatasetMetadata])
topicDetails_dataSets = Lens.lens (\TopicDetails' {dataSets} -> dataSets) (\s@TopicDetails' {} a -> s {dataSets = a} :: TopicDetails) Prelude.. Lens.mapping Lens.coerced

-- | The description of the topic.
topicDetails_description :: Lens.Lens' TopicDetails (Prelude.Maybe Prelude.Text)
topicDetails_description = Lens.lens (\TopicDetails' {description} -> description) (\s@TopicDetails' {} a -> s {description = a} :: TopicDetails)

-- | The name of the topic.
topicDetails_name :: Lens.Lens' TopicDetails (Prelude.Maybe Prelude.Text)
topicDetails_name = Lens.lens (\TopicDetails' {name} -> name) (\s@TopicDetails' {} a -> s {name = a} :: TopicDetails)

instance Data.FromJSON TopicDetails where
  parseJSON =
    Data.withObject
      "TopicDetails"
      ( \x ->
          TopicDetails'
            Prelude.<$> (x Data..:? "DataSets" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "Name")
      )

instance Prelude.Hashable TopicDetails where
  hashWithSalt _salt TopicDetails' {..} =
    _salt
      `Prelude.hashWithSalt` dataSets
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name

instance Prelude.NFData TopicDetails where
  rnf TopicDetails' {..} =
    Prelude.rnf dataSets
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name

instance Data.ToJSON TopicDetails where
  toJSON TopicDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSets" Data..=) Prelude.<$> dataSets,
            ("Description" Data..=) Prelude.<$> description,
            ("Name" Data..=) Prelude.<$> name
          ]
      )
