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
-- Module      : Amazonka.ImageBuilder.Types.DistributionConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.DistributionConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A high-level overview of a distribution configuration.
--
-- /See:/ 'newDistributionConfigurationSummary' smart constructor.
data DistributionConfigurationSummary = DistributionConfigurationSummary'
  { -- | The Amazon Resource Name (ARN) of the distribution configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date on which the distribution configuration was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The date on which the distribution configuration was updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The description of the distribution configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the distribution configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of Regions where the container image is distributed to.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | The tags associated with the distribution configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DistributionConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'distributionConfigurationSummary_arn' - The Amazon Resource Name (ARN) of the distribution configuration.
--
-- 'dateCreated', 'distributionConfigurationSummary_dateCreated' - The date on which the distribution configuration was created.
--
-- 'dateUpdated', 'distributionConfigurationSummary_dateUpdated' - The date on which the distribution configuration was updated.
--
-- 'description', 'distributionConfigurationSummary_description' - The description of the distribution configuration.
--
-- 'name', 'distributionConfigurationSummary_name' - The name of the distribution configuration.
--
-- 'regions', 'distributionConfigurationSummary_regions' - A list of Regions where the container image is distributed to.
--
-- 'tags', 'distributionConfigurationSummary_tags' - The tags associated with the distribution configuration.
newDistributionConfigurationSummary ::
  DistributionConfigurationSummary
newDistributionConfigurationSummary =
  DistributionConfigurationSummary'
    { arn =
        Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      description = Prelude.Nothing,
      name = Prelude.Nothing,
      regions = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the distribution configuration.
distributionConfigurationSummary_arn :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_arn = Lens.lens (\DistributionConfigurationSummary' {arn} -> arn) (\s@DistributionConfigurationSummary' {} a -> s {arn = a} :: DistributionConfigurationSummary)

-- | The date on which the distribution configuration was created.
distributionConfigurationSummary_dateCreated :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_dateCreated = Lens.lens (\DistributionConfigurationSummary' {dateCreated} -> dateCreated) (\s@DistributionConfigurationSummary' {} a -> s {dateCreated = a} :: DistributionConfigurationSummary)

-- | The date on which the distribution configuration was updated.
distributionConfigurationSummary_dateUpdated :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_dateUpdated = Lens.lens (\DistributionConfigurationSummary' {dateUpdated} -> dateUpdated) (\s@DistributionConfigurationSummary' {} a -> s {dateUpdated = a} :: DistributionConfigurationSummary)

-- | The description of the distribution configuration.
distributionConfigurationSummary_description :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_description = Lens.lens (\DistributionConfigurationSummary' {description} -> description) (\s@DistributionConfigurationSummary' {} a -> s {description = a} :: DistributionConfigurationSummary)

-- | The name of the distribution configuration.
distributionConfigurationSummary_name :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_name = Lens.lens (\DistributionConfigurationSummary' {name} -> name) (\s@DistributionConfigurationSummary' {} a -> s {name = a} :: DistributionConfigurationSummary)

-- | A list of Regions where the container image is distributed to.
distributionConfigurationSummary_regions :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe [Prelude.Text])
distributionConfigurationSummary_regions = Lens.lens (\DistributionConfigurationSummary' {regions} -> regions) (\s@DistributionConfigurationSummary' {} a -> s {regions = a} :: DistributionConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The tags associated with the distribution configuration.
distributionConfigurationSummary_tags :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
distributionConfigurationSummary_tags = Lens.lens (\DistributionConfigurationSummary' {tags} -> tags) (\s@DistributionConfigurationSummary' {} a -> s {tags = a} :: DistributionConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    DistributionConfigurationSummary
  where
  parseJSON =
    Data.withObject
      "DistributionConfigurationSummary"
      ( \x ->
          DistributionConfigurationSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "dateUpdated")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "regions" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    DistributionConfigurationSummary
  where
  hashWithSalt
    _salt
    DistributionConfigurationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` dateCreated
        `Prelude.hashWithSalt` dateUpdated
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` regions
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    DistributionConfigurationSummary
  where
  rnf DistributionConfigurationSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf tags
