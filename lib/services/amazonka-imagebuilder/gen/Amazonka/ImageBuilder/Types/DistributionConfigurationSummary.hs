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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.DistributionConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A high-level overview of a distribution configuration.
--
-- /See:/ 'newDistributionConfigurationSummary' smart constructor.
data DistributionConfigurationSummary = DistributionConfigurationSummary'
  { -- | The tags associated with the distribution configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the distribution configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | A list of Regions where the container image is distributed to.
    regions :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the distribution configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the distribution configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date on which the distribution configuration was updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The date on which the distribution configuration was created.
    dateCreated :: Prelude.Maybe Prelude.Text
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
-- 'tags', 'distributionConfigurationSummary_tags' - The tags associated with the distribution configuration.
--
-- 'name', 'distributionConfigurationSummary_name' - The name of the distribution configuration.
--
-- 'regions', 'distributionConfigurationSummary_regions' - A list of Regions where the container image is distributed to.
--
-- 'arn', 'distributionConfigurationSummary_arn' - The Amazon Resource Name (ARN) of the distribution configuration.
--
-- 'description', 'distributionConfigurationSummary_description' - The description of the distribution configuration.
--
-- 'dateUpdated', 'distributionConfigurationSummary_dateUpdated' - The date on which the distribution configuration was updated.
--
-- 'dateCreated', 'distributionConfigurationSummary_dateCreated' - The date on which the distribution configuration was created.
newDistributionConfigurationSummary ::
  DistributionConfigurationSummary
newDistributionConfigurationSummary =
  DistributionConfigurationSummary'
    { tags =
        Prelude.Nothing,
      name = Prelude.Nothing,
      regions = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      dateCreated = Prelude.Nothing
    }

-- | The tags associated with the distribution configuration.
distributionConfigurationSummary_tags :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
distributionConfigurationSummary_tags = Lens.lens (\DistributionConfigurationSummary' {tags} -> tags) (\s@DistributionConfigurationSummary' {} a -> s {tags = a} :: DistributionConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the distribution configuration.
distributionConfigurationSummary_name :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_name = Lens.lens (\DistributionConfigurationSummary' {name} -> name) (\s@DistributionConfigurationSummary' {} a -> s {name = a} :: DistributionConfigurationSummary)

-- | A list of Regions where the container image is distributed to.
distributionConfigurationSummary_regions :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe [Prelude.Text])
distributionConfigurationSummary_regions = Lens.lens (\DistributionConfigurationSummary' {regions} -> regions) (\s@DistributionConfigurationSummary' {} a -> s {regions = a} :: DistributionConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the distribution configuration.
distributionConfigurationSummary_arn :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_arn = Lens.lens (\DistributionConfigurationSummary' {arn} -> arn) (\s@DistributionConfigurationSummary' {} a -> s {arn = a} :: DistributionConfigurationSummary)

-- | The description of the distribution configuration.
distributionConfigurationSummary_description :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_description = Lens.lens (\DistributionConfigurationSummary' {description} -> description) (\s@DistributionConfigurationSummary' {} a -> s {description = a} :: DistributionConfigurationSummary)

-- | The date on which the distribution configuration was updated.
distributionConfigurationSummary_dateUpdated :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_dateUpdated = Lens.lens (\DistributionConfigurationSummary' {dateUpdated} -> dateUpdated) (\s@DistributionConfigurationSummary' {} a -> s {dateUpdated = a} :: DistributionConfigurationSummary)

-- | The date on which the distribution configuration was created.
distributionConfigurationSummary_dateCreated :: Lens.Lens' DistributionConfigurationSummary (Prelude.Maybe Prelude.Text)
distributionConfigurationSummary_dateCreated = Lens.lens (\DistributionConfigurationSummary' {dateCreated} -> dateCreated) (\s@DistributionConfigurationSummary' {} a -> s {dateCreated = a} :: DistributionConfigurationSummary)

instance
  Core.FromJSON
    DistributionConfigurationSummary
  where
  parseJSON =
    Core.withObject
      "DistributionConfigurationSummary"
      ( \x ->
          DistributionConfigurationSummary'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "regions" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "dateUpdated")
            Prelude.<*> (x Core..:? "dateCreated")
      )

instance
  Prelude.Hashable
    DistributionConfigurationSummary
  where
  hashWithSalt
    _salt
    DistributionConfigurationSummary' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` regions
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` dateUpdated
        `Prelude.hashWithSalt` dateCreated

instance
  Prelude.NFData
    DistributionConfigurationSummary
  where
  rnf DistributionConfigurationSummary' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf regions
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf dateCreated
