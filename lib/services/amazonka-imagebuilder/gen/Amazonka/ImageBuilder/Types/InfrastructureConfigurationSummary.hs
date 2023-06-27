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
-- Module      : Amazonka.ImageBuilder.Types.InfrastructureConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ImageBuilder.Types.InfrastructureConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The infrastructure used when building Amazon EC2 AMIs.
--
-- /See:/ 'newInfrastructureConfigurationSummary' smart constructor.
data InfrastructureConfigurationSummary = InfrastructureConfigurationSummary'
  { -- | The Amazon Resource Name (ARN) of the infrastructure configuration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date on which the infrastructure configuration was created.
    dateCreated :: Prelude.Maybe Prelude.Text,
    -- | The date on which the infrastructure configuration was last updated.
    dateUpdated :: Prelude.Maybe Prelude.Text,
    -- | The description of the infrastructure configuration.
    description :: Prelude.Maybe Prelude.Text,
    -- | The instance profile of the infrastructure configuration.
    instanceProfileName :: Prelude.Maybe Prelude.Text,
    -- | The instance types of the infrastructure configuration.
    instanceTypes :: Prelude.Maybe [Prelude.Text],
    -- | The name of the infrastructure configuration.
    name :: Prelude.Maybe Prelude.Text,
    -- | The tags attached to the image created by Image Builder.
    resourceTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The tags of the infrastructure configuration.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InfrastructureConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'infrastructureConfigurationSummary_arn' - The Amazon Resource Name (ARN) of the infrastructure configuration.
--
-- 'dateCreated', 'infrastructureConfigurationSummary_dateCreated' - The date on which the infrastructure configuration was created.
--
-- 'dateUpdated', 'infrastructureConfigurationSummary_dateUpdated' - The date on which the infrastructure configuration was last updated.
--
-- 'description', 'infrastructureConfigurationSummary_description' - The description of the infrastructure configuration.
--
-- 'instanceProfileName', 'infrastructureConfigurationSummary_instanceProfileName' - The instance profile of the infrastructure configuration.
--
-- 'instanceTypes', 'infrastructureConfigurationSummary_instanceTypes' - The instance types of the infrastructure configuration.
--
-- 'name', 'infrastructureConfigurationSummary_name' - The name of the infrastructure configuration.
--
-- 'resourceTags', 'infrastructureConfigurationSummary_resourceTags' - The tags attached to the image created by Image Builder.
--
-- 'tags', 'infrastructureConfigurationSummary_tags' - The tags of the infrastructure configuration.
newInfrastructureConfigurationSummary ::
  InfrastructureConfigurationSummary
newInfrastructureConfigurationSummary =
  InfrastructureConfigurationSummary'
    { arn =
        Prelude.Nothing,
      dateCreated = Prelude.Nothing,
      dateUpdated = Prelude.Nothing,
      description = Prelude.Nothing,
      instanceProfileName = Prelude.Nothing,
      instanceTypes = Prelude.Nothing,
      name = Prelude.Nothing,
      resourceTags = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the infrastructure configuration.
infrastructureConfigurationSummary_arn :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe Prelude.Text)
infrastructureConfigurationSummary_arn = Lens.lens (\InfrastructureConfigurationSummary' {arn} -> arn) (\s@InfrastructureConfigurationSummary' {} a -> s {arn = a} :: InfrastructureConfigurationSummary)

-- | The date on which the infrastructure configuration was created.
infrastructureConfigurationSummary_dateCreated :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe Prelude.Text)
infrastructureConfigurationSummary_dateCreated = Lens.lens (\InfrastructureConfigurationSummary' {dateCreated} -> dateCreated) (\s@InfrastructureConfigurationSummary' {} a -> s {dateCreated = a} :: InfrastructureConfigurationSummary)

-- | The date on which the infrastructure configuration was last updated.
infrastructureConfigurationSummary_dateUpdated :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe Prelude.Text)
infrastructureConfigurationSummary_dateUpdated = Lens.lens (\InfrastructureConfigurationSummary' {dateUpdated} -> dateUpdated) (\s@InfrastructureConfigurationSummary' {} a -> s {dateUpdated = a} :: InfrastructureConfigurationSummary)

-- | The description of the infrastructure configuration.
infrastructureConfigurationSummary_description :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe Prelude.Text)
infrastructureConfigurationSummary_description = Lens.lens (\InfrastructureConfigurationSummary' {description} -> description) (\s@InfrastructureConfigurationSummary' {} a -> s {description = a} :: InfrastructureConfigurationSummary)

-- | The instance profile of the infrastructure configuration.
infrastructureConfigurationSummary_instanceProfileName :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe Prelude.Text)
infrastructureConfigurationSummary_instanceProfileName = Lens.lens (\InfrastructureConfigurationSummary' {instanceProfileName} -> instanceProfileName) (\s@InfrastructureConfigurationSummary' {} a -> s {instanceProfileName = a} :: InfrastructureConfigurationSummary)

-- | The instance types of the infrastructure configuration.
infrastructureConfigurationSummary_instanceTypes :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe [Prelude.Text])
infrastructureConfigurationSummary_instanceTypes = Lens.lens (\InfrastructureConfigurationSummary' {instanceTypes} -> instanceTypes) (\s@InfrastructureConfigurationSummary' {} a -> s {instanceTypes = a} :: InfrastructureConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The name of the infrastructure configuration.
infrastructureConfigurationSummary_name :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe Prelude.Text)
infrastructureConfigurationSummary_name = Lens.lens (\InfrastructureConfigurationSummary' {name} -> name) (\s@InfrastructureConfigurationSummary' {} a -> s {name = a} :: InfrastructureConfigurationSummary)

-- | The tags attached to the image created by Image Builder.
infrastructureConfigurationSummary_resourceTags :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
infrastructureConfigurationSummary_resourceTags = Lens.lens (\InfrastructureConfigurationSummary' {resourceTags} -> resourceTags) (\s@InfrastructureConfigurationSummary' {} a -> s {resourceTags = a} :: InfrastructureConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

-- | The tags of the infrastructure configuration.
infrastructureConfigurationSummary_tags :: Lens.Lens' InfrastructureConfigurationSummary (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
infrastructureConfigurationSummary_tags = Lens.lens (\InfrastructureConfigurationSummary' {tags} -> tags) (\s@InfrastructureConfigurationSummary' {} a -> s {tags = a} :: InfrastructureConfigurationSummary) Prelude.. Lens.mapping Lens.coerced

instance
  Data.FromJSON
    InfrastructureConfigurationSummary
  where
  parseJSON =
    Data.withObject
      "InfrastructureConfigurationSummary"
      ( \x ->
          InfrastructureConfigurationSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "dateCreated")
            Prelude.<*> (x Data..:? "dateUpdated")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "instanceProfileName")
            Prelude.<*> (x Data..:? "instanceTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "resourceTags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance
  Prelude.Hashable
    InfrastructureConfigurationSummary
  where
  hashWithSalt
    _salt
    InfrastructureConfigurationSummary' {..} =
      _salt
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` dateCreated
        `Prelude.hashWithSalt` dateUpdated
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` instanceProfileName
        `Prelude.hashWithSalt` instanceTypes
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` resourceTags
        `Prelude.hashWithSalt` tags

instance
  Prelude.NFData
    InfrastructureConfigurationSummary
  where
  rnf InfrastructureConfigurationSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dateCreated
      `Prelude.seq` Prelude.rnf dateUpdated
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf instanceProfileName
      `Prelude.seq` Prelude.rnf instanceTypes
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf resourceTags
      `Prelude.seq` Prelude.rnf tags
