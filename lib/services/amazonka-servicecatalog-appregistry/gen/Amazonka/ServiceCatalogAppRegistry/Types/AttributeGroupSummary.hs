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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of a Amazon Web Services Service Catalog AppRegistry attribute
-- group.
--
-- /See:/ 'newAttributeGroupSummary' smart constructor.
data AttributeGroupSummary = AttributeGroupSummary'
  { -- | The Amazon resource name (ARN) that specifies the attribute group across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment the attribute group was
    -- created.
    creationTime :: Prelude.Maybe Data.ISO8601,
    -- | The description of the attribute group that the user provides.
    description :: Prelude.Maybe Prelude.Text,
    -- | The globally unique attribute group identifier of the attribute group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment the attribute group was
    -- last updated. This time is the same as the creationTime for a newly
    -- created attribute group.
    lastUpdateTime :: Prelude.Maybe Data.ISO8601,
    -- | The name of the attribute group.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeGroupSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'attributeGroupSummary_arn' - The Amazon resource name (ARN) that specifies the attribute group across
-- services.
--
-- 'creationTime', 'attributeGroupSummary_creationTime' - The ISO-8601 formatted timestamp of the moment the attribute group was
-- created.
--
-- 'description', 'attributeGroupSummary_description' - The description of the attribute group that the user provides.
--
-- 'id', 'attributeGroupSummary_id' - The globally unique attribute group identifier of the attribute group.
--
-- 'lastUpdateTime', 'attributeGroupSummary_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment the attribute group was
-- last updated. This time is the same as the creationTime for a newly
-- created attribute group.
--
-- 'name', 'attributeGroupSummary_name' - The name of the attribute group.
newAttributeGroupSummary ::
  AttributeGroupSummary
newAttributeGroupSummary =
  AttributeGroupSummary'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing,
      name = Prelude.Nothing
    }

-- | The Amazon resource name (ARN) that specifies the attribute group across
-- services.
attributeGroupSummary_arn :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_arn = Lens.lens (\AttributeGroupSummary' {arn} -> arn) (\s@AttributeGroupSummary' {} a -> s {arn = a} :: AttributeGroupSummary)

-- | The ISO-8601 formatted timestamp of the moment the attribute group was
-- created.
attributeGroupSummary_creationTime :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.UTCTime)
attributeGroupSummary_creationTime = Lens.lens (\AttributeGroupSummary' {creationTime} -> creationTime) (\s@AttributeGroupSummary' {} a -> s {creationTime = a} :: AttributeGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The description of the attribute group that the user provides.
attributeGroupSummary_description :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_description = Lens.lens (\AttributeGroupSummary' {description} -> description) (\s@AttributeGroupSummary' {} a -> s {description = a} :: AttributeGroupSummary)

-- | The globally unique attribute group identifier of the attribute group.
attributeGroupSummary_id :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_id = Lens.lens (\AttributeGroupSummary' {id} -> id) (\s@AttributeGroupSummary' {} a -> s {id = a} :: AttributeGroupSummary)

-- | The ISO-8601 formatted timestamp of the moment the attribute group was
-- last updated. This time is the same as the creationTime for a newly
-- created attribute group.
attributeGroupSummary_lastUpdateTime :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.UTCTime)
attributeGroupSummary_lastUpdateTime = Lens.lens (\AttributeGroupSummary' {lastUpdateTime} -> lastUpdateTime) (\s@AttributeGroupSummary' {} a -> s {lastUpdateTime = a} :: AttributeGroupSummary) Prelude.. Lens.mapping Data._Time

-- | The name of the attribute group.
attributeGroupSummary_name :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_name = Lens.lens (\AttributeGroupSummary' {name} -> name) (\s@AttributeGroupSummary' {} a -> s {name = a} :: AttributeGroupSummary)

instance Data.FromJSON AttributeGroupSummary where
  parseJSON =
    Data.withObject
      "AttributeGroupSummary"
      ( \x ->
          AttributeGroupSummary'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "lastUpdateTime")
            Prelude.<*> (x Data..:? "name")
      )

instance Prelude.Hashable AttributeGroupSummary where
  hashWithSalt _salt AttributeGroupSummary' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` lastUpdateTime
      `Prelude.hashWithSalt` name

instance Prelude.NFData AttributeGroupSummary where
  rnf AttributeGroupSummary' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf lastUpdateTime
      `Prelude.seq` Prelude.rnf name
