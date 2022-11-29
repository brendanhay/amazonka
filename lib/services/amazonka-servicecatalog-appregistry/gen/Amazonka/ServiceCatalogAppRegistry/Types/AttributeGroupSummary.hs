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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of a Amazon Web Services Service Catalog AppRegistry attribute
-- group.
--
-- /See:/ 'newAttributeGroupSummary' smart constructor.
data AttributeGroupSummary = AttributeGroupSummary'
  { -- | The name of the attribute group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon resource name (ARN) that specifies the attribute group across
    -- services.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The description of the attribute group that the user provides.
    description :: Prelude.Maybe Prelude.Text,
    -- | The globally unique attribute group identifier of the attribute group.
    id :: Prelude.Maybe Prelude.Text,
    -- | The ISO-8601 formatted timestamp of the moment the attribute group was
    -- created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ISO-8601 formatted timestamp of the moment the attribute group was
    -- last updated. This time is the same as the creationTime for a newly
    -- created attribute group.
    lastUpdateTime :: Prelude.Maybe Core.POSIX
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
-- 'name', 'attributeGroupSummary_name' - The name of the attribute group.
--
-- 'arn', 'attributeGroupSummary_arn' - The Amazon resource name (ARN) that specifies the attribute group across
-- services.
--
-- 'description', 'attributeGroupSummary_description' - The description of the attribute group that the user provides.
--
-- 'id', 'attributeGroupSummary_id' - The globally unique attribute group identifier of the attribute group.
--
-- 'creationTime', 'attributeGroupSummary_creationTime' - The ISO-8601 formatted timestamp of the moment the attribute group was
-- created.
--
-- 'lastUpdateTime', 'attributeGroupSummary_lastUpdateTime' - The ISO-8601 formatted timestamp of the moment the attribute group was
-- last updated. This time is the same as the creationTime for a newly
-- created attribute group.
newAttributeGroupSummary ::
  AttributeGroupSummary
newAttributeGroupSummary =
  AttributeGroupSummary'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      id = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      lastUpdateTime = Prelude.Nothing
    }

-- | The name of the attribute group.
attributeGroupSummary_name :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_name = Lens.lens (\AttributeGroupSummary' {name} -> name) (\s@AttributeGroupSummary' {} a -> s {name = a} :: AttributeGroupSummary)

-- | The Amazon resource name (ARN) that specifies the attribute group across
-- services.
attributeGroupSummary_arn :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_arn = Lens.lens (\AttributeGroupSummary' {arn} -> arn) (\s@AttributeGroupSummary' {} a -> s {arn = a} :: AttributeGroupSummary)

-- | The description of the attribute group that the user provides.
attributeGroupSummary_description :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_description = Lens.lens (\AttributeGroupSummary' {description} -> description) (\s@AttributeGroupSummary' {} a -> s {description = a} :: AttributeGroupSummary)

-- | The globally unique attribute group identifier of the attribute group.
attributeGroupSummary_id :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.Text)
attributeGroupSummary_id = Lens.lens (\AttributeGroupSummary' {id} -> id) (\s@AttributeGroupSummary' {} a -> s {id = a} :: AttributeGroupSummary)

-- | The ISO-8601 formatted timestamp of the moment the attribute group was
-- created.
attributeGroupSummary_creationTime :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.UTCTime)
attributeGroupSummary_creationTime = Lens.lens (\AttributeGroupSummary' {creationTime} -> creationTime) (\s@AttributeGroupSummary' {} a -> s {creationTime = a} :: AttributeGroupSummary) Prelude.. Lens.mapping Core._Time

-- | The ISO-8601 formatted timestamp of the moment the attribute group was
-- last updated. This time is the same as the creationTime for a newly
-- created attribute group.
attributeGroupSummary_lastUpdateTime :: Lens.Lens' AttributeGroupSummary (Prelude.Maybe Prelude.UTCTime)
attributeGroupSummary_lastUpdateTime = Lens.lens (\AttributeGroupSummary' {lastUpdateTime} -> lastUpdateTime) (\s@AttributeGroupSummary' {} a -> s {lastUpdateTime = a} :: AttributeGroupSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON AttributeGroupSummary where
  parseJSON =
    Core.withObject
      "AttributeGroupSummary"
      ( \x ->
          AttributeGroupSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "lastUpdateTime")
      )

instance Prelude.Hashable AttributeGroupSummary where
  hashWithSalt _salt AttributeGroupSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData AttributeGroupSummary where
  rnf AttributeGroupSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastUpdateTime
