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
-- Module      : Amazonka.FraudDetector.Types.EntityType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.FraudDetector.Types.EntityType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The entity type details.
--
-- /See:/ 'newEntityType' smart constructor.
data EntityType = EntityType'
  { -- | The entity type name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the entity type was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The entity type ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The entity type description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Timestamp of when the entity type was last updated.
    lastUpdatedTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'entityType_name' - The entity type name.
--
-- 'createdTime', 'entityType_createdTime' - Timestamp of when the entity type was created.
--
-- 'arn', 'entityType_arn' - The entity type ARN.
--
-- 'description', 'entityType_description' - The entity type description.
--
-- 'lastUpdatedTime', 'entityType_lastUpdatedTime' - Timestamp of when the entity type was last updated.
newEntityType ::
  EntityType
newEntityType =
  EntityType'
    { name = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      description = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing
    }

-- | The entity type name.
entityType_name :: Lens.Lens' EntityType (Prelude.Maybe Prelude.Text)
entityType_name = Lens.lens (\EntityType' {name} -> name) (\s@EntityType' {} a -> s {name = a} :: EntityType)

-- | Timestamp of when the entity type was created.
entityType_createdTime :: Lens.Lens' EntityType (Prelude.Maybe Prelude.Text)
entityType_createdTime = Lens.lens (\EntityType' {createdTime} -> createdTime) (\s@EntityType' {} a -> s {createdTime = a} :: EntityType)

-- | The entity type ARN.
entityType_arn :: Lens.Lens' EntityType (Prelude.Maybe Prelude.Text)
entityType_arn = Lens.lens (\EntityType' {arn} -> arn) (\s@EntityType' {} a -> s {arn = a} :: EntityType)

-- | The entity type description.
entityType_description :: Lens.Lens' EntityType (Prelude.Maybe Prelude.Text)
entityType_description = Lens.lens (\EntityType' {description} -> description) (\s@EntityType' {} a -> s {description = a} :: EntityType)

-- | Timestamp of when the entity type was last updated.
entityType_lastUpdatedTime :: Lens.Lens' EntityType (Prelude.Maybe Prelude.Text)
entityType_lastUpdatedTime = Lens.lens (\EntityType' {lastUpdatedTime} -> lastUpdatedTime) (\s@EntityType' {} a -> s {lastUpdatedTime = a} :: EntityType)

instance Data.FromJSON EntityType where
  parseJSON =
    Data.withObject
      "EntityType"
      ( \x ->
          EntityType'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "createdTime")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedTime")
      )

instance Prelude.Hashable EntityType where
  hashWithSalt _salt EntityType' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedTime

instance Prelude.NFData EntityType where
  rnf EntityType' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedTime
