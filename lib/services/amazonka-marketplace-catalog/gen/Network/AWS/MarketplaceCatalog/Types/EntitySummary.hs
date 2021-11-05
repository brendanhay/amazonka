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
-- Module      : Network.AWS.MarketplaceCatalog.Types.EntitySummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MarketplaceCatalog.Types.EntitySummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | This object is a container for common summary information about the
-- entity. The summary doesn\'t contain the whole entity structure, but it
-- does contain information common across all entities.
--
-- /See:/ 'newEntitySummary' smart constructor.
data EntitySummary = EntitySummary'
  { -- | The last time the entity was published, using ISO 8601 format
    -- (2018-02-27T13:45:22Z).
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The type of the entity.
    entityType :: Prelude.Maybe Prelude.Text,
    -- | The visibility status of the entity to buyers. This value can be
    -- @Public@ (everyone can view the entity), @Limited@ (the entity is
    -- visible to limited accounts only), or @Restricted@ (the entity was
    -- published and then unpublished and only existing buyers can view it).
    visibility :: Prelude.Maybe Prelude.Text,
    -- | The name for the entity. This value is not unique. It is defined by the
    -- seller.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the entity.
    entityId :: Prelude.Maybe Prelude.Text,
    -- | The ARN associated with the unique identifier for the entity.
    entityArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntitySummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'entitySummary_lastModifiedDate' - The last time the entity was published, using ISO 8601 format
-- (2018-02-27T13:45:22Z).
--
-- 'entityType', 'entitySummary_entityType' - The type of the entity.
--
-- 'visibility', 'entitySummary_visibility' - The visibility status of the entity to buyers. This value can be
-- @Public@ (everyone can view the entity), @Limited@ (the entity is
-- visible to limited accounts only), or @Restricted@ (the entity was
-- published and then unpublished and only existing buyers can view it).
--
-- 'name', 'entitySummary_name' - The name for the entity. This value is not unique. It is defined by the
-- seller.
--
-- 'entityId', 'entitySummary_entityId' - The unique identifier for the entity.
--
-- 'entityArn', 'entitySummary_entityArn' - The ARN associated with the unique identifier for the entity.
newEntitySummary ::
  EntitySummary
newEntitySummary =
  EntitySummary'
    { lastModifiedDate = Prelude.Nothing,
      entityType = Prelude.Nothing,
      visibility = Prelude.Nothing,
      name = Prelude.Nothing,
      entityId = Prelude.Nothing,
      entityArn = Prelude.Nothing
    }

-- | The last time the entity was published, using ISO 8601 format
-- (2018-02-27T13:45:22Z).
entitySummary_lastModifiedDate :: Lens.Lens' EntitySummary (Prelude.Maybe Prelude.Text)
entitySummary_lastModifiedDate = Lens.lens (\EntitySummary' {lastModifiedDate} -> lastModifiedDate) (\s@EntitySummary' {} a -> s {lastModifiedDate = a} :: EntitySummary)

-- | The type of the entity.
entitySummary_entityType :: Lens.Lens' EntitySummary (Prelude.Maybe Prelude.Text)
entitySummary_entityType = Lens.lens (\EntitySummary' {entityType} -> entityType) (\s@EntitySummary' {} a -> s {entityType = a} :: EntitySummary)

-- | The visibility status of the entity to buyers. This value can be
-- @Public@ (everyone can view the entity), @Limited@ (the entity is
-- visible to limited accounts only), or @Restricted@ (the entity was
-- published and then unpublished and only existing buyers can view it).
entitySummary_visibility :: Lens.Lens' EntitySummary (Prelude.Maybe Prelude.Text)
entitySummary_visibility = Lens.lens (\EntitySummary' {visibility} -> visibility) (\s@EntitySummary' {} a -> s {visibility = a} :: EntitySummary)

-- | The name for the entity. This value is not unique. It is defined by the
-- seller.
entitySummary_name :: Lens.Lens' EntitySummary (Prelude.Maybe Prelude.Text)
entitySummary_name = Lens.lens (\EntitySummary' {name} -> name) (\s@EntitySummary' {} a -> s {name = a} :: EntitySummary)

-- | The unique identifier for the entity.
entitySummary_entityId :: Lens.Lens' EntitySummary (Prelude.Maybe Prelude.Text)
entitySummary_entityId = Lens.lens (\EntitySummary' {entityId} -> entityId) (\s@EntitySummary' {} a -> s {entityId = a} :: EntitySummary)

-- | The ARN associated with the unique identifier for the entity.
entitySummary_entityArn :: Lens.Lens' EntitySummary (Prelude.Maybe Prelude.Text)
entitySummary_entityArn = Lens.lens (\EntitySummary' {entityArn} -> entityArn) (\s@EntitySummary' {} a -> s {entityArn = a} :: EntitySummary)

instance Core.FromJSON EntitySummary where
  parseJSON =
    Core.withObject
      "EntitySummary"
      ( \x ->
          EntitySummary'
            Prelude.<$> (x Core..:? "LastModifiedDate")
            Prelude.<*> (x Core..:? "EntityType")
            Prelude.<*> (x Core..:? "Visibility")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "EntityId")
            Prelude.<*> (x Core..:? "EntityArn")
      )

instance Prelude.Hashable EntitySummary

instance Prelude.NFData EntitySummary
