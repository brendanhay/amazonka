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
-- Module      : Amazonka.Config.Types.Relationship
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.Relationship where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The relationship of the related resource to the main resource.
--
-- /See:/ 'newRelationship' smart constructor.
data Relationship = Relationship'
  { -- | The type of relationship with the related resource.
    relationshipName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the related resource (for example, @sg-xxxxxx@).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the related resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the related resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Relationship' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationshipName', 'relationship_relationshipName' - The type of relationship with the related resource.
--
-- 'resourceId', 'relationship_resourceId' - The ID of the related resource (for example, @sg-xxxxxx@).
--
-- 'resourceName', 'relationship_resourceName' - The custom name of the related resource, if available.
--
-- 'resourceType', 'relationship_resourceType' - The resource type of the related resource.
newRelationship ::
  Relationship
newRelationship =
  Relationship'
    { relationshipName = Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The type of relationship with the related resource.
relationship_relationshipName :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_relationshipName = Lens.lens (\Relationship' {relationshipName} -> relationshipName) (\s@Relationship' {} a -> s {relationshipName = a} :: Relationship)

-- | The ID of the related resource (for example, @sg-xxxxxx@).
relationship_resourceId :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_resourceId = Lens.lens (\Relationship' {resourceId} -> resourceId) (\s@Relationship' {} a -> s {resourceId = a} :: Relationship)

-- | The custom name of the related resource, if available.
relationship_resourceName :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_resourceName = Lens.lens (\Relationship' {resourceName} -> resourceName) (\s@Relationship' {} a -> s {resourceName = a} :: Relationship)

-- | The resource type of the related resource.
relationship_resourceType :: Lens.Lens' Relationship (Prelude.Maybe ResourceType)
relationship_resourceType = Lens.lens (\Relationship' {resourceType} -> resourceType) (\s@Relationship' {} a -> s {resourceType = a} :: Relationship)

instance Data.FromJSON Relationship where
  parseJSON =
    Data.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Prelude.<$> (x Data..:? "relationshipName")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable Relationship where
  hashWithSalt _salt Relationship' {..} =
    _salt
      `Prelude.hashWithSalt` relationshipName
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData Relationship where
  rnf Relationship' {..} =
    Prelude.rnf relationshipName
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf resourceType
