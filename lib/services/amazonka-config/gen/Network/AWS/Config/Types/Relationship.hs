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
-- Module      : Network.AWS.Config.Types.Relationship
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.Relationship where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The relationship of the related resource to the main resource.
--
-- /See:/ 'newRelationship' smart constructor.
data Relationship = Relationship'
  { -- | The ID of the related resource (for example, @sg-xxxxxx@).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The resource type of the related resource.
    resourceType :: Prelude.Maybe ResourceType,
    -- | The custom name of the related resource, if available.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of relationship with the related resource.
    relationshipName :: Prelude.Maybe Prelude.Text
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
-- 'resourceId', 'relationship_resourceId' - The ID of the related resource (for example, @sg-xxxxxx@).
--
-- 'resourceType', 'relationship_resourceType' - The resource type of the related resource.
--
-- 'resourceName', 'relationship_resourceName' - The custom name of the related resource, if available.
--
-- 'relationshipName', 'relationship_relationshipName' - The type of relationship with the related resource.
newRelationship ::
  Relationship
newRelationship =
  Relationship'
    { resourceId = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      relationshipName = Prelude.Nothing
    }

-- | The ID of the related resource (for example, @sg-xxxxxx@).
relationship_resourceId :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_resourceId = Lens.lens (\Relationship' {resourceId} -> resourceId) (\s@Relationship' {} a -> s {resourceId = a} :: Relationship)

-- | The resource type of the related resource.
relationship_resourceType :: Lens.Lens' Relationship (Prelude.Maybe ResourceType)
relationship_resourceType = Lens.lens (\Relationship' {resourceType} -> resourceType) (\s@Relationship' {} a -> s {resourceType = a} :: Relationship)

-- | The custom name of the related resource, if available.
relationship_resourceName :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_resourceName = Lens.lens (\Relationship' {resourceName} -> resourceName) (\s@Relationship' {} a -> s {resourceName = a} :: Relationship)

-- | The type of relationship with the related resource.
relationship_relationshipName :: Lens.Lens' Relationship (Prelude.Maybe Prelude.Text)
relationship_relationshipName = Lens.lens (\Relationship' {relationshipName} -> relationshipName) (\s@Relationship' {} a -> s {relationshipName = a} :: Relationship)

instance Core.FromJSON Relationship where
  parseJSON =
    Core.withObject
      "Relationship"
      ( \x ->
          Relationship'
            Prelude.<$> (x Core..:? "resourceId")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "resourceName")
            Prelude.<*> (x Core..:? "relationshipName")
      )

instance Prelude.Hashable Relationship

instance Prelude.NFData Relationship
