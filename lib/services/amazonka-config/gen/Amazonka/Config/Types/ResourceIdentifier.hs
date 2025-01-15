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
-- Module      : Amazonka.Config.Types.ResourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceIdentifier where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details that identify a resource that is discovered by Config,
-- including the resource type, ID, and (if available) the custom resource
-- name.
--
-- /See:/ 'newResourceIdentifier' smart constructor.
data ResourceIdentifier = ResourceIdentifier'
  { -- | The time that the resource was deleted.
    resourceDeletionTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the resource (for example, @sg-xxxxxx@).
    resourceId :: Prelude.Maybe Prelude.Text,
    -- | The custom name of the resource (if available).
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The type of resource.
    resourceType :: Prelude.Maybe ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceDeletionTime', 'resourceIdentifier_resourceDeletionTime' - The time that the resource was deleted.
--
-- 'resourceId', 'resourceIdentifier_resourceId' - The ID of the resource (for example, @sg-xxxxxx@).
--
-- 'resourceName', 'resourceIdentifier_resourceName' - The custom name of the resource (if available).
--
-- 'resourceType', 'resourceIdentifier_resourceType' - The type of resource.
newResourceIdentifier ::
  ResourceIdentifier
newResourceIdentifier =
  ResourceIdentifier'
    { resourceDeletionTime =
        Prelude.Nothing,
      resourceId = Prelude.Nothing,
      resourceName = Prelude.Nothing,
      resourceType = Prelude.Nothing
    }

-- | The time that the resource was deleted.
resourceIdentifier_resourceDeletionTime :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.UTCTime)
resourceIdentifier_resourceDeletionTime = Lens.lens (\ResourceIdentifier' {resourceDeletionTime} -> resourceDeletionTime) (\s@ResourceIdentifier' {} a -> s {resourceDeletionTime = a} :: ResourceIdentifier) Prelude.. Lens.mapping Data._Time

-- | The ID of the resource (for example, @sg-xxxxxx@).
resourceIdentifier_resourceId :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_resourceId = Lens.lens (\ResourceIdentifier' {resourceId} -> resourceId) (\s@ResourceIdentifier' {} a -> s {resourceId = a} :: ResourceIdentifier)

-- | The custom name of the resource (if available).
resourceIdentifier_resourceName :: Lens.Lens' ResourceIdentifier (Prelude.Maybe Prelude.Text)
resourceIdentifier_resourceName = Lens.lens (\ResourceIdentifier' {resourceName} -> resourceName) (\s@ResourceIdentifier' {} a -> s {resourceName = a} :: ResourceIdentifier)

-- | The type of resource.
resourceIdentifier_resourceType :: Lens.Lens' ResourceIdentifier (Prelude.Maybe ResourceType)
resourceIdentifier_resourceType = Lens.lens (\ResourceIdentifier' {resourceType} -> resourceType) (\s@ResourceIdentifier' {} a -> s {resourceType = a} :: ResourceIdentifier)

instance Data.FromJSON ResourceIdentifier where
  parseJSON =
    Data.withObject
      "ResourceIdentifier"
      ( \x ->
          ResourceIdentifier'
            Prelude.<$> (x Data..:? "resourceDeletionTime")
            Prelude.<*> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "resourceName")
            Prelude.<*> (x Data..:? "resourceType")
      )

instance Prelude.Hashable ResourceIdentifier where
  hashWithSalt _salt ResourceIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` resourceDeletionTime
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData ResourceIdentifier where
  rnf ResourceIdentifier' {..} =
    Prelude.rnf resourceDeletionTime `Prelude.seq`
      Prelude.rnf resourceId `Prelude.seq`
        Prelude.rnf resourceName `Prelude.seq`
          Prelude.rnf resourceType
