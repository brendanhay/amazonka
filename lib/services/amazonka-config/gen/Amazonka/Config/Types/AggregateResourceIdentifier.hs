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
-- Module      : Amazonka.Config.Types.AggregateResourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.AggregateResourceIdentifier where

import Amazonka.Config.Types.ResourceType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details that identify a resource that is collected by Config
-- aggregator, including the resource type, ID, (if available) the custom
-- resource name, the source account, and source region.
--
-- /See:/ 'newAggregateResourceIdentifier' smart constructor.
data AggregateResourceIdentifier = AggregateResourceIdentifier'
  { -- | The name of the Amazon Web Services resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account ID of the source account.
    sourceAccountId :: Prelude.Text,
    -- | The source region where data is aggregated.
    sourceRegion :: Prelude.Text,
    -- | The ID of the Amazon Web Services resource.
    resourceId :: Prelude.Text,
    -- | The type of the Amazon Web Services resource.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'aggregateResourceIdentifier_resourceName' - The name of the Amazon Web Services resource.
--
-- 'sourceAccountId', 'aggregateResourceIdentifier_sourceAccountId' - The 12-digit account ID of the source account.
--
-- 'sourceRegion', 'aggregateResourceIdentifier_sourceRegion' - The source region where data is aggregated.
--
-- 'resourceId', 'aggregateResourceIdentifier_resourceId' - The ID of the Amazon Web Services resource.
--
-- 'resourceType', 'aggregateResourceIdentifier_resourceType' - The type of the Amazon Web Services resource.
newAggregateResourceIdentifier ::
  -- | 'sourceAccountId'
  Prelude.Text ->
  -- | 'sourceRegion'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  -- | 'resourceType'
  ResourceType ->
  AggregateResourceIdentifier
newAggregateResourceIdentifier
  pSourceAccountId_
  pSourceRegion_
  pResourceId_
  pResourceType_ =
    AggregateResourceIdentifier'
      { resourceName =
          Prelude.Nothing,
        sourceAccountId = pSourceAccountId_,
        sourceRegion = pSourceRegion_,
        resourceId = pResourceId_,
        resourceType = pResourceType_
      }

-- | The name of the Amazon Web Services resource.
aggregateResourceIdentifier_resourceName :: Lens.Lens' AggregateResourceIdentifier (Prelude.Maybe Prelude.Text)
aggregateResourceIdentifier_resourceName = Lens.lens (\AggregateResourceIdentifier' {resourceName} -> resourceName) (\s@AggregateResourceIdentifier' {} a -> s {resourceName = a} :: AggregateResourceIdentifier)

-- | The 12-digit account ID of the source account.
aggregateResourceIdentifier_sourceAccountId :: Lens.Lens' AggregateResourceIdentifier Prelude.Text
aggregateResourceIdentifier_sourceAccountId = Lens.lens (\AggregateResourceIdentifier' {sourceAccountId} -> sourceAccountId) (\s@AggregateResourceIdentifier' {} a -> s {sourceAccountId = a} :: AggregateResourceIdentifier)

-- | The source region where data is aggregated.
aggregateResourceIdentifier_sourceRegion :: Lens.Lens' AggregateResourceIdentifier Prelude.Text
aggregateResourceIdentifier_sourceRegion = Lens.lens (\AggregateResourceIdentifier' {sourceRegion} -> sourceRegion) (\s@AggregateResourceIdentifier' {} a -> s {sourceRegion = a} :: AggregateResourceIdentifier)

-- | The ID of the Amazon Web Services resource.
aggregateResourceIdentifier_resourceId :: Lens.Lens' AggregateResourceIdentifier Prelude.Text
aggregateResourceIdentifier_resourceId = Lens.lens (\AggregateResourceIdentifier' {resourceId} -> resourceId) (\s@AggregateResourceIdentifier' {} a -> s {resourceId = a} :: AggregateResourceIdentifier)

-- | The type of the Amazon Web Services resource.
aggregateResourceIdentifier_resourceType :: Lens.Lens' AggregateResourceIdentifier ResourceType
aggregateResourceIdentifier_resourceType = Lens.lens (\AggregateResourceIdentifier' {resourceType} -> resourceType) (\s@AggregateResourceIdentifier' {} a -> s {resourceType = a} :: AggregateResourceIdentifier)

instance Data.FromJSON AggregateResourceIdentifier where
  parseJSON =
    Data.withObject
      "AggregateResourceIdentifier"
      ( \x ->
          AggregateResourceIdentifier'
            Prelude.<$> (x Data..:? "ResourceName")
            Prelude.<*> (x Data..: "SourceAccountId")
            Prelude.<*> (x Data..: "SourceRegion")
            Prelude.<*> (x Data..: "ResourceId")
            Prelude.<*> (x Data..: "ResourceType")
      )

instance Prelude.Hashable AggregateResourceIdentifier where
  hashWithSalt _salt AggregateResourceIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` sourceAccountId
      `Prelude.hashWithSalt` sourceRegion
      `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData AggregateResourceIdentifier where
  rnf AggregateResourceIdentifier' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf sourceAccountId
      `Prelude.seq` Prelude.rnf sourceRegion
      `Prelude.seq` Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToJSON AggregateResourceIdentifier where
  toJSON AggregateResourceIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ResourceName" Data..=) Prelude.<$> resourceName,
            Prelude.Just
              ("SourceAccountId" Data..= sourceAccountId),
            Prelude.Just ("SourceRegion" Data..= sourceRegion),
            Prelude.Just ("ResourceId" Data..= resourceId),
            Prelude.Just ("ResourceType" Data..= resourceType)
          ]
      )
