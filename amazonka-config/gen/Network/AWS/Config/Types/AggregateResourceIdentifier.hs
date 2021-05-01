{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Config.Types.AggregateResourceIdentifier
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.AggregateResourceIdentifier where

import Network.AWS.Config.Types.ResourceType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The details that identify a resource that is collected by AWS Config
-- aggregator, including the resource type, ID, (if available) the custom
-- resource name, the source account, and source region.
--
-- /See:/ 'newAggregateResourceIdentifier' smart constructor.
data AggregateResourceIdentifier = AggregateResourceIdentifier'
  { -- | The name of the AWS resource.
    resourceName :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account ID of the source account.
    sourceAccountId :: Prelude.Text,
    -- | The source region where data is aggregated.
    sourceRegion :: Prelude.Text,
    -- | The ID of the AWS resource.
    resourceId :: Prelude.Text,
    -- | The type of the AWS resource.
    resourceType :: ResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AggregateResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'aggregateResourceIdentifier_resourceName' - The name of the AWS resource.
--
-- 'sourceAccountId', 'aggregateResourceIdentifier_sourceAccountId' - The 12-digit account ID of the source account.
--
-- 'sourceRegion', 'aggregateResourceIdentifier_sourceRegion' - The source region where data is aggregated.
--
-- 'resourceId', 'aggregateResourceIdentifier_resourceId' - The ID of the AWS resource.
--
-- 'resourceType', 'aggregateResourceIdentifier_resourceType' - The type of the AWS resource.
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

-- | The name of the AWS resource.
aggregateResourceIdentifier_resourceName :: Lens.Lens' AggregateResourceIdentifier (Prelude.Maybe Prelude.Text)
aggregateResourceIdentifier_resourceName = Lens.lens (\AggregateResourceIdentifier' {resourceName} -> resourceName) (\s@AggregateResourceIdentifier' {} a -> s {resourceName = a} :: AggregateResourceIdentifier)

-- | The 12-digit account ID of the source account.
aggregateResourceIdentifier_sourceAccountId :: Lens.Lens' AggregateResourceIdentifier Prelude.Text
aggregateResourceIdentifier_sourceAccountId = Lens.lens (\AggregateResourceIdentifier' {sourceAccountId} -> sourceAccountId) (\s@AggregateResourceIdentifier' {} a -> s {sourceAccountId = a} :: AggregateResourceIdentifier)

-- | The source region where data is aggregated.
aggregateResourceIdentifier_sourceRegion :: Lens.Lens' AggregateResourceIdentifier Prelude.Text
aggregateResourceIdentifier_sourceRegion = Lens.lens (\AggregateResourceIdentifier' {sourceRegion} -> sourceRegion) (\s@AggregateResourceIdentifier' {} a -> s {sourceRegion = a} :: AggregateResourceIdentifier)

-- | The ID of the AWS resource.
aggregateResourceIdentifier_resourceId :: Lens.Lens' AggregateResourceIdentifier Prelude.Text
aggregateResourceIdentifier_resourceId = Lens.lens (\AggregateResourceIdentifier' {resourceId} -> resourceId) (\s@AggregateResourceIdentifier' {} a -> s {resourceId = a} :: AggregateResourceIdentifier)

-- | The type of the AWS resource.
aggregateResourceIdentifier_resourceType :: Lens.Lens' AggregateResourceIdentifier ResourceType
aggregateResourceIdentifier_resourceType = Lens.lens (\AggregateResourceIdentifier' {resourceType} -> resourceType) (\s@AggregateResourceIdentifier' {} a -> s {resourceType = a} :: AggregateResourceIdentifier)

instance Prelude.FromJSON AggregateResourceIdentifier where
  parseJSON =
    Prelude.withObject
      "AggregateResourceIdentifier"
      ( \x ->
          AggregateResourceIdentifier'
            Prelude.<$> (x Prelude..:? "ResourceName")
            Prelude.<*> (x Prelude..: "SourceAccountId")
            Prelude.<*> (x Prelude..: "SourceRegion")
            Prelude.<*> (x Prelude..: "ResourceId")
            Prelude.<*> (x Prelude..: "ResourceType")
      )

instance Prelude.Hashable AggregateResourceIdentifier

instance Prelude.NFData AggregateResourceIdentifier

instance Prelude.ToJSON AggregateResourceIdentifier where
  toJSON AggregateResourceIdentifier' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ResourceName" Prelude..=)
              Prelude.<$> resourceName,
            Prelude.Just
              ("SourceAccountId" Prelude..= sourceAccountId),
            Prelude.Just
              ("SourceRegion" Prelude..= sourceRegion),
            Prelude.Just ("ResourceId" Prelude..= resourceId),
            Prelude.Just
              ("ResourceType" Prelude..= resourceType)
          ]
      )
