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
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphService where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.InsightImpactGraphEdge

-- | Information about an application that processed requests, users that
-- made requests, or downstream services, resources, and applications that
-- an application used.
--
-- /See:/ 'newInsightImpactGraphService' smart constructor.
data InsightImpactGraphService = InsightImpactGraphService'
  { -- | A list of names for the service, including the canonical name.
    names :: Core.Maybe [Core.Text],
    -- | Identifier of the AWS account in which the service runs.
    accountId :: Core.Maybe Core.Text,
    -- | Identifier for the service. Unique within the service map.
    referenceId :: Core.Maybe Core.Int,
    -- | Connections to downstream services.
    edges :: Core.Maybe [InsightImpactGraphEdge],
    -- | The canonical name of the service.
    name :: Core.Maybe Core.Text,
    -- | Identifier for the service. Unique within the service map.
    --
    -- -   AWS Resource - The type of an AWS resource. For example,
    --     AWS::EC2::Instance for an application running on Amazon EC2 or
    --     AWS::DynamoDB::Table for an Amazon DynamoDB table that the
    --     application used.
    --
    -- -   AWS Service - The type of an AWS service. For example, AWS::DynamoDB
    --     for downstream calls to Amazon DynamoDB that didn\'t target a
    --     specific table.
    --
    -- -   AWS Service - The type of an AWS service. For example, AWS::DynamoDB
    --     for downstream calls to Amazon DynamoDB that didn\'t target a
    --     specific table.
    --
    -- -   remote - A downstream service of indeterminate type.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InsightImpactGraphService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'names', 'insightImpactGraphService_names' - A list of names for the service, including the canonical name.
--
-- 'accountId', 'insightImpactGraphService_accountId' - Identifier of the AWS account in which the service runs.
--
-- 'referenceId', 'insightImpactGraphService_referenceId' - Identifier for the service. Unique within the service map.
--
-- 'edges', 'insightImpactGraphService_edges' - Connections to downstream services.
--
-- 'name', 'insightImpactGraphService_name' - The canonical name of the service.
--
-- 'type'', 'insightImpactGraphService_type' - Identifier for the service. Unique within the service map.
--
-- -   AWS Resource - The type of an AWS resource. For example,
--     AWS::EC2::Instance for an application running on Amazon EC2 or
--     AWS::DynamoDB::Table for an Amazon DynamoDB table that the
--     application used.
--
-- -   AWS Service - The type of an AWS service. For example, AWS::DynamoDB
--     for downstream calls to Amazon DynamoDB that didn\'t target a
--     specific table.
--
-- -   AWS Service - The type of an AWS service. For example, AWS::DynamoDB
--     for downstream calls to Amazon DynamoDB that didn\'t target a
--     specific table.
--
-- -   remote - A downstream service of indeterminate type.
newInsightImpactGraphService ::
  InsightImpactGraphService
newInsightImpactGraphService =
  InsightImpactGraphService'
    { names = Core.Nothing,
      accountId = Core.Nothing,
      referenceId = Core.Nothing,
      edges = Core.Nothing,
      name = Core.Nothing,
      type' = Core.Nothing
    }

-- | A list of names for the service, including the canonical name.
insightImpactGraphService_names :: Lens.Lens' InsightImpactGraphService (Core.Maybe [Core.Text])
insightImpactGraphService_names = Lens.lens (\InsightImpactGraphService' {names} -> names) (\s@InsightImpactGraphService' {} a -> s {names = a} :: InsightImpactGraphService) Core.. Lens.mapping Lens._Coerce

-- | Identifier of the AWS account in which the service runs.
insightImpactGraphService_accountId :: Lens.Lens' InsightImpactGraphService (Core.Maybe Core.Text)
insightImpactGraphService_accountId = Lens.lens (\InsightImpactGraphService' {accountId} -> accountId) (\s@InsightImpactGraphService' {} a -> s {accountId = a} :: InsightImpactGraphService)

-- | Identifier for the service. Unique within the service map.
insightImpactGraphService_referenceId :: Lens.Lens' InsightImpactGraphService (Core.Maybe Core.Int)
insightImpactGraphService_referenceId = Lens.lens (\InsightImpactGraphService' {referenceId} -> referenceId) (\s@InsightImpactGraphService' {} a -> s {referenceId = a} :: InsightImpactGraphService)

-- | Connections to downstream services.
insightImpactGraphService_edges :: Lens.Lens' InsightImpactGraphService (Core.Maybe [InsightImpactGraphEdge])
insightImpactGraphService_edges = Lens.lens (\InsightImpactGraphService' {edges} -> edges) (\s@InsightImpactGraphService' {} a -> s {edges = a} :: InsightImpactGraphService) Core.. Lens.mapping Lens._Coerce

-- | The canonical name of the service.
insightImpactGraphService_name :: Lens.Lens' InsightImpactGraphService (Core.Maybe Core.Text)
insightImpactGraphService_name = Lens.lens (\InsightImpactGraphService' {name} -> name) (\s@InsightImpactGraphService' {} a -> s {name = a} :: InsightImpactGraphService)

-- | Identifier for the service. Unique within the service map.
--
-- -   AWS Resource - The type of an AWS resource. For example,
--     AWS::EC2::Instance for an application running on Amazon EC2 or
--     AWS::DynamoDB::Table for an Amazon DynamoDB table that the
--     application used.
--
-- -   AWS Service - The type of an AWS service. For example, AWS::DynamoDB
--     for downstream calls to Amazon DynamoDB that didn\'t target a
--     specific table.
--
-- -   AWS Service - The type of an AWS service. For example, AWS::DynamoDB
--     for downstream calls to Amazon DynamoDB that didn\'t target a
--     specific table.
--
-- -   remote - A downstream service of indeterminate type.
insightImpactGraphService_type :: Lens.Lens' InsightImpactGraphService (Core.Maybe Core.Text)
insightImpactGraphService_type = Lens.lens (\InsightImpactGraphService' {type'} -> type') (\s@InsightImpactGraphService' {} a -> s {type' = a} :: InsightImpactGraphService)

instance Core.FromJSON InsightImpactGraphService where
  parseJSON =
    Core.withObject
      "InsightImpactGraphService"
      ( \x ->
          InsightImpactGraphService'
            Core.<$> (x Core..:? "Names" Core..!= Core.mempty)
            Core.<*> (x Core..:? "AccountId")
            Core.<*> (x Core..:? "ReferenceId")
            Core.<*> (x Core..:? "Edges" Core..!= Core.mempty)
            Core.<*> (x Core..:? "Name")
            Core.<*> (x Core..:? "Type")
      )

instance Core.Hashable InsightImpactGraphService

instance Core.NFData InsightImpactGraphService
