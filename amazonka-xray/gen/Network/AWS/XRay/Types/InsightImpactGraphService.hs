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
-- Module      : Network.AWS.XRay.Types.InsightImpactGraphService
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.InsightImpactGraphService where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.XRay.Types.InsightImpactGraphEdge

-- | Information about an application that processed requests, users that
-- made requests, or downstream services, resources, and applications that
-- an application used.
--
-- /See:/ 'newInsightImpactGraphService' smart constructor.
data InsightImpactGraphService = InsightImpactGraphService'
  { -- | A list of names for the service, including the canonical name.
    names :: Prelude.Maybe [Prelude.Text],
    -- | Identifier of the AWS account in which the service runs.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Identifier for the service. Unique within the service map.
    referenceId :: Prelude.Maybe Prelude.Int,
    -- | Connections to downstream services.
    edges :: Prelude.Maybe [InsightImpactGraphEdge],
    -- | The canonical name of the service.
    name :: Prelude.Maybe Prelude.Text,
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
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { names = Prelude.Nothing,
      accountId = Prelude.Nothing,
      referenceId = Prelude.Nothing,
      edges = Prelude.Nothing,
      name = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | A list of names for the service, including the canonical name.
insightImpactGraphService_names :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe [Prelude.Text])
insightImpactGraphService_names = Lens.lens (\InsightImpactGraphService' {names} -> names) (\s@InsightImpactGraphService' {} a -> s {names = a} :: InsightImpactGraphService) Prelude.. Lens.mapping Prelude._Coerce

-- | Identifier of the AWS account in which the service runs.
insightImpactGraphService_accountId :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Text)
insightImpactGraphService_accountId = Lens.lens (\InsightImpactGraphService' {accountId} -> accountId) (\s@InsightImpactGraphService' {} a -> s {accountId = a} :: InsightImpactGraphService)

-- | Identifier for the service. Unique within the service map.
insightImpactGraphService_referenceId :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Int)
insightImpactGraphService_referenceId = Lens.lens (\InsightImpactGraphService' {referenceId} -> referenceId) (\s@InsightImpactGraphService' {} a -> s {referenceId = a} :: InsightImpactGraphService)

-- | Connections to downstream services.
insightImpactGraphService_edges :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe [InsightImpactGraphEdge])
insightImpactGraphService_edges = Lens.lens (\InsightImpactGraphService' {edges} -> edges) (\s@InsightImpactGraphService' {} a -> s {edges = a} :: InsightImpactGraphService) Prelude.. Lens.mapping Prelude._Coerce

-- | The canonical name of the service.
insightImpactGraphService_name :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Text)
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
insightImpactGraphService_type :: Lens.Lens' InsightImpactGraphService (Prelude.Maybe Prelude.Text)
insightImpactGraphService_type = Lens.lens (\InsightImpactGraphService' {type'} -> type') (\s@InsightImpactGraphService' {} a -> s {type' = a} :: InsightImpactGraphService)

instance Prelude.FromJSON InsightImpactGraphService where
  parseJSON =
    Prelude.withObject
      "InsightImpactGraphService"
      ( \x ->
          InsightImpactGraphService'
            Prelude.<$> (x Prelude..:? "Names" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "AccountId")
            Prelude.<*> (x Prelude..:? "ReferenceId")
            Prelude.<*> (x Prelude..:? "Edges" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "Name")
            Prelude.<*> (x Prelude..:? "Type")
      )

instance Prelude.Hashable InsightImpactGraphService

instance Prelude.NFData InsightImpactGraphService
