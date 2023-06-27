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
-- Module      : Amazonka.ResilienceHub.Types.LogicalResourceId
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.LogicalResourceId where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a logical resource identifier.
--
-- /See:/ 'newLogicalResourceId' smart constructor.
data LogicalResourceId = LogicalResourceId'
  { -- | The name of the Amazon Elastic Kubernetes Service cluster and namespace
    -- this resource belongs to.
    --
    -- This parameter accepts values in \"eks-cluster\/namespace\" format.
    eksSourceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the CloudFormation stack this resource belongs to.
    logicalStackName :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource group that this resource belongs to.
    resourceGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of the Terraform S3 state file this resource belongs to.
    terraformSourceName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the resource.
    identifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LogicalResourceId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eksSourceName', 'logicalResourceId_eksSourceName' - The name of the Amazon Elastic Kubernetes Service cluster and namespace
-- this resource belongs to.
--
-- This parameter accepts values in \"eks-cluster\/namespace\" format.
--
-- 'logicalStackName', 'logicalResourceId_logicalStackName' - The name of the CloudFormation stack this resource belongs to.
--
-- 'resourceGroupName', 'logicalResourceId_resourceGroupName' - The name of the resource group that this resource belongs to.
--
-- 'terraformSourceName', 'logicalResourceId_terraformSourceName' - The name of the Terraform S3 state file this resource belongs to.
--
-- 'identifier', 'logicalResourceId_identifier' - The identifier of the resource.
newLogicalResourceId ::
  -- | 'identifier'
  Prelude.Text ->
  LogicalResourceId
newLogicalResourceId pIdentifier_ =
  LogicalResourceId'
    { eksSourceName = Prelude.Nothing,
      logicalStackName = Prelude.Nothing,
      resourceGroupName = Prelude.Nothing,
      terraformSourceName = Prelude.Nothing,
      identifier = pIdentifier_
    }

-- | The name of the Amazon Elastic Kubernetes Service cluster and namespace
-- this resource belongs to.
--
-- This parameter accepts values in \"eks-cluster\/namespace\" format.
logicalResourceId_eksSourceName :: Lens.Lens' LogicalResourceId (Prelude.Maybe Prelude.Text)
logicalResourceId_eksSourceName = Lens.lens (\LogicalResourceId' {eksSourceName} -> eksSourceName) (\s@LogicalResourceId' {} a -> s {eksSourceName = a} :: LogicalResourceId)

-- | The name of the CloudFormation stack this resource belongs to.
logicalResourceId_logicalStackName :: Lens.Lens' LogicalResourceId (Prelude.Maybe Prelude.Text)
logicalResourceId_logicalStackName = Lens.lens (\LogicalResourceId' {logicalStackName} -> logicalStackName) (\s@LogicalResourceId' {} a -> s {logicalStackName = a} :: LogicalResourceId)

-- | The name of the resource group that this resource belongs to.
logicalResourceId_resourceGroupName :: Lens.Lens' LogicalResourceId (Prelude.Maybe Prelude.Text)
logicalResourceId_resourceGroupName = Lens.lens (\LogicalResourceId' {resourceGroupName} -> resourceGroupName) (\s@LogicalResourceId' {} a -> s {resourceGroupName = a} :: LogicalResourceId)

-- | The name of the Terraform S3 state file this resource belongs to.
logicalResourceId_terraformSourceName :: Lens.Lens' LogicalResourceId (Prelude.Maybe Prelude.Text)
logicalResourceId_terraformSourceName = Lens.lens (\LogicalResourceId' {terraformSourceName} -> terraformSourceName) (\s@LogicalResourceId' {} a -> s {terraformSourceName = a} :: LogicalResourceId)

-- | The identifier of the resource.
logicalResourceId_identifier :: Lens.Lens' LogicalResourceId Prelude.Text
logicalResourceId_identifier = Lens.lens (\LogicalResourceId' {identifier} -> identifier) (\s@LogicalResourceId' {} a -> s {identifier = a} :: LogicalResourceId)

instance Data.FromJSON LogicalResourceId where
  parseJSON =
    Data.withObject
      "LogicalResourceId"
      ( \x ->
          LogicalResourceId'
            Prelude.<$> (x Data..:? "eksSourceName")
            Prelude.<*> (x Data..:? "logicalStackName")
            Prelude.<*> (x Data..:? "resourceGroupName")
            Prelude.<*> (x Data..:? "terraformSourceName")
            Prelude.<*> (x Data..: "identifier")
      )

instance Prelude.Hashable LogicalResourceId where
  hashWithSalt _salt LogicalResourceId' {..} =
    _salt
      `Prelude.hashWithSalt` eksSourceName
      `Prelude.hashWithSalt` logicalStackName
      `Prelude.hashWithSalt` resourceGroupName
      `Prelude.hashWithSalt` terraformSourceName
      `Prelude.hashWithSalt` identifier

instance Prelude.NFData LogicalResourceId where
  rnf LogicalResourceId' {..} =
    Prelude.rnf eksSourceName
      `Prelude.seq` Prelude.rnf logicalStackName
      `Prelude.seq` Prelude.rnf resourceGroupName
      `Prelude.seq` Prelude.rnf terraformSourceName
      `Prelude.seq` Prelude.rnf identifier

instance Data.ToJSON LogicalResourceId where
  toJSON LogicalResourceId' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("eksSourceName" Data..=) Prelude.<$> eksSourceName,
            ("logicalStackName" Data..=)
              Prelude.<$> logicalStackName,
            ("resourceGroupName" Data..=)
              Prelude.<$> resourceGroupName,
            ("terraformSourceName" Data..=)
              Prelude.<$> terraformSourceName,
            Prelude.Just ("identifier" Data..= identifier)
          ]
      )
