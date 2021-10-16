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
-- Module      : Network.AWS.EC2.Types.AthenaIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AthenaIntegration where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.PartitionLoadFrequency
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes integration options for Amazon Athena.
--
-- /See:/ 'newAthenaIntegration' smart constructor.
data AthenaIntegration = AthenaIntegration'
  { -- | The start date for the partition.
    partitionStartDate :: Prelude.Maybe Core.ISO8601,
    -- | The end date for the partition.
    partitionEndDate :: Prelude.Maybe Core.ISO8601,
    -- | The location in Amazon S3 to store the generated CloudFormation
    -- template.
    integrationResultS3DestinationArn :: Prelude.Text,
    -- | The schedule for adding new partitions to the table.
    partitionLoadFrequency :: PartitionLoadFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AthenaIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'partitionStartDate', 'athenaIntegration_partitionStartDate' - The start date for the partition.
--
-- 'partitionEndDate', 'athenaIntegration_partitionEndDate' - The end date for the partition.
--
-- 'integrationResultS3DestinationArn', 'athenaIntegration_integrationResultS3DestinationArn' - The location in Amazon S3 to store the generated CloudFormation
-- template.
--
-- 'partitionLoadFrequency', 'athenaIntegration_partitionLoadFrequency' - The schedule for adding new partitions to the table.
newAthenaIntegration ::
  -- | 'integrationResultS3DestinationArn'
  Prelude.Text ->
  -- | 'partitionLoadFrequency'
  PartitionLoadFrequency ->
  AthenaIntegration
newAthenaIntegration
  pIntegrationResultS3DestinationArn_
  pPartitionLoadFrequency_ =
    AthenaIntegration'
      { partitionStartDate =
          Prelude.Nothing,
        partitionEndDate = Prelude.Nothing,
        integrationResultS3DestinationArn =
          pIntegrationResultS3DestinationArn_,
        partitionLoadFrequency = pPartitionLoadFrequency_
      }

-- | The start date for the partition.
athenaIntegration_partitionStartDate :: Lens.Lens' AthenaIntegration (Prelude.Maybe Prelude.UTCTime)
athenaIntegration_partitionStartDate = Lens.lens (\AthenaIntegration' {partitionStartDate} -> partitionStartDate) (\s@AthenaIntegration' {} a -> s {partitionStartDate = a} :: AthenaIntegration) Prelude.. Lens.mapping Core._Time

-- | The end date for the partition.
athenaIntegration_partitionEndDate :: Lens.Lens' AthenaIntegration (Prelude.Maybe Prelude.UTCTime)
athenaIntegration_partitionEndDate = Lens.lens (\AthenaIntegration' {partitionEndDate} -> partitionEndDate) (\s@AthenaIntegration' {} a -> s {partitionEndDate = a} :: AthenaIntegration) Prelude.. Lens.mapping Core._Time

-- | The location in Amazon S3 to store the generated CloudFormation
-- template.
athenaIntegration_integrationResultS3DestinationArn :: Lens.Lens' AthenaIntegration Prelude.Text
athenaIntegration_integrationResultS3DestinationArn = Lens.lens (\AthenaIntegration' {integrationResultS3DestinationArn} -> integrationResultS3DestinationArn) (\s@AthenaIntegration' {} a -> s {integrationResultS3DestinationArn = a} :: AthenaIntegration)

-- | The schedule for adding new partitions to the table.
athenaIntegration_partitionLoadFrequency :: Lens.Lens' AthenaIntegration PartitionLoadFrequency
athenaIntegration_partitionLoadFrequency = Lens.lens (\AthenaIntegration' {partitionLoadFrequency} -> partitionLoadFrequency) (\s@AthenaIntegration' {} a -> s {partitionLoadFrequency = a} :: AthenaIntegration)

instance Prelude.Hashable AthenaIntegration

instance Prelude.NFData AthenaIntegration

instance Core.ToQuery AthenaIntegration where
  toQuery AthenaIntegration' {..} =
    Prelude.mconcat
      [ "PartitionStartDate" Core.=: partitionStartDate,
        "PartitionEndDate" Core.=: partitionEndDate,
        "IntegrationResultS3DestinationArn"
          Core.=: integrationResultS3DestinationArn,
        "PartitionLoadFrequency"
          Core.=: partitionLoadFrequency
      ]
