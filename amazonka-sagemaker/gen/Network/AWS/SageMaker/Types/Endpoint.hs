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
-- Module      : Network.AWS.SageMaker.Types.Endpoint
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.Endpoint where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.SageMaker.Types.DataCaptureConfigSummary
import Network.AWS.SageMaker.Types.EndpointStatus
import Network.AWS.SageMaker.Types.MonitoringSchedule
import Network.AWS.SageMaker.Types.ProductionVariantSummary
import Network.AWS.SageMaker.Types.Tag

-- | A hosted endpoint for real-time inference.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { -- | A list of the production variants hosted on the endpoint. Each
    -- production variant is a model.
    productionVariants :: Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary),
    -- | A list of monitoring schedules for the endpoint. For information about
    -- model monitoring, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
    monitoringSchedules :: Prelude.Maybe [MonitoringSchedule],
    -- | If the endpoint failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A list of the tags associated with the endpoint. For more information,
    -- see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    dataCaptureConfig :: Prelude.Maybe DataCaptureConfigSummary,
    -- | The name of the endpoint.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text,
    -- | The endpoint configuration associated with the endpoint.
    endpointConfigName :: Prelude.Text,
    -- | The status of the endpoint.
    endpointStatus :: EndpointStatus,
    -- | The time that the endpoint was created.
    creationTime :: Core.POSIX,
    -- | The last time the endpoint was modified.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Endpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'productionVariants', 'endpoint_productionVariants' - A list of the production variants hosted on the endpoint. Each
-- production variant is a model.
--
-- 'monitoringSchedules', 'endpoint_monitoringSchedules' - A list of monitoring schedules for the endpoint. For information about
-- model monitoring, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
--
-- 'failureReason', 'endpoint_failureReason' - If the endpoint failed, the reason it failed.
--
-- 'tags', 'endpoint_tags' - A list of the tags associated with the endpoint. For more information,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
--
-- 'dataCaptureConfig', 'endpoint_dataCaptureConfig' - Undocumented member.
--
-- 'endpointName', 'endpoint_endpointName' - The name of the endpoint.
--
-- 'endpointArn', 'endpoint_endpointArn' - The Amazon Resource Name (ARN) of the endpoint.
--
-- 'endpointConfigName', 'endpoint_endpointConfigName' - The endpoint configuration associated with the endpoint.
--
-- 'endpointStatus', 'endpoint_endpointStatus' - The status of the endpoint.
--
-- 'creationTime', 'endpoint_creationTime' - The time that the endpoint was created.
--
-- 'lastModifiedTime', 'endpoint_lastModifiedTime' - The last time the endpoint was modified.
newEndpoint ::
  -- | 'endpointName'
  Prelude.Text ->
  -- | 'endpointArn'
  Prelude.Text ->
  -- | 'endpointConfigName'
  Prelude.Text ->
  -- | 'endpointStatus'
  EndpointStatus ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  Endpoint
newEndpoint
  pEndpointName_
  pEndpointArn_
  pEndpointConfigName_
  pEndpointStatus_
  pCreationTime_
  pLastModifiedTime_ =
    Endpoint'
      { productionVariants = Prelude.Nothing,
        monitoringSchedules = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        tags = Prelude.Nothing,
        dataCaptureConfig = Prelude.Nothing,
        endpointName = pEndpointName_,
        endpointArn = pEndpointArn_,
        endpointConfigName = pEndpointConfigName_,
        endpointStatus = pEndpointStatus_,
        creationTime = Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | A list of the production variants hosted on the endpoint. Each
-- production variant is a model.
endpoint_productionVariants :: Lens.Lens' Endpoint (Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary))
endpoint_productionVariants = Lens.lens (\Endpoint' {productionVariants} -> productionVariants) (\s@Endpoint' {} a -> s {productionVariants = a} :: Endpoint) Prelude.. Lens.mapping Lens._Coerce

-- | A list of monitoring schedules for the endpoint. For information about
-- model monitoring, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
endpoint_monitoringSchedules :: Lens.Lens' Endpoint (Prelude.Maybe [MonitoringSchedule])
endpoint_monitoringSchedules = Lens.lens (\Endpoint' {monitoringSchedules} -> monitoringSchedules) (\s@Endpoint' {} a -> s {monitoringSchedules = a} :: Endpoint) Prelude.. Lens.mapping Lens._Coerce

-- | If the endpoint failed, the reason it failed.
endpoint_failureReason :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_failureReason = Lens.lens (\Endpoint' {failureReason} -> failureReason) (\s@Endpoint' {} a -> s {failureReason = a} :: Endpoint)

-- | A list of the tags associated with the endpoint. For more information,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
endpoint_tags :: Lens.Lens' Endpoint (Prelude.Maybe [Tag])
endpoint_tags = Lens.lens (\Endpoint' {tags} -> tags) (\s@Endpoint' {} a -> s {tags = a} :: Endpoint) Prelude.. Lens.mapping Lens._Coerce

-- | Undocumented member.
endpoint_dataCaptureConfig :: Lens.Lens' Endpoint (Prelude.Maybe DataCaptureConfigSummary)
endpoint_dataCaptureConfig = Lens.lens (\Endpoint' {dataCaptureConfig} -> dataCaptureConfig) (\s@Endpoint' {} a -> s {dataCaptureConfig = a} :: Endpoint)

-- | The name of the endpoint.
endpoint_endpointName :: Lens.Lens' Endpoint Prelude.Text
endpoint_endpointName = Lens.lens (\Endpoint' {endpointName} -> endpointName) (\s@Endpoint' {} a -> s {endpointName = a} :: Endpoint)

-- | The Amazon Resource Name (ARN) of the endpoint.
endpoint_endpointArn :: Lens.Lens' Endpoint Prelude.Text
endpoint_endpointArn = Lens.lens (\Endpoint' {endpointArn} -> endpointArn) (\s@Endpoint' {} a -> s {endpointArn = a} :: Endpoint)

-- | The endpoint configuration associated with the endpoint.
endpoint_endpointConfigName :: Lens.Lens' Endpoint Prelude.Text
endpoint_endpointConfigName = Lens.lens (\Endpoint' {endpointConfigName} -> endpointConfigName) (\s@Endpoint' {} a -> s {endpointConfigName = a} :: Endpoint)

-- | The status of the endpoint.
endpoint_endpointStatus :: Lens.Lens' Endpoint EndpointStatus
endpoint_endpointStatus = Lens.lens (\Endpoint' {endpointStatus} -> endpointStatus) (\s@Endpoint' {} a -> s {endpointStatus = a} :: Endpoint)

-- | The time that the endpoint was created.
endpoint_creationTime :: Lens.Lens' Endpoint Prelude.UTCTime
endpoint_creationTime = Lens.lens (\Endpoint' {creationTime} -> creationTime) (\s@Endpoint' {} a -> s {creationTime = a} :: Endpoint) Prelude.. Core._Time

-- | The last time the endpoint was modified.
endpoint_lastModifiedTime :: Lens.Lens' Endpoint Prelude.UTCTime
endpoint_lastModifiedTime = Lens.lens (\Endpoint' {lastModifiedTime} -> lastModifiedTime) (\s@Endpoint' {} a -> s {lastModifiedTime = a} :: Endpoint) Prelude.. Core._Time

instance Core.FromJSON Endpoint where
  parseJSON =
    Core.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Core..:? "ProductionVariants")
            Prelude.<*> ( x Core..:? "MonitoringSchedules"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "FailureReason")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "DataCaptureConfig")
            Prelude.<*> (x Core..: "EndpointName")
            Prelude.<*> (x Core..: "EndpointArn")
            Prelude.<*> (x Core..: "EndpointConfigName")
            Prelude.<*> (x Core..: "EndpointStatus")
            Prelude.<*> (x Core..: "CreationTime")
            Prelude.<*> (x Core..: "LastModifiedTime")
      )

instance Prelude.Hashable Endpoint

instance Prelude.NFData Endpoint
