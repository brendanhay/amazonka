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
-- Module      : Amazonka.SageMaker.Types.Endpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.Endpoint where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.DataCaptureConfigSummary
import Amazonka.SageMaker.Types.EndpointStatus
import Amazonka.SageMaker.Types.MonitoringSchedule
import Amazonka.SageMaker.Types.ProductionVariantSummary
import Amazonka.SageMaker.Types.Tag

-- | A hosted endpoint for real-time inference.
--
-- /See:/ 'newEndpoint' smart constructor.
data Endpoint = Endpoint'
  { dataCaptureConfig :: Prelude.Maybe DataCaptureConfigSummary,
    -- | If the endpoint failed, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | A list of monitoring schedules for the endpoint. For information about
    -- model monitoring, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
    monitoringSchedules :: Prelude.Maybe [MonitoringSchedule],
    -- | A list of the production variants hosted on the endpoint. Each
    -- production variant is a model.
    productionVariants :: Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary),
    -- | A list of the shadow variants hosted on the endpoint. Each shadow
    -- variant is a model in shadow mode with production traffic replicated
    -- from the proudction variant.
    shadowProductionVariants :: Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary),
    -- | A list of the tags associated with the endpoint. For more information,
    -- see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
    -- in the /Amazon Web Services General Reference Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the endpoint.
    endpointName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the endpoint.
    endpointArn :: Prelude.Text,
    -- | The endpoint configuration associated with the endpoint.
    endpointConfigName :: Prelude.Text,
    -- | The status of the endpoint.
    endpointStatus :: EndpointStatus,
    -- | The time that the endpoint was created.
    creationTime :: Data.POSIX,
    -- | The last time the endpoint was modified.
    lastModifiedTime :: Data.POSIX
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
-- 'dataCaptureConfig', 'endpoint_dataCaptureConfig' - Undocumented member.
--
-- 'failureReason', 'endpoint_failureReason' - If the endpoint failed, the reason it failed.
--
-- 'monitoringSchedules', 'endpoint_monitoringSchedules' - A list of monitoring schedules for the endpoint. For information about
-- model monitoring, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
--
-- 'productionVariants', 'endpoint_productionVariants' - A list of the production variants hosted on the endpoint. Each
-- production variant is a model.
--
-- 'shadowProductionVariants', 'endpoint_shadowProductionVariants' - A list of the shadow variants hosted on the endpoint. Each shadow
-- variant is a model in shadow mode with production traffic replicated
-- from the proudction variant.
--
-- 'tags', 'endpoint_tags' - A list of the tags associated with the endpoint. For more information,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
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
      { dataCaptureConfig = Prelude.Nothing,
        failureReason = Prelude.Nothing,
        monitoringSchedules = Prelude.Nothing,
        productionVariants = Prelude.Nothing,
        shadowProductionVariants = Prelude.Nothing,
        tags = Prelude.Nothing,
        endpointName = pEndpointName_,
        endpointArn = pEndpointArn_,
        endpointConfigName = pEndpointConfigName_,
        endpointStatus = pEndpointStatus_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Data._Time Lens.# pLastModifiedTime_
      }

-- | Undocumented member.
endpoint_dataCaptureConfig :: Lens.Lens' Endpoint (Prelude.Maybe DataCaptureConfigSummary)
endpoint_dataCaptureConfig = Lens.lens (\Endpoint' {dataCaptureConfig} -> dataCaptureConfig) (\s@Endpoint' {} a -> s {dataCaptureConfig = a} :: Endpoint)

-- | If the endpoint failed, the reason it failed.
endpoint_failureReason :: Lens.Lens' Endpoint (Prelude.Maybe Prelude.Text)
endpoint_failureReason = Lens.lens (\Endpoint' {failureReason} -> failureReason) (\s@Endpoint' {} a -> s {failureReason = a} :: Endpoint)

-- | A list of monitoring schedules for the endpoint. For information about
-- model monitoring, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/model-monitor.html Amazon SageMaker Model Monitor>.
endpoint_monitoringSchedules :: Lens.Lens' Endpoint (Prelude.Maybe [MonitoringSchedule])
endpoint_monitoringSchedules = Lens.lens (\Endpoint' {monitoringSchedules} -> monitoringSchedules) (\s@Endpoint' {} a -> s {monitoringSchedules = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | A list of the production variants hosted on the endpoint. Each
-- production variant is a model.
endpoint_productionVariants :: Lens.Lens' Endpoint (Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary))
endpoint_productionVariants = Lens.lens (\Endpoint' {productionVariants} -> productionVariants) (\s@Endpoint' {} a -> s {productionVariants = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | A list of the shadow variants hosted on the endpoint. Each shadow
-- variant is a model in shadow mode with production traffic replicated
-- from the proudction variant.
endpoint_shadowProductionVariants :: Lens.Lens' Endpoint (Prelude.Maybe (Prelude.NonEmpty ProductionVariantSummary))
endpoint_shadowProductionVariants = Lens.lens (\Endpoint' {shadowProductionVariants} -> shadowProductionVariants) (\s@Endpoint' {} a -> s {shadowProductionVariants = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

-- | A list of the tags associated with the endpoint. For more information,
-- see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services resources>
-- in the /Amazon Web Services General Reference Guide/.
endpoint_tags :: Lens.Lens' Endpoint (Prelude.Maybe [Tag])
endpoint_tags = Lens.lens (\Endpoint' {tags} -> tags) (\s@Endpoint' {} a -> s {tags = a} :: Endpoint) Prelude.. Lens.mapping Lens.coerced

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
endpoint_creationTime = Lens.lens (\Endpoint' {creationTime} -> creationTime) (\s@Endpoint' {} a -> s {creationTime = a} :: Endpoint) Prelude.. Data._Time

-- | The last time the endpoint was modified.
endpoint_lastModifiedTime :: Lens.Lens' Endpoint Prelude.UTCTime
endpoint_lastModifiedTime = Lens.lens (\Endpoint' {lastModifiedTime} -> lastModifiedTime) (\s@Endpoint' {} a -> s {lastModifiedTime = a} :: Endpoint) Prelude.. Data._Time

instance Data.FromJSON Endpoint where
  parseJSON =
    Data.withObject
      "Endpoint"
      ( \x ->
          Endpoint'
            Prelude.<$> (x Data..:? "DataCaptureConfig")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> ( x
                            Data..:? "MonitoringSchedules"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ProductionVariants")
            Prelude.<*> (x Data..:? "ShadowProductionVariants")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "EndpointName")
            Prelude.<*> (x Data..: "EndpointArn")
            Prelude.<*> (x Data..: "EndpointConfigName")
            Prelude.<*> (x Data..: "EndpointStatus")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "LastModifiedTime")
      )

instance Prelude.Hashable Endpoint where
  hashWithSalt _salt Endpoint' {..} =
    _salt
      `Prelude.hashWithSalt` dataCaptureConfig
      `Prelude.hashWithSalt` failureReason
      `Prelude.hashWithSalt` monitoringSchedules
      `Prelude.hashWithSalt` productionVariants
      `Prelude.hashWithSalt` shadowProductionVariants
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` endpointName
      `Prelude.hashWithSalt` endpointArn
      `Prelude.hashWithSalt` endpointConfigName
      `Prelude.hashWithSalt` endpointStatus
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` lastModifiedTime

instance Prelude.NFData Endpoint where
  rnf Endpoint' {..} =
    Prelude.rnf dataCaptureConfig
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf monitoringSchedules
      `Prelude.seq` Prelude.rnf productionVariants
      `Prelude.seq` Prelude.rnf shadowProductionVariants
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf endpointName
      `Prelude.seq` Prelude.rnf endpointArn
      `Prelude.seq` Prelude.rnf endpointConfigName
      `Prelude.seq` Prelude.rnf endpointStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
