{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Forecast.CreateExplainability
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Explainability is only available for Forecasts and Predictors generated
-- from an AutoPredictor (CreateAutoPredictor)
--
-- Creates an Amazon Forecast Explainability.
--
-- Explainability helps you better understand how the attributes in your
-- datasets impact forecast. Amazon Forecast uses a metric called Impact
-- scores to quantify the relative impact of each attribute and determine
-- whether they increase or decrease forecast values.
--
-- To enable Forecast Explainability, your predictor must include at least
-- one of the following: related time series, item metadata, or additional
-- datasets like Holidays and the Weather Index.
--
-- CreateExplainability accepts either a Predictor ARN or Forecast ARN. To
-- receive aggregated Impact scores for all time series and time points in
-- your datasets, provide a Predictor ARN. To receive Impact scores for
-- specific time series and time points, provide a Forecast ARN.
--
-- __CreateExplainability with a Predictor ARN__
--
-- You can only have one Explainability resource per predictor. If you
-- already enabled @ExplainPredictor@ in CreateAutoPredictor, that
-- predictor already has an Explainability resource.
--
-- The following parameters are required when providing a Predictor ARN:
--
-- -   @ExplainabilityName@ - A unique name for the Explainability.
--
-- -   @ResourceArn@ - The Arn of the predictor.
--
-- -   @TimePointGranularity@ - Must be set to “ALL”.
--
-- -   @TimeSeriesGranularity@ - Must be set to “ALL”.
--
-- Do not specify a value for the following parameters:
--
-- -   @DataSource@ - Only valid when TimeSeriesGranularity is “SPECIFIC”.
--
-- -   @Schema@ - Only valid when TimeSeriesGranularity is “SPECIFIC”.
--
-- -   @StartDateTime@ - Only valid when TimePointGranularity is
--     “SPECIFIC”.
--
-- -   @EndDateTime@ - Only valid when TimePointGranularity is “SPECIFIC”.
--
-- __CreateExplainability with a Forecast ARN__
--
-- You can specify a maximum of 50 time series and 500 time points.
--
-- The following parameters are required when providing a Predictor ARN:
--
-- -   @ExplainabilityName@ - A unique name for the Explainability.
--
-- -   @ResourceArn@ - The Arn of the forecast.
--
-- -   @TimePointGranularity@ - Either “ALL” or “SPECIFIC”.
--
-- -   @TimeSeriesGranularity@ - Either “ALL” or “SPECIFIC”.
--
-- If you set TimeSeriesGranularity to “SPECIFIC”, you must also provide
-- the following:
--
-- -   @DataSource@ - The S3 location of the CSV file specifying your time
--     series.
--
-- -   @Schema@ - The Schema defines the attributes and attribute types
--     listed in the Data Source.
--
-- If you set TimePointGranularity to “SPECIFIC”, you must also provide the
-- following:
--
-- -   @StartDateTime@ - The first timestamp in the range of time points.
--
-- -   @EndDateTime@ - The last timestamp in the range of time points.
module Amazonka.Forecast.CreateExplainability
  ( -- * Creating a Request
    CreateExplainability (..),
    newCreateExplainability,

    -- * Request Lenses
    createExplainability_dataSource,
    createExplainability_enableVisualization,
    createExplainability_endDateTime,
    createExplainability_schema,
    createExplainability_startDateTime,
    createExplainability_tags,
    createExplainability_explainabilityName,
    createExplainability_resourceArn,
    createExplainability_explainabilityConfig,

    -- * Destructuring the Response
    CreateExplainabilityResponse (..),
    newCreateExplainabilityResponse,

    -- * Response Lenses
    createExplainabilityResponse_explainabilityArn,
    createExplainabilityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateExplainability' smart constructor.
data CreateExplainability = CreateExplainability'
  { dataSource :: Prelude.Maybe DataSource,
    -- | Create an Explainability visualization that is viewable within the AWS
    -- console.
    enableVisualization :: Prelude.Maybe Prelude.Bool,
    -- | If @TimePointGranularity@ is set to @SPECIFIC@, define the last time
    -- point for the Explainability.
    --
    -- Use the following timestamp format: yyyy-MM-ddTHH:mm:ss (example:
    -- 2015-01-01T20:00:00)
    endDateTime :: Prelude.Maybe Prelude.Text,
    schema :: Prelude.Maybe Schema,
    -- | If @TimePointGranularity@ is set to @SPECIFIC@, define the first point
    -- for the Explainability.
    --
    -- Use the following timestamp format: yyyy-MM-ddTHH:mm:ss (example:
    -- 2015-01-01T20:00:00)
    startDateTime :: Prelude.Maybe Prelude.Text,
    -- | Optional metadata to help you categorize and organize your resources.
    -- Each tag consists of a key and an optional value, both of which you
    -- define. Tag keys and values are case sensitive.
    --
    -- The following restrictions apply to tags:
    --
    -- -   For each resource, each tag key must be unique and each tag key must
    --     have one value.
    --
    -- -   Maximum number of tags per resource: 50.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8.
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8.
    --
    -- -   Accepted characters: all letters and numbers, spaces representable
    --     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
    --     across other services and resources, the character restrictions of
    --     those services also apply.
    --
    -- -   Key prefixes cannot include any upper or lowercase combination of
    --     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
    --     @aws@ as its prefix but the key does not, Forecast considers it to
    --     be a user tag and will count against the limit of 50 tags. Tags with
    --     only the key prefix of @aws@ do not count against your tags per
    --     resource limit. You cannot edit or delete tag keys with this prefix.
    tags :: Prelude.Maybe [Tag],
    -- | A unique name for the Explainability.
    explainabilityName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Predictor or Forecast used to
    -- create the Explainability.
    resourceArn :: Prelude.Text,
    -- | The configuration settings that define the granularity of time series
    -- and time points for the Explainability.
    explainabilityConfig :: ExplainabilityConfig
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExplainability' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSource', 'createExplainability_dataSource' - Undocumented member.
--
-- 'enableVisualization', 'createExplainability_enableVisualization' - Create an Explainability visualization that is viewable within the AWS
-- console.
--
-- 'endDateTime', 'createExplainability_endDateTime' - If @TimePointGranularity@ is set to @SPECIFIC@, define the last time
-- point for the Explainability.
--
-- Use the following timestamp format: yyyy-MM-ddTHH:mm:ss (example:
-- 2015-01-01T20:00:00)
--
-- 'schema', 'createExplainability_schema' - Undocumented member.
--
-- 'startDateTime', 'createExplainability_startDateTime' - If @TimePointGranularity@ is set to @SPECIFIC@, define the first point
-- for the Explainability.
--
-- Use the following timestamp format: yyyy-MM-ddTHH:mm:ss (example:
-- 2015-01-01T20:00:00)
--
-- 'tags', 'createExplainability_tags' - Optional metadata to help you categorize and organize your resources.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
--
-- 'explainabilityName', 'createExplainability_explainabilityName' - A unique name for the Explainability.
--
-- 'resourceArn', 'createExplainability_resourceArn' - The Amazon Resource Name (ARN) of the Predictor or Forecast used to
-- create the Explainability.
--
-- 'explainabilityConfig', 'createExplainability_explainabilityConfig' - The configuration settings that define the granularity of time series
-- and time points for the Explainability.
newCreateExplainability ::
  -- | 'explainabilityName'
  Prelude.Text ->
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'explainabilityConfig'
  ExplainabilityConfig ->
  CreateExplainability
newCreateExplainability
  pExplainabilityName_
  pResourceArn_
  pExplainabilityConfig_ =
    CreateExplainability'
      { dataSource = Prelude.Nothing,
        enableVisualization = Prelude.Nothing,
        endDateTime = Prelude.Nothing,
        schema = Prelude.Nothing,
        startDateTime = Prelude.Nothing,
        tags = Prelude.Nothing,
        explainabilityName = pExplainabilityName_,
        resourceArn = pResourceArn_,
        explainabilityConfig = pExplainabilityConfig_
      }

-- | Undocumented member.
createExplainability_dataSource :: Lens.Lens' CreateExplainability (Prelude.Maybe DataSource)
createExplainability_dataSource = Lens.lens (\CreateExplainability' {dataSource} -> dataSource) (\s@CreateExplainability' {} a -> s {dataSource = a} :: CreateExplainability)

-- | Create an Explainability visualization that is viewable within the AWS
-- console.
createExplainability_enableVisualization :: Lens.Lens' CreateExplainability (Prelude.Maybe Prelude.Bool)
createExplainability_enableVisualization = Lens.lens (\CreateExplainability' {enableVisualization} -> enableVisualization) (\s@CreateExplainability' {} a -> s {enableVisualization = a} :: CreateExplainability)

-- | If @TimePointGranularity@ is set to @SPECIFIC@, define the last time
-- point for the Explainability.
--
-- Use the following timestamp format: yyyy-MM-ddTHH:mm:ss (example:
-- 2015-01-01T20:00:00)
createExplainability_endDateTime :: Lens.Lens' CreateExplainability (Prelude.Maybe Prelude.Text)
createExplainability_endDateTime = Lens.lens (\CreateExplainability' {endDateTime} -> endDateTime) (\s@CreateExplainability' {} a -> s {endDateTime = a} :: CreateExplainability)

-- | Undocumented member.
createExplainability_schema :: Lens.Lens' CreateExplainability (Prelude.Maybe Schema)
createExplainability_schema = Lens.lens (\CreateExplainability' {schema} -> schema) (\s@CreateExplainability' {} a -> s {schema = a} :: CreateExplainability)

-- | If @TimePointGranularity@ is set to @SPECIFIC@, define the first point
-- for the Explainability.
--
-- Use the following timestamp format: yyyy-MM-ddTHH:mm:ss (example:
-- 2015-01-01T20:00:00)
createExplainability_startDateTime :: Lens.Lens' CreateExplainability (Prelude.Maybe Prelude.Text)
createExplainability_startDateTime = Lens.lens (\CreateExplainability' {startDateTime} -> startDateTime) (\s@CreateExplainability' {} a -> s {startDateTime = a} :: CreateExplainability)

-- | Optional metadata to help you categorize and organize your resources.
-- Each tag consists of a key and an optional value, both of which you
-- define. Tag keys and values are case sensitive.
--
-- The following restrictions apply to tags:
--
-- -   For each resource, each tag key must be unique and each tag key must
--     have one value.
--
-- -   Maximum number of tags per resource: 50.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8.
--
-- -   Maximum value length: 256 Unicode characters in UTF-8.
--
-- -   Accepted characters: all letters and numbers, spaces representable
--     in UTF-8, and + - = . _ : \/ \@. If your tagging schema is used
--     across other services and resources, the character restrictions of
--     those services also apply.
--
-- -   Key prefixes cannot include any upper or lowercase combination of
--     @aws:@ or @AWS:@. Values can have this prefix. If a tag value has
--     @aws@ as its prefix but the key does not, Forecast considers it to
--     be a user tag and will count against the limit of 50 tags. Tags with
--     only the key prefix of @aws@ do not count against your tags per
--     resource limit. You cannot edit or delete tag keys with this prefix.
createExplainability_tags :: Lens.Lens' CreateExplainability (Prelude.Maybe [Tag])
createExplainability_tags = Lens.lens (\CreateExplainability' {tags} -> tags) (\s@CreateExplainability' {} a -> s {tags = a} :: CreateExplainability) Prelude.. Lens.mapping Lens.coerced

-- | A unique name for the Explainability.
createExplainability_explainabilityName :: Lens.Lens' CreateExplainability Prelude.Text
createExplainability_explainabilityName = Lens.lens (\CreateExplainability' {explainabilityName} -> explainabilityName) (\s@CreateExplainability' {} a -> s {explainabilityName = a} :: CreateExplainability)

-- | The Amazon Resource Name (ARN) of the Predictor or Forecast used to
-- create the Explainability.
createExplainability_resourceArn :: Lens.Lens' CreateExplainability Prelude.Text
createExplainability_resourceArn = Lens.lens (\CreateExplainability' {resourceArn} -> resourceArn) (\s@CreateExplainability' {} a -> s {resourceArn = a} :: CreateExplainability)

-- | The configuration settings that define the granularity of time series
-- and time points for the Explainability.
createExplainability_explainabilityConfig :: Lens.Lens' CreateExplainability ExplainabilityConfig
createExplainability_explainabilityConfig = Lens.lens (\CreateExplainability' {explainabilityConfig} -> explainabilityConfig) (\s@CreateExplainability' {} a -> s {explainabilityConfig = a} :: CreateExplainability)

instance Core.AWSRequest CreateExplainability where
  type
    AWSResponse CreateExplainability =
      CreateExplainabilityResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateExplainabilityResponse'
            Prelude.<$> (x Data..?> "ExplainabilityArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateExplainability where
  hashWithSalt _salt CreateExplainability' {..} =
    _salt
      `Prelude.hashWithSalt` dataSource
      `Prelude.hashWithSalt` enableVisualization
      `Prelude.hashWithSalt` endDateTime
      `Prelude.hashWithSalt` schema
      `Prelude.hashWithSalt` startDateTime
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` explainabilityName
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` explainabilityConfig

instance Prelude.NFData CreateExplainability where
  rnf CreateExplainability' {..} =
    Prelude.rnf dataSource `Prelude.seq`
      Prelude.rnf enableVisualization `Prelude.seq`
        Prelude.rnf endDateTime `Prelude.seq`
          Prelude.rnf schema `Prelude.seq`
            Prelude.rnf startDateTime `Prelude.seq`
              Prelude.rnf tags `Prelude.seq`
                Prelude.rnf explainabilityName `Prelude.seq`
                  Prelude.rnf resourceArn `Prelude.seq`
                    Prelude.rnf explainabilityConfig

instance Data.ToHeaders CreateExplainability where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateExplainability" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateExplainability where
  toJSON CreateExplainability' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DataSource" Data..=) Prelude.<$> dataSource,
            ("EnableVisualization" Data..=)
              Prelude.<$> enableVisualization,
            ("EndDateTime" Data..=) Prelude.<$> endDateTime,
            ("Schema" Data..=) Prelude.<$> schema,
            ("StartDateTime" Data..=) Prelude.<$> startDateTime,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ExplainabilityName" Data..= explainabilityName),
            Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just
              ( "ExplainabilityConfig"
                  Data..= explainabilityConfig
              )
          ]
      )

instance Data.ToPath CreateExplainability where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateExplainability where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateExplainabilityResponse' smart constructor.
data CreateExplainabilityResponse = CreateExplainabilityResponse'
  { -- | The Amazon Resource Name (ARN) of the Explainability.
    explainabilityArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateExplainabilityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'explainabilityArn', 'createExplainabilityResponse_explainabilityArn' - The Amazon Resource Name (ARN) of the Explainability.
--
-- 'httpStatus', 'createExplainabilityResponse_httpStatus' - The response's http status code.
newCreateExplainabilityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateExplainabilityResponse
newCreateExplainabilityResponse pHttpStatus_ =
  CreateExplainabilityResponse'
    { explainabilityArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the Explainability.
createExplainabilityResponse_explainabilityArn :: Lens.Lens' CreateExplainabilityResponse (Prelude.Maybe Prelude.Text)
createExplainabilityResponse_explainabilityArn = Lens.lens (\CreateExplainabilityResponse' {explainabilityArn} -> explainabilityArn) (\s@CreateExplainabilityResponse' {} a -> s {explainabilityArn = a} :: CreateExplainabilityResponse)

-- | The response's http status code.
createExplainabilityResponse_httpStatus :: Lens.Lens' CreateExplainabilityResponse Prelude.Int
createExplainabilityResponse_httpStatus = Lens.lens (\CreateExplainabilityResponse' {httpStatus} -> httpStatus) (\s@CreateExplainabilityResponse' {} a -> s {httpStatus = a} :: CreateExplainabilityResponse)

instance Prelude.NFData CreateExplainabilityResponse where
  rnf CreateExplainabilityResponse' {..} =
    Prelude.rnf explainabilityArn `Prelude.seq`
      Prelude.rnf httpStatus
