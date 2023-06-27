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
-- Module      : Amazonka.Forecast.CreateWhatIfAnalysis
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- What-if analysis is a scenario modeling technique where you make a
-- hypothetical change to a time series and compare the forecasts generated
-- by these changes against the baseline, unchanged time series. It is
-- important to remember that the purpose of a what-if analysis is to
-- understand how a forecast can change given different modifications to
-- the baseline time series.
--
-- For example, imagine you are a clothing retailer who is considering an
-- end of season sale to clear space for new styles. After creating a
-- baseline forecast, you can use a what-if analysis to investigate how
-- different sales tactics might affect your goals.
--
-- You could create a scenario where everything is given a 25% markdown,
-- and another where everything is given a fixed dollar markdown. You could
-- create a scenario where the sale lasts for one week and another where
-- the sale lasts for one month. With a what-if analysis, you can compare
-- many different scenarios against each other.
--
-- Note that a what-if analysis is meant to display what the forecasting
-- model has learned and how it will behave in the scenarios that you are
-- evaluating. Do not blindly use the results of the what-if analysis to
-- make business decisions. For instance, forecasts might not be accurate
-- for novel scenarios where there is no reference available to determine
-- whether a forecast is good.
--
-- The TimeSeriesSelector object defines the items that you want in the
-- what-if analysis.
module Amazonka.Forecast.CreateWhatIfAnalysis
  ( -- * Creating a Request
    CreateWhatIfAnalysis (..),
    newCreateWhatIfAnalysis,

    -- * Request Lenses
    createWhatIfAnalysis_tags,
    createWhatIfAnalysis_timeSeriesSelector,
    createWhatIfAnalysis_whatIfAnalysisName,
    createWhatIfAnalysis_forecastArn,

    -- * Destructuring the Response
    CreateWhatIfAnalysisResponse (..),
    newCreateWhatIfAnalysisResponse,

    -- * Response Lenses
    createWhatIfAnalysisResponse_whatIfAnalysisArn,
    createWhatIfAnalysisResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Forecast.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateWhatIfAnalysis' smart constructor.
data CreateWhatIfAnalysis = CreateWhatIfAnalysis'
  { -- | A list of
    -- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
    -- to apply to the what if forecast.
    tags :: Prelude.Maybe [Tag],
    -- | Defines the set of time series that are used in the what-if analysis
    -- with a @TimeSeriesIdentifiers@ object. What-if analyses are performed
    -- only for the time series in this object.
    --
    -- The @TimeSeriesIdentifiers@ object needs the following information:
    --
    -- -   @DataSource@
    --
    -- -   @Format@
    --
    -- -   @Schema@
    timeSeriesSelector :: Prelude.Maybe TimeSeriesSelector,
    -- | The name of the what-if analysis. Each name must be unique.
    whatIfAnalysisName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the baseline forecast.
    forecastArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWhatIfAnalysis' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createWhatIfAnalysis_tags' - A list of
-- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
-- to apply to the what if forecast.
--
-- 'timeSeriesSelector', 'createWhatIfAnalysis_timeSeriesSelector' - Defines the set of time series that are used in the what-if analysis
-- with a @TimeSeriesIdentifiers@ object. What-if analyses are performed
-- only for the time series in this object.
--
-- The @TimeSeriesIdentifiers@ object needs the following information:
--
-- -   @DataSource@
--
-- -   @Format@
--
-- -   @Schema@
--
-- 'whatIfAnalysisName', 'createWhatIfAnalysis_whatIfAnalysisName' - The name of the what-if analysis. Each name must be unique.
--
-- 'forecastArn', 'createWhatIfAnalysis_forecastArn' - The Amazon Resource Name (ARN) of the baseline forecast.
newCreateWhatIfAnalysis ::
  -- | 'whatIfAnalysisName'
  Prelude.Text ->
  -- | 'forecastArn'
  Prelude.Text ->
  CreateWhatIfAnalysis
newCreateWhatIfAnalysis
  pWhatIfAnalysisName_
  pForecastArn_ =
    CreateWhatIfAnalysis'
      { tags = Prelude.Nothing,
        timeSeriesSelector = Prelude.Nothing,
        whatIfAnalysisName = pWhatIfAnalysisName_,
        forecastArn = pForecastArn_
      }

-- | A list of
-- <https://docs.aws.amazon.com/forecast/latest/dg/tagging-forecast-resources.html tags>
-- to apply to the what if forecast.
createWhatIfAnalysis_tags :: Lens.Lens' CreateWhatIfAnalysis (Prelude.Maybe [Tag])
createWhatIfAnalysis_tags = Lens.lens (\CreateWhatIfAnalysis' {tags} -> tags) (\s@CreateWhatIfAnalysis' {} a -> s {tags = a} :: CreateWhatIfAnalysis) Prelude.. Lens.mapping Lens.coerced

-- | Defines the set of time series that are used in the what-if analysis
-- with a @TimeSeriesIdentifiers@ object. What-if analyses are performed
-- only for the time series in this object.
--
-- The @TimeSeriesIdentifiers@ object needs the following information:
--
-- -   @DataSource@
--
-- -   @Format@
--
-- -   @Schema@
createWhatIfAnalysis_timeSeriesSelector :: Lens.Lens' CreateWhatIfAnalysis (Prelude.Maybe TimeSeriesSelector)
createWhatIfAnalysis_timeSeriesSelector = Lens.lens (\CreateWhatIfAnalysis' {timeSeriesSelector} -> timeSeriesSelector) (\s@CreateWhatIfAnalysis' {} a -> s {timeSeriesSelector = a} :: CreateWhatIfAnalysis)

-- | The name of the what-if analysis. Each name must be unique.
createWhatIfAnalysis_whatIfAnalysisName :: Lens.Lens' CreateWhatIfAnalysis Prelude.Text
createWhatIfAnalysis_whatIfAnalysisName = Lens.lens (\CreateWhatIfAnalysis' {whatIfAnalysisName} -> whatIfAnalysisName) (\s@CreateWhatIfAnalysis' {} a -> s {whatIfAnalysisName = a} :: CreateWhatIfAnalysis)

-- | The Amazon Resource Name (ARN) of the baseline forecast.
createWhatIfAnalysis_forecastArn :: Lens.Lens' CreateWhatIfAnalysis Prelude.Text
createWhatIfAnalysis_forecastArn = Lens.lens (\CreateWhatIfAnalysis' {forecastArn} -> forecastArn) (\s@CreateWhatIfAnalysis' {} a -> s {forecastArn = a} :: CreateWhatIfAnalysis)

instance Core.AWSRequest CreateWhatIfAnalysis where
  type
    AWSResponse CreateWhatIfAnalysis =
      CreateWhatIfAnalysisResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateWhatIfAnalysisResponse'
            Prelude.<$> (x Data..?> "WhatIfAnalysisArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateWhatIfAnalysis where
  hashWithSalt _salt CreateWhatIfAnalysis' {..} =
    _salt
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` timeSeriesSelector
      `Prelude.hashWithSalt` whatIfAnalysisName
      `Prelude.hashWithSalt` forecastArn

instance Prelude.NFData CreateWhatIfAnalysis where
  rnf CreateWhatIfAnalysis' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf timeSeriesSelector
      `Prelude.seq` Prelude.rnf whatIfAnalysisName
      `Prelude.seq` Prelude.rnf forecastArn

instance Data.ToHeaders CreateWhatIfAnalysis where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonForecast.CreateWhatIfAnalysis" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateWhatIfAnalysis where
  toJSON CreateWhatIfAnalysis' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("TimeSeriesSelector" Data..=)
              Prelude.<$> timeSeriesSelector,
            Prelude.Just
              ("WhatIfAnalysisName" Data..= whatIfAnalysisName),
            Prelude.Just ("ForecastArn" Data..= forecastArn)
          ]
      )

instance Data.ToPath CreateWhatIfAnalysis where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateWhatIfAnalysis where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateWhatIfAnalysisResponse' smart constructor.
data CreateWhatIfAnalysisResponse = CreateWhatIfAnalysisResponse'
  { -- | The Amazon Resource Name (ARN) of the what-if analysis.
    whatIfAnalysisArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateWhatIfAnalysisResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'whatIfAnalysisArn', 'createWhatIfAnalysisResponse_whatIfAnalysisArn' - The Amazon Resource Name (ARN) of the what-if analysis.
--
-- 'httpStatus', 'createWhatIfAnalysisResponse_httpStatus' - The response's http status code.
newCreateWhatIfAnalysisResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateWhatIfAnalysisResponse
newCreateWhatIfAnalysisResponse pHttpStatus_ =
  CreateWhatIfAnalysisResponse'
    { whatIfAnalysisArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the what-if analysis.
createWhatIfAnalysisResponse_whatIfAnalysisArn :: Lens.Lens' CreateWhatIfAnalysisResponse (Prelude.Maybe Prelude.Text)
createWhatIfAnalysisResponse_whatIfAnalysisArn = Lens.lens (\CreateWhatIfAnalysisResponse' {whatIfAnalysisArn} -> whatIfAnalysisArn) (\s@CreateWhatIfAnalysisResponse' {} a -> s {whatIfAnalysisArn = a} :: CreateWhatIfAnalysisResponse)

-- | The response's http status code.
createWhatIfAnalysisResponse_httpStatus :: Lens.Lens' CreateWhatIfAnalysisResponse Prelude.Int
createWhatIfAnalysisResponse_httpStatus = Lens.lens (\CreateWhatIfAnalysisResponse' {httpStatus} -> httpStatus) (\s@CreateWhatIfAnalysisResponse' {} a -> s {httpStatus = a} :: CreateWhatIfAnalysisResponse)

instance Prelude.NFData CreateWhatIfAnalysisResponse where
  rnf CreateWhatIfAnalysisResponse' {..} =
    Prelude.rnf whatIfAnalysisArn
      `Prelude.seq` Prelude.rnf httpStatus
