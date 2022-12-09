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
-- Module      : Amazonka.SESV2.Types.MetricDataError
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.MetricDataError where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.QueryErrorCode

-- | An error corresponding to the unsuccessful processing of a single metric
-- data query.
--
-- /See:/ 'newMetricDataError' smart constructor.
data MetricDataError = MetricDataError'
  { -- | The query error code. Can be one of:
    --
    -- -   @INTERNAL_FAILURE@ – Amazon SES has failed to process one of the
    --     queries.
    --
    -- -   @ACCESS_DENIED@ – You have insufficient access to retrieve metrics
    --     based on the given query.
    code :: Prelude.Maybe QueryErrorCode,
    -- | The query identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The error message associated with the current query error.
    message :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetricDataError' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'metricDataError_code' - The query error code. Can be one of:
--
-- -   @INTERNAL_FAILURE@ – Amazon SES has failed to process one of the
--     queries.
--
-- -   @ACCESS_DENIED@ – You have insufficient access to retrieve metrics
--     based on the given query.
--
-- 'id', 'metricDataError_id' - The query identifier.
--
-- 'message', 'metricDataError_message' - The error message associated with the current query error.
newMetricDataError ::
  MetricDataError
newMetricDataError =
  MetricDataError'
    { code = Prelude.Nothing,
      id = Prelude.Nothing,
      message = Prelude.Nothing
    }

-- | The query error code. Can be one of:
--
-- -   @INTERNAL_FAILURE@ – Amazon SES has failed to process one of the
--     queries.
--
-- -   @ACCESS_DENIED@ – You have insufficient access to retrieve metrics
--     based on the given query.
metricDataError_code :: Lens.Lens' MetricDataError (Prelude.Maybe QueryErrorCode)
metricDataError_code = Lens.lens (\MetricDataError' {code} -> code) (\s@MetricDataError' {} a -> s {code = a} :: MetricDataError)

-- | The query identifier.
metricDataError_id :: Lens.Lens' MetricDataError (Prelude.Maybe Prelude.Text)
metricDataError_id = Lens.lens (\MetricDataError' {id} -> id) (\s@MetricDataError' {} a -> s {id = a} :: MetricDataError)

-- | The error message associated with the current query error.
metricDataError_message :: Lens.Lens' MetricDataError (Prelude.Maybe Prelude.Text)
metricDataError_message = Lens.lens (\MetricDataError' {message} -> message) (\s@MetricDataError' {} a -> s {message = a} :: MetricDataError)

instance Data.FromJSON MetricDataError where
  parseJSON =
    Data.withObject
      "MetricDataError"
      ( \x ->
          MetricDataError'
            Prelude.<$> (x Data..:? "Code")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Message")
      )

instance Prelude.Hashable MetricDataError where
  hashWithSalt _salt MetricDataError' {..} =
    _salt `Prelude.hashWithSalt` code
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` message

instance Prelude.NFData MetricDataError where
  rnf MetricDataError' {..} =
    Prelude.rnf code
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf message
