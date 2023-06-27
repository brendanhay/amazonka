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
-- Module      : Amazonka.DMS.BatchStartRecommendations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the analysis of up to 20 source databases to recommend target
-- engines for each source database. This is a batch version of
-- <https://docs.aws.amazon.com/dms/latest/APIReference/API_StartRecommendations.html StartRecommendations>.
--
-- The result of analysis of each source database is reported individually
-- in the response. Because the batch request can result in a combination
-- of successful and unsuccessful actions, you should check for batch
-- errors even when the call returns an HTTP status code of @200@.
module Amazonka.DMS.BatchStartRecommendations
  ( -- * Creating a Request
    BatchStartRecommendations (..),
    newBatchStartRecommendations,

    -- * Request Lenses
    batchStartRecommendations_data,

    -- * Destructuring the Response
    BatchStartRecommendationsResponse (..),
    newBatchStartRecommendationsResponse,

    -- * Response Lenses
    batchStartRecommendationsResponse_errorEntries,
    batchStartRecommendationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newBatchStartRecommendations' smart constructor.
data BatchStartRecommendations = BatchStartRecommendations'
  { -- | Provides information about source databases to analyze. After this
    -- analysis, Fleet Advisor recommends target engines for each source
    -- database.
    data' :: Prelude.Maybe [StartRecommendationsRequestEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStartRecommendations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'data'', 'batchStartRecommendations_data' - Provides information about source databases to analyze. After this
-- analysis, Fleet Advisor recommends target engines for each source
-- database.
newBatchStartRecommendations ::
  BatchStartRecommendations
newBatchStartRecommendations =
  BatchStartRecommendations' {data' = Prelude.Nothing}

-- | Provides information about source databases to analyze. After this
-- analysis, Fleet Advisor recommends target engines for each source
-- database.
batchStartRecommendations_data :: Lens.Lens' BatchStartRecommendations (Prelude.Maybe [StartRecommendationsRequestEntry])
batchStartRecommendations_data = Lens.lens (\BatchStartRecommendations' {data'} -> data') (\s@BatchStartRecommendations' {} a -> s {data' = a} :: BatchStartRecommendations) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest BatchStartRecommendations where
  type
    AWSResponse BatchStartRecommendations =
      BatchStartRecommendationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          BatchStartRecommendationsResponse'
            Prelude.<$> (x Data..?> "ErrorEntries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable BatchStartRecommendations where
  hashWithSalt _salt BatchStartRecommendations' {..} =
    _salt `Prelude.hashWithSalt` data'

instance Prelude.NFData BatchStartRecommendations where
  rnf BatchStartRecommendations' {..} =
    Prelude.rnf data'

instance Data.ToHeaders BatchStartRecommendations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.BatchStartRecommendations" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON BatchStartRecommendations where
  toJSON BatchStartRecommendations' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Data" Data..=) Prelude.<$> data']
      )

instance Data.ToPath BatchStartRecommendations where
  toPath = Prelude.const "/"

instance Data.ToQuery BatchStartRecommendations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newBatchStartRecommendationsResponse' smart constructor.
data BatchStartRecommendationsResponse = BatchStartRecommendationsResponse'
  { -- | A list with error details about the analysis of each source database.
    errorEntries :: Prelude.Maybe [BatchStartRecommendationsErrorEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BatchStartRecommendationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorEntries', 'batchStartRecommendationsResponse_errorEntries' - A list with error details about the analysis of each source database.
--
-- 'httpStatus', 'batchStartRecommendationsResponse_httpStatus' - The response's http status code.
newBatchStartRecommendationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  BatchStartRecommendationsResponse
newBatchStartRecommendationsResponse pHttpStatus_ =
  BatchStartRecommendationsResponse'
    { errorEntries =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list with error details about the analysis of each source database.
batchStartRecommendationsResponse_errorEntries :: Lens.Lens' BatchStartRecommendationsResponse (Prelude.Maybe [BatchStartRecommendationsErrorEntry])
batchStartRecommendationsResponse_errorEntries = Lens.lens (\BatchStartRecommendationsResponse' {errorEntries} -> errorEntries) (\s@BatchStartRecommendationsResponse' {} a -> s {errorEntries = a} :: BatchStartRecommendationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
batchStartRecommendationsResponse_httpStatus :: Lens.Lens' BatchStartRecommendationsResponse Prelude.Int
batchStartRecommendationsResponse_httpStatus = Lens.lens (\BatchStartRecommendationsResponse' {httpStatus} -> httpStatus) (\s@BatchStartRecommendationsResponse' {} a -> s {httpStatus = a} :: BatchStartRecommendationsResponse)

instance
  Prelude.NFData
    BatchStartRecommendationsResponse
  where
  rnf BatchStartRecommendationsResponse' {..} =
    Prelude.rnf errorEntries
      `Prelude.seq` Prelude.rnf httpStatus
