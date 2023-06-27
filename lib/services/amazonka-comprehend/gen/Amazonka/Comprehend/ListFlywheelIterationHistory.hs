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
-- Module      : Amazonka.Comprehend.ListFlywheelIterationHistory
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the history of a flywheel iteration. For more
-- information about flywheels, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/flywheels-about.html Flywheel overview>
-- in the /Amazon Comprehend Developer Guide/.
module Amazonka.Comprehend.ListFlywheelIterationHistory
  ( -- * Creating a Request
    ListFlywheelIterationHistory (..),
    newListFlywheelIterationHistory,

    -- * Request Lenses
    listFlywheelIterationHistory_filter,
    listFlywheelIterationHistory_maxResults,
    listFlywheelIterationHistory_nextToken,
    listFlywheelIterationHistory_flywheelArn,

    -- * Destructuring the Response
    ListFlywheelIterationHistoryResponse (..),
    newListFlywheelIterationHistoryResponse,

    -- * Response Lenses
    listFlywheelIterationHistoryResponse_flywheelIterationPropertiesList,
    listFlywheelIterationHistoryResponse_nextToken,
    listFlywheelIterationHistoryResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListFlywheelIterationHistory' smart constructor.
data ListFlywheelIterationHistory = ListFlywheelIterationHistory'
  { -- | Filter the flywheel iteration history based on creation time.
    filter' :: Prelude.Maybe FlywheelIterationFilter,
    -- | Maximum number of iteration history results to return
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Next token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the flywheel.
    flywheelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlywheelIterationHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listFlywheelIterationHistory_filter' - Filter the flywheel iteration history based on creation time.
--
-- 'maxResults', 'listFlywheelIterationHistory_maxResults' - Maximum number of iteration history results to return
--
-- 'nextToken', 'listFlywheelIterationHistory_nextToken' - Next token
--
-- 'flywheelArn', 'listFlywheelIterationHistory_flywheelArn' - The ARN of the flywheel.
newListFlywheelIterationHistory ::
  -- | 'flywheelArn'
  Prelude.Text ->
  ListFlywheelIterationHistory
newListFlywheelIterationHistory pFlywheelArn_ =
  ListFlywheelIterationHistory'
    { filter' =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      flywheelArn = pFlywheelArn_
    }

-- | Filter the flywheel iteration history based on creation time.
listFlywheelIterationHistory_filter :: Lens.Lens' ListFlywheelIterationHistory (Prelude.Maybe FlywheelIterationFilter)
listFlywheelIterationHistory_filter = Lens.lens (\ListFlywheelIterationHistory' {filter'} -> filter') (\s@ListFlywheelIterationHistory' {} a -> s {filter' = a} :: ListFlywheelIterationHistory)

-- | Maximum number of iteration history results to return
listFlywheelIterationHistory_maxResults :: Lens.Lens' ListFlywheelIterationHistory (Prelude.Maybe Prelude.Natural)
listFlywheelIterationHistory_maxResults = Lens.lens (\ListFlywheelIterationHistory' {maxResults} -> maxResults) (\s@ListFlywheelIterationHistory' {} a -> s {maxResults = a} :: ListFlywheelIterationHistory)

-- | Next token
listFlywheelIterationHistory_nextToken :: Lens.Lens' ListFlywheelIterationHistory (Prelude.Maybe Prelude.Text)
listFlywheelIterationHistory_nextToken = Lens.lens (\ListFlywheelIterationHistory' {nextToken} -> nextToken) (\s@ListFlywheelIterationHistory' {} a -> s {nextToken = a} :: ListFlywheelIterationHistory)

-- | The ARN of the flywheel.
listFlywheelIterationHistory_flywheelArn :: Lens.Lens' ListFlywheelIterationHistory Prelude.Text
listFlywheelIterationHistory_flywheelArn = Lens.lens (\ListFlywheelIterationHistory' {flywheelArn} -> flywheelArn) (\s@ListFlywheelIterationHistory' {} a -> s {flywheelArn = a} :: ListFlywheelIterationHistory)

instance Core.AWSRequest ListFlywheelIterationHistory where
  type
    AWSResponse ListFlywheelIterationHistory =
      ListFlywheelIterationHistoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFlywheelIterationHistoryResponse'
            Prelude.<$> ( x
                            Data..?> "FlywheelIterationPropertiesList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListFlywheelIterationHistory
  where
  hashWithSalt _salt ListFlywheelIterationHistory' {..} =
    _salt
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` flywheelArn

instance Prelude.NFData ListFlywheelIterationHistory where
  rnf ListFlywheelIterationHistory' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf flywheelArn

instance Data.ToHeaders ListFlywheelIterationHistory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.ListFlywheelIterationHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFlywheelIterationHistory where
  toJSON ListFlywheelIterationHistory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filter" Data..=) Prelude.<$> filter',
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("FlywheelArn" Data..= flywheelArn)
          ]
      )

instance Data.ToPath ListFlywheelIterationHistory where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFlywheelIterationHistory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFlywheelIterationHistoryResponse' smart constructor.
data ListFlywheelIterationHistoryResponse = ListFlywheelIterationHistoryResponse'
  { -- | List of flywheel iteration properties
    flywheelIterationPropertiesList :: Prelude.Maybe [FlywheelIterationProperties],
    -- | Next token
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFlywheelIterationHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flywheelIterationPropertiesList', 'listFlywheelIterationHistoryResponse_flywheelIterationPropertiesList' - List of flywheel iteration properties
--
-- 'nextToken', 'listFlywheelIterationHistoryResponse_nextToken' - Next token
--
-- 'httpStatus', 'listFlywheelIterationHistoryResponse_httpStatus' - The response's http status code.
newListFlywheelIterationHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFlywheelIterationHistoryResponse
newListFlywheelIterationHistoryResponse pHttpStatus_ =
  ListFlywheelIterationHistoryResponse'
    { flywheelIterationPropertiesList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of flywheel iteration properties
listFlywheelIterationHistoryResponse_flywheelIterationPropertiesList :: Lens.Lens' ListFlywheelIterationHistoryResponse (Prelude.Maybe [FlywheelIterationProperties])
listFlywheelIterationHistoryResponse_flywheelIterationPropertiesList = Lens.lens (\ListFlywheelIterationHistoryResponse' {flywheelIterationPropertiesList} -> flywheelIterationPropertiesList) (\s@ListFlywheelIterationHistoryResponse' {} a -> s {flywheelIterationPropertiesList = a} :: ListFlywheelIterationHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | Next token
listFlywheelIterationHistoryResponse_nextToken :: Lens.Lens' ListFlywheelIterationHistoryResponse (Prelude.Maybe Prelude.Text)
listFlywheelIterationHistoryResponse_nextToken = Lens.lens (\ListFlywheelIterationHistoryResponse' {nextToken} -> nextToken) (\s@ListFlywheelIterationHistoryResponse' {} a -> s {nextToken = a} :: ListFlywheelIterationHistoryResponse)

-- | The response's http status code.
listFlywheelIterationHistoryResponse_httpStatus :: Lens.Lens' ListFlywheelIterationHistoryResponse Prelude.Int
listFlywheelIterationHistoryResponse_httpStatus = Lens.lens (\ListFlywheelIterationHistoryResponse' {httpStatus} -> httpStatus) (\s@ListFlywheelIterationHistoryResponse' {} a -> s {httpStatus = a} :: ListFlywheelIterationHistoryResponse)

instance
  Prelude.NFData
    ListFlywheelIterationHistoryResponse
  where
  rnf ListFlywheelIterationHistoryResponse' {..} =
    Prelude.rnf flywheelIterationPropertiesList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
