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
-- Module      : Amazonka.SSMContacts.ListRotationOverrides
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of overrides currently specified for an on-call
-- rotation.
--
-- This operation returns paginated results.
module Amazonka.SSMContacts.ListRotationOverrides
  ( -- * Creating a Request
    ListRotationOverrides (..),
    newListRotationOverrides,

    -- * Request Lenses
    listRotationOverrides_maxResults,
    listRotationOverrides_nextToken,
    listRotationOverrides_rotationId,
    listRotationOverrides_startTime,
    listRotationOverrides_endTime,

    -- * Destructuring the Response
    ListRotationOverridesResponse (..),
    newListRotationOverridesResponse,

    -- * Response Lenses
    listRotationOverridesResponse_nextToken,
    listRotationOverridesResponse_rotationOverrides,
    listRotationOverridesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMContacts.Types

-- | /See:/ 'newListRotationOverrides' smart constructor.
data ListRotationOverrides = ListRotationOverrides'
  { -- | The maximum number of items to return for this call. The call also
    -- returns a token that you can specify in a subsequent call to get the
    -- next set of results.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A token to start the list. Use this token to get the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the rotation to retrieve information
    -- about.
    rotationId :: Prelude.Text,
    -- | The date and time for the beginning of a time range for listing
    -- overrides.
    startTime :: Data.POSIX,
    -- | The date and time for the end of a time range for listing overrides.
    endTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRotationOverrides' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRotationOverrides_maxResults' - The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
--
-- 'nextToken', 'listRotationOverrides_nextToken' - A token to start the list. Use this token to get the next set of
-- results.
--
-- 'rotationId', 'listRotationOverrides_rotationId' - The Amazon Resource Name (ARN) of the rotation to retrieve information
-- about.
--
-- 'startTime', 'listRotationOverrides_startTime' - The date and time for the beginning of a time range for listing
-- overrides.
--
-- 'endTime', 'listRotationOverrides_endTime' - The date and time for the end of a time range for listing overrides.
newListRotationOverrides ::
  -- | 'rotationId'
  Prelude.Text ->
  -- | 'startTime'
  Prelude.UTCTime ->
  -- | 'endTime'
  Prelude.UTCTime ->
  ListRotationOverrides
newListRotationOverrides
  pRotationId_
  pStartTime_
  pEndTime_ =
    ListRotationOverrides'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        rotationId = pRotationId_,
        startTime = Data._Time Lens.# pStartTime_,
        endTime = Data._Time Lens.# pEndTime_
      }

-- | The maximum number of items to return for this call. The call also
-- returns a token that you can specify in a subsequent call to get the
-- next set of results.
listRotationOverrides_maxResults :: Lens.Lens' ListRotationOverrides (Prelude.Maybe Prelude.Natural)
listRotationOverrides_maxResults = Lens.lens (\ListRotationOverrides' {maxResults} -> maxResults) (\s@ListRotationOverrides' {} a -> s {maxResults = a} :: ListRotationOverrides)

-- | A token to start the list. Use this token to get the next set of
-- results.
listRotationOverrides_nextToken :: Lens.Lens' ListRotationOverrides (Prelude.Maybe Prelude.Text)
listRotationOverrides_nextToken = Lens.lens (\ListRotationOverrides' {nextToken} -> nextToken) (\s@ListRotationOverrides' {} a -> s {nextToken = a} :: ListRotationOverrides)

-- | The Amazon Resource Name (ARN) of the rotation to retrieve information
-- about.
listRotationOverrides_rotationId :: Lens.Lens' ListRotationOverrides Prelude.Text
listRotationOverrides_rotationId = Lens.lens (\ListRotationOverrides' {rotationId} -> rotationId) (\s@ListRotationOverrides' {} a -> s {rotationId = a} :: ListRotationOverrides)

-- | The date and time for the beginning of a time range for listing
-- overrides.
listRotationOverrides_startTime :: Lens.Lens' ListRotationOverrides Prelude.UTCTime
listRotationOverrides_startTime = Lens.lens (\ListRotationOverrides' {startTime} -> startTime) (\s@ListRotationOverrides' {} a -> s {startTime = a} :: ListRotationOverrides) Prelude.. Data._Time

-- | The date and time for the end of a time range for listing overrides.
listRotationOverrides_endTime :: Lens.Lens' ListRotationOverrides Prelude.UTCTime
listRotationOverrides_endTime = Lens.lens (\ListRotationOverrides' {endTime} -> endTime) (\s@ListRotationOverrides' {} a -> s {endTime = a} :: ListRotationOverrides) Prelude.. Data._Time

instance Core.AWSPager ListRotationOverrides where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRotationOverridesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRotationOverridesResponse_rotationOverrides
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listRotationOverrides_nextToken
          Lens..~ rs
          Lens.^? listRotationOverridesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListRotationOverrides where
  type
    AWSResponse ListRotationOverrides =
      ListRotationOverridesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRotationOverridesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "RotationOverrides"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRotationOverrides where
  hashWithSalt _salt ListRotationOverrides' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` rotationId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` endTime

instance Prelude.NFData ListRotationOverrides where
  rnf ListRotationOverrides' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rotationId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf endTime

instance Data.ToHeaders ListRotationOverrides where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SSMContacts.ListRotationOverrides" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRotationOverrides where
  toJSON ListRotationOverrides' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("RotationId" Data..= rotationId),
            Prelude.Just ("StartTime" Data..= startTime),
            Prelude.Just ("EndTime" Data..= endTime)
          ]
      )

instance Data.ToPath ListRotationOverrides where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRotationOverrides where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRotationOverridesResponse' smart constructor.
data ListRotationOverridesResponse = ListRotationOverridesResponse'
  { -- | The token for the next set of items to return. Use this token to get the
    -- next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of rotation overrides in the specified time range.
    rotationOverrides :: Prelude.Maybe [RotationOverride],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRotationOverridesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRotationOverridesResponse_nextToken' - The token for the next set of items to return. Use this token to get the
-- next set of results.
--
-- 'rotationOverrides', 'listRotationOverridesResponse_rotationOverrides' - A list of rotation overrides in the specified time range.
--
-- 'httpStatus', 'listRotationOverridesResponse_httpStatus' - The response's http status code.
newListRotationOverridesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRotationOverridesResponse
newListRotationOverridesResponse pHttpStatus_ =
  ListRotationOverridesResponse'
    { nextToken =
        Prelude.Nothing,
      rotationOverrides = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of items to return. Use this token to get the
-- next set of results.
listRotationOverridesResponse_nextToken :: Lens.Lens' ListRotationOverridesResponse (Prelude.Maybe Prelude.Text)
listRotationOverridesResponse_nextToken = Lens.lens (\ListRotationOverridesResponse' {nextToken} -> nextToken) (\s@ListRotationOverridesResponse' {} a -> s {nextToken = a} :: ListRotationOverridesResponse)

-- | A list of rotation overrides in the specified time range.
listRotationOverridesResponse_rotationOverrides :: Lens.Lens' ListRotationOverridesResponse (Prelude.Maybe [RotationOverride])
listRotationOverridesResponse_rotationOverrides = Lens.lens (\ListRotationOverridesResponse' {rotationOverrides} -> rotationOverrides) (\s@ListRotationOverridesResponse' {} a -> s {rotationOverrides = a} :: ListRotationOverridesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRotationOverridesResponse_httpStatus :: Lens.Lens' ListRotationOverridesResponse Prelude.Int
listRotationOverridesResponse_httpStatus = Lens.lens (\ListRotationOverridesResponse' {httpStatus} -> httpStatus) (\s@ListRotationOverridesResponse' {} a -> s {httpStatus = a} :: ListRotationOverridesResponse)

instance Prelude.NFData ListRotationOverridesResponse where
  rnf ListRotationOverridesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf rotationOverrides
      `Prelude.seq` Prelude.rnf httpStatus
