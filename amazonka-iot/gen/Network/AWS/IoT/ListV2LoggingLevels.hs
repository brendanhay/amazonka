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
-- Module      : Network.AWS.IoT.ListV2LoggingLevels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists logging levels.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListV2LoggingLevels
  ( -- * Creating a Request
    ListV2LoggingLevels (..),
    newListV2LoggingLevels,

    -- * Request Lenses
    listV2LoggingLevels_nextToken,
    listV2LoggingLevels_targetType,
    listV2LoggingLevels_maxResults,

    -- * Destructuring the Response
    ListV2LoggingLevelsResponse (..),
    newListV2LoggingLevelsResponse,

    -- * Response Lenses
    listV2LoggingLevelsResponse_nextToken,
    listV2LoggingLevelsResponse_logTargetConfigurations,
    listV2LoggingLevelsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListV2LoggingLevels' smart constructor.
data ListV2LoggingLevels = ListV2LoggingLevels'
  { -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of resource for which you are configuring logging. Must be
    -- @THING_Group@.
    targetType :: Prelude.Maybe LogTargetType,
    -- | The maximum number of results to return at one time.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListV2LoggingLevels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listV2LoggingLevels_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'targetType', 'listV2LoggingLevels_targetType' - The type of resource for which you are configuring logging. Must be
-- @THING_Group@.
--
-- 'maxResults', 'listV2LoggingLevels_maxResults' - The maximum number of results to return at one time.
newListV2LoggingLevels ::
  ListV2LoggingLevels
newListV2LoggingLevels =
  ListV2LoggingLevels'
    { nextToken = Prelude.Nothing,
      targetType = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listV2LoggingLevels_nextToken :: Lens.Lens' ListV2LoggingLevels (Prelude.Maybe Prelude.Text)
listV2LoggingLevels_nextToken = Lens.lens (\ListV2LoggingLevels' {nextToken} -> nextToken) (\s@ListV2LoggingLevels' {} a -> s {nextToken = a} :: ListV2LoggingLevels)

-- | The type of resource for which you are configuring logging. Must be
-- @THING_Group@.
listV2LoggingLevels_targetType :: Lens.Lens' ListV2LoggingLevels (Prelude.Maybe LogTargetType)
listV2LoggingLevels_targetType = Lens.lens (\ListV2LoggingLevels' {targetType} -> targetType) (\s@ListV2LoggingLevels' {} a -> s {targetType = a} :: ListV2LoggingLevels)

-- | The maximum number of results to return at one time.
listV2LoggingLevels_maxResults :: Lens.Lens' ListV2LoggingLevels (Prelude.Maybe Prelude.Natural)
listV2LoggingLevels_maxResults = Lens.lens (\ListV2LoggingLevels' {maxResults} -> maxResults) (\s@ListV2LoggingLevels' {} a -> s {maxResults = a} :: ListV2LoggingLevels)

instance Core.AWSPager ListV2LoggingLevels where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listV2LoggingLevelsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listV2LoggingLevelsResponse_logTargetConfigurations
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listV2LoggingLevels_nextToken
          Lens..~ rs
          Lens.^? listV2LoggingLevelsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListV2LoggingLevels where
  type
    AWSResponse ListV2LoggingLevels =
      ListV2LoggingLevelsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListV2LoggingLevelsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> ( x Core..?> "logTargetConfigurations"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListV2LoggingLevels

instance Prelude.NFData ListV2LoggingLevels

instance Core.ToHeaders ListV2LoggingLevels where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListV2LoggingLevels where
  toPath = Prelude.const "/v2LoggingLevel"

instance Core.ToQuery ListV2LoggingLevels where
  toQuery ListV2LoggingLevels' {..} =
    Prelude.mconcat
      [ "nextToken" Core.=: nextToken,
        "targetType" Core.=: targetType,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListV2LoggingLevelsResponse' smart constructor.
data ListV2LoggingLevelsResponse = ListV2LoggingLevelsResponse'
  { -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The logging configuration for a target.
    logTargetConfigurations :: Prelude.Maybe [LogTargetConfiguration],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListV2LoggingLevelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listV2LoggingLevelsResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'logTargetConfigurations', 'listV2LoggingLevelsResponse_logTargetConfigurations' - The logging configuration for a target.
--
-- 'httpStatus', 'listV2LoggingLevelsResponse_httpStatus' - The response's http status code.
newListV2LoggingLevelsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListV2LoggingLevelsResponse
newListV2LoggingLevelsResponse pHttpStatus_ =
  ListV2LoggingLevelsResponse'
    { nextToken =
        Prelude.Nothing,
      logTargetConfigurations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listV2LoggingLevelsResponse_nextToken :: Lens.Lens' ListV2LoggingLevelsResponse (Prelude.Maybe Prelude.Text)
listV2LoggingLevelsResponse_nextToken = Lens.lens (\ListV2LoggingLevelsResponse' {nextToken} -> nextToken) (\s@ListV2LoggingLevelsResponse' {} a -> s {nextToken = a} :: ListV2LoggingLevelsResponse)

-- | The logging configuration for a target.
listV2LoggingLevelsResponse_logTargetConfigurations :: Lens.Lens' ListV2LoggingLevelsResponse (Prelude.Maybe [LogTargetConfiguration])
listV2LoggingLevelsResponse_logTargetConfigurations = Lens.lens (\ListV2LoggingLevelsResponse' {logTargetConfigurations} -> logTargetConfigurations) (\s@ListV2LoggingLevelsResponse' {} a -> s {logTargetConfigurations = a} :: ListV2LoggingLevelsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listV2LoggingLevelsResponse_httpStatus :: Lens.Lens' ListV2LoggingLevelsResponse Prelude.Int
listV2LoggingLevelsResponse_httpStatus = Lens.lens (\ListV2LoggingLevelsResponse' {httpStatus} -> httpStatus) (\s@ListV2LoggingLevelsResponse' {} a -> s {httpStatus = a} :: ListV2LoggingLevelsResponse)

instance Prelude.NFData ListV2LoggingLevelsResponse
