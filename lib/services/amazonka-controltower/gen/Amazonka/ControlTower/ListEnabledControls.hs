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
-- Module      : Amazonka.ControlTower.ListEnabledControls
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the controls enabled by AWS Control Tower on the specified
-- organizational unit and the accounts it contains.
--
-- This operation returns paginated results.
module Amazonka.ControlTower.ListEnabledControls
  ( -- * Creating a Request
    ListEnabledControls (..),
    newListEnabledControls,

    -- * Request Lenses
    listEnabledControls_nextToken,
    listEnabledControls_maxResults,
    listEnabledControls_targetIdentifier,

    -- * Destructuring the Response
    ListEnabledControlsResponse (..),
    newListEnabledControlsResponse,

    -- * Response Lenses
    listEnabledControlsResponse_nextToken,
    listEnabledControlsResponse_httpStatus,
    listEnabledControlsResponse_enabledControls,
  )
where

import Amazonka.ControlTower.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListEnabledControls' smart constructor.
data ListEnabledControls = ListEnabledControls'
  { -- | The token to continue the list from a previous API call with the same
    -- parameters.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | How many results to return per API call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the organizational unit.
    targetIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnabledControls' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnabledControls_nextToken' - The token to continue the list from a previous API call with the same
-- parameters.
--
-- 'maxResults', 'listEnabledControls_maxResults' - How many results to return per API call.
--
-- 'targetIdentifier', 'listEnabledControls_targetIdentifier' - The ARN of the organizational unit.
newListEnabledControls ::
  -- | 'targetIdentifier'
  Prelude.Text ->
  ListEnabledControls
newListEnabledControls pTargetIdentifier_ =
  ListEnabledControls'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      targetIdentifier = pTargetIdentifier_
    }

-- | The token to continue the list from a previous API call with the same
-- parameters.
listEnabledControls_nextToken :: Lens.Lens' ListEnabledControls (Prelude.Maybe Prelude.Text)
listEnabledControls_nextToken = Lens.lens (\ListEnabledControls' {nextToken} -> nextToken) (\s@ListEnabledControls' {} a -> s {nextToken = a} :: ListEnabledControls)

-- | How many results to return per API call.
listEnabledControls_maxResults :: Lens.Lens' ListEnabledControls (Prelude.Maybe Prelude.Natural)
listEnabledControls_maxResults = Lens.lens (\ListEnabledControls' {maxResults} -> maxResults) (\s@ListEnabledControls' {} a -> s {maxResults = a} :: ListEnabledControls)

-- | The ARN of the organizational unit.
listEnabledControls_targetIdentifier :: Lens.Lens' ListEnabledControls Prelude.Text
listEnabledControls_targetIdentifier = Lens.lens (\ListEnabledControls' {targetIdentifier} -> targetIdentifier) (\s@ListEnabledControls' {} a -> s {targetIdentifier = a} :: ListEnabledControls)

instance Core.AWSPager ListEnabledControls where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEnabledControlsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listEnabledControlsResponse_enabledControls
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEnabledControls_nextToken
          Lens..~ rs
          Lens.^? listEnabledControlsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListEnabledControls where
  type
    AWSResponse ListEnabledControls =
      ListEnabledControlsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEnabledControlsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "enabledControls"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEnabledControls where
  hashWithSalt _salt ListEnabledControls' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` targetIdentifier

instance Prelude.NFData ListEnabledControls where
  rnf ListEnabledControls' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf targetIdentifier

instance Core.ToHeaders ListEnabledControls where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEnabledControls where
  toJSON ListEnabledControls' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("maxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just
              ("targetIdentifier" Core..= targetIdentifier)
          ]
      )

instance Core.ToPath ListEnabledControls where
  toPath = Prelude.const "/list-enabled-controls"

instance Core.ToQuery ListEnabledControls where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEnabledControlsResponse' smart constructor.
data ListEnabledControlsResponse = ListEnabledControlsResponse'
  { -- | Retrieves the next page of results. If the string is empty, the current
    -- response is the end of the results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Lists the controls enabled by AWS Control Tower on the specified
    -- organizational unit and the accounts it contains.
    enabledControls :: [EnabledControlSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEnabledControlsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEnabledControlsResponse_nextToken' - Retrieves the next page of results. If the string is empty, the current
-- response is the end of the results.
--
-- 'httpStatus', 'listEnabledControlsResponse_httpStatus' - The response's http status code.
--
-- 'enabledControls', 'listEnabledControlsResponse_enabledControls' - Lists the controls enabled by AWS Control Tower on the specified
-- organizational unit and the accounts it contains.
newListEnabledControlsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEnabledControlsResponse
newListEnabledControlsResponse pHttpStatus_ =
  ListEnabledControlsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      enabledControls = Prelude.mempty
    }

-- | Retrieves the next page of results. If the string is empty, the current
-- response is the end of the results.
listEnabledControlsResponse_nextToken :: Lens.Lens' ListEnabledControlsResponse (Prelude.Maybe Prelude.Text)
listEnabledControlsResponse_nextToken = Lens.lens (\ListEnabledControlsResponse' {nextToken} -> nextToken) (\s@ListEnabledControlsResponse' {} a -> s {nextToken = a} :: ListEnabledControlsResponse)

-- | The response's http status code.
listEnabledControlsResponse_httpStatus :: Lens.Lens' ListEnabledControlsResponse Prelude.Int
listEnabledControlsResponse_httpStatus = Lens.lens (\ListEnabledControlsResponse' {httpStatus} -> httpStatus) (\s@ListEnabledControlsResponse' {} a -> s {httpStatus = a} :: ListEnabledControlsResponse)

-- | Lists the controls enabled by AWS Control Tower on the specified
-- organizational unit and the accounts it contains.
listEnabledControlsResponse_enabledControls :: Lens.Lens' ListEnabledControlsResponse [EnabledControlSummary]
listEnabledControlsResponse_enabledControls = Lens.lens (\ListEnabledControlsResponse' {enabledControls} -> enabledControls) (\s@ListEnabledControlsResponse' {} a -> s {enabledControls = a} :: ListEnabledControlsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEnabledControlsResponse where
  rnf ListEnabledControlsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf enabledControls
