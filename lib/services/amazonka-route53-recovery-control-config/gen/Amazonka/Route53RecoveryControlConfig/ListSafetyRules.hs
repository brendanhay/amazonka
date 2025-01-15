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
-- Module      : Amazonka.Route53RecoveryControlConfig.ListSafetyRules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the safety rules (the assertion rules and gating rules) that
-- you\'ve defined for the routing controls in a control panel.
--
-- This operation returns paginated results.
module Amazonka.Route53RecoveryControlConfig.ListSafetyRules
  ( -- * Creating a Request
    ListSafetyRules (..),
    newListSafetyRules,

    -- * Request Lenses
    listSafetyRules_maxResults,
    listSafetyRules_nextToken,
    listSafetyRules_controlPanelArn,

    -- * Destructuring the Response
    ListSafetyRulesResponse (..),
    newListSafetyRulesResponse,

    -- * Response Lenses
    listSafetyRulesResponse_nextToken,
    listSafetyRulesResponse_safetyRules,
    listSafetyRulesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | /See:/ 'newListSafetyRules' smart constructor.
data ListSafetyRules = ListSafetyRules'
  { -- | The number of objects that you want to return with this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the control panel.
    controlPanelArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSafetyRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSafetyRules_maxResults' - The number of objects that you want to return with this call.
--
-- 'nextToken', 'listSafetyRules_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'controlPanelArn', 'listSafetyRules_controlPanelArn' - The Amazon Resource Name (ARN) of the control panel.
newListSafetyRules ::
  -- | 'controlPanelArn'
  Prelude.Text ->
  ListSafetyRules
newListSafetyRules pControlPanelArn_ =
  ListSafetyRules'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      controlPanelArn = pControlPanelArn_
    }

-- | The number of objects that you want to return with this call.
listSafetyRules_maxResults :: Lens.Lens' ListSafetyRules (Prelude.Maybe Prelude.Natural)
listSafetyRules_maxResults = Lens.lens (\ListSafetyRules' {maxResults} -> maxResults) (\s@ListSafetyRules' {} a -> s {maxResults = a} :: ListSafetyRules)

-- | The token that identifies which batch of results you want to see.
listSafetyRules_nextToken :: Lens.Lens' ListSafetyRules (Prelude.Maybe Prelude.Text)
listSafetyRules_nextToken = Lens.lens (\ListSafetyRules' {nextToken} -> nextToken) (\s@ListSafetyRules' {} a -> s {nextToken = a} :: ListSafetyRules)

-- | The Amazon Resource Name (ARN) of the control panel.
listSafetyRules_controlPanelArn :: Lens.Lens' ListSafetyRules Prelude.Text
listSafetyRules_controlPanelArn = Lens.lens (\ListSafetyRules' {controlPanelArn} -> controlPanelArn) (\s@ListSafetyRules' {} a -> s {controlPanelArn = a} :: ListSafetyRules)

instance Core.AWSPager ListSafetyRules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSafetyRulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSafetyRulesResponse_safetyRules
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listSafetyRules_nextToken
              Lens..~ rs
              Lens.^? listSafetyRulesResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListSafetyRules where
  type
    AWSResponse ListSafetyRules =
      ListSafetyRulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSafetyRulesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "SafetyRules" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSafetyRules where
  hashWithSalt _salt ListSafetyRules' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` controlPanelArn

instance Prelude.NFData ListSafetyRules where
  rnf ListSafetyRules' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf controlPanelArn

instance Data.ToHeaders ListSafetyRules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSafetyRules where
  toPath ListSafetyRules' {..} =
    Prelude.mconcat
      [ "/controlpanel/",
        Data.toBS controlPanelArn,
        "/safetyrules"
      ]

instance Data.ToQuery ListSafetyRules where
  toQuery ListSafetyRules' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListSafetyRulesResponse' smart constructor.
data ListSafetyRulesResponse = ListSafetyRulesResponse'
  { -- | The token that identifies which batch of results you want to see.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The list of safety rules in a control panel.
    safetyRules :: Prelude.Maybe [Rule],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSafetyRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSafetyRulesResponse_nextToken' - The token that identifies which batch of results you want to see.
--
-- 'safetyRules', 'listSafetyRulesResponse_safetyRules' - The list of safety rules in a control panel.
--
-- 'httpStatus', 'listSafetyRulesResponse_httpStatus' - The response's http status code.
newListSafetyRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSafetyRulesResponse
newListSafetyRulesResponse pHttpStatus_ =
  ListSafetyRulesResponse'
    { nextToken =
        Prelude.Nothing,
      safetyRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that identifies which batch of results you want to see.
listSafetyRulesResponse_nextToken :: Lens.Lens' ListSafetyRulesResponse (Prelude.Maybe Prelude.Text)
listSafetyRulesResponse_nextToken = Lens.lens (\ListSafetyRulesResponse' {nextToken} -> nextToken) (\s@ListSafetyRulesResponse' {} a -> s {nextToken = a} :: ListSafetyRulesResponse)

-- | The list of safety rules in a control panel.
listSafetyRulesResponse_safetyRules :: Lens.Lens' ListSafetyRulesResponse (Prelude.Maybe [Rule])
listSafetyRulesResponse_safetyRules = Lens.lens (\ListSafetyRulesResponse' {safetyRules} -> safetyRules) (\s@ListSafetyRulesResponse' {} a -> s {safetyRules = a} :: ListSafetyRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSafetyRulesResponse_httpStatus :: Lens.Lens' ListSafetyRulesResponse Prelude.Int
listSafetyRulesResponse_httpStatus = Lens.lens (\ListSafetyRulesResponse' {httpStatus} -> httpStatus) (\s@ListSafetyRulesResponse' {} a -> s {httpStatus = a} :: ListSafetyRulesResponse)

instance Prelude.NFData ListSafetyRulesResponse where
  rnf ListSafetyRulesResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf safetyRules `Prelude.seq`
        Prelude.rnf httpStatus
