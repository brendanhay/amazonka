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
-- Module      : Amazonka.Connect.ListContactFlowModules
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the flow modules for the specified Amazon
-- Connect instance.
--
-- This operation returns paginated results.
module Amazonka.Connect.ListContactFlowModules
  ( -- * Creating a Request
    ListContactFlowModules (..),
    newListContactFlowModules,

    -- * Request Lenses
    listContactFlowModules_contactFlowModuleState,
    listContactFlowModules_maxResults,
    listContactFlowModules_nextToken,
    listContactFlowModules_instanceId,

    -- * Destructuring the Response
    ListContactFlowModulesResponse (..),
    newListContactFlowModulesResponse,

    -- * Response Lenses
    listContactFlowModulesResponse_contactFlowModulesSummaryList,
    listContactFlowModulesResponse_nextToken,
    listContactFlowModulesResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListContactFlowModules' smart constructor.
data ListContactFlowModules = ListContactFlowModules'
  { -- | The state of the flow module.
    contactFlowModuleState :: Prelude.Maybe ContactFlowModuleState,
    -- | The maximum number of results to return per page.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactFlowModules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFlowModuleState', 'listContactFlowModules_contactFlowModuleState' - The state of the flow module.
--
-- 'maxResults', 'listContactFlowModules_maxResults' - The maximum number of results to return per page.
--
-- 'nextToken', 'listContactFlowModules_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'instanceId', 'listContactFlowModules_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newListContactFlowModules ::
  -- | 'instanceId'
  Prelude.Text ->
  ListContactFlowModules
newListContactFlowModules pInstanceId_ =
  ListContactFlowModules'
    { contactFlowModuleState =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      instanceId = pInstanceId_
    }

-- | The state of the flow module.
listContactFlowModules_contactFlowModuleState :: Lens.Lens' ListContactFlowModules (Prelude.Maybe ContactFlowModuleState)
listContactFlowModules_contactFlowModuleState = Lens.lens (\ListContactFlowModules' {contactFlowModuleState} -> contactFlowModuleState) (\s@ListContactFlowModules' {} a -> s {contactFlowModuleState = a} :: ListContactFlowModules)

-- | The maximum number of results to return per page.
listContactFlowModules_maxResults :: Lens.Lens' ListContactFlowModules (Prelude.Maybe Prelude.Natural)
listContactFlowModules_maxResults = Lens.lens (\ListContactFlowModules' {maxResults} -> maxResults) (\s@ListContactFlowModules' {} a -> s {maxResults = a} :: ListContactFlowModules)

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listContactFlowModules_nextToken :: Lens.Lens' ListContactFlowModules (Prelude.Maybe Prelude.Text)
listContactFlowModules_nextToken = Lens.lens (\ListContactFlowModules' {nextToken} -> nextToken) (\s@ListContactFlowModules' {} a -> s {nextToken = a} :: ListContactFlowModules)

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
listContactFlowModules_instanceId :: Lens.Lens' ListContactFlowModules Prelude.Text
listContactFlowModules_instanceId = Lens.lens (\ListContactFlowModules' {instanceId} -> instanceId) (\s@ListContactFlowModules' {} a -> s {instanceId = a} :: ListContactFlowModules)

instance Core.AWSPager ListContactFlowModules where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContactFlowModulesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContactFlowModulesResponse_contactFlowModulesSummaryList
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listContactFlowModules_nextToken
          Lens..~ rs
          Lens.^? listContactFlowModulesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListContactFlowModules where
  type
    AWSResponse ListContactFlowModules =
      ListContactFlowModulesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContactFlowModulesResponse'
            Prelude.<$> ( x
                            Data..?> "ContactFlowModulesSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContactFlowModules where
  hashWithSalt _salt ListContactFlowModules' {..} =
    _salt
      `Prelude.hashWithSalt` contactFlowModuleState
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` instanceId

instance Prelude.NFData ListContactFlowModules where
  rnf ListContactFlowModules' {..} =
    Prelude.rnf contactFlowModuleState
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf instanceId

instance Data.ToHeaders ListContactFlowModules where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListContactFlowModules where
  toPath ListContactFlowModules' {..} =
    Prelude.mconcat
      [ "/contact-flow-modules-summary/",
        Data.toBS instanceId
      ]

instance Data.ToQuery ListContactFlowModules where
  toQuery ListContactFlowModules' {..} =
    Prelude.mconcat
      [ "state" Data.=: contactFlowModuleState,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListContactFlowModulesResponse' smart constructor.
data ListContactFlowModulesResponse = ListContactFlowModulesResponse'
  { -- | Information about the flow module.
    contactFlowModulesSummaryList :: Prelude.Maybe [ContactFlowModuleSummary],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactFlowModulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFlowModulesSummaryList', 'listContactFlowModulesResponse_contactFlowModulesSummaryList' - Information about the flow module.
--
-- 'nextToken', 'listContactFlowModulesResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listContactFlowModulesResponse_httpStatus' - The response's http status code.
newListContactFlowModulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContactFlowModulesResponse
newListContactFlowModulesResponse pHttpStatus_ =
  ListContactFlowModulesResponse'
    { contactFlowModulesSummaryList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the flow module.
listContactFlowModulesResponse_contactFlowModulesSummaryList :: Lens.Lens' ListContactFlowModulesResponse (Prelude.Maybe [ContactFlowModuleSummary])
listContactFlowModulesResponse_contactFlowModulesSummaryList = Lens.lens (\ListContactFlowModulesResponse' {contactFlowModulesSummaryList} -> contactFlowModulesSummaryList) (\s@ListContactFlowModulesResponse' {} a -> s {contactFlowModulesSummaryList = a} :: ListContactFlowModulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are additional results, this is the token for the next set of
-- results.
listContactFlowModulesResponse_nextToken :: Lens.Lens' ListContactFlowModulesResponse (Prelude.Maybe Prelude.Text)
listContactFlowModulesResponse_nextToken = Lens.lens (\ListContactFlowModulesResponse' {nextToken} -> nextToken) (\s@ListContactFlowModulesResponse' {} a -> s {nextToken = a} :: ListContactFlowModulesResponse)

-- | The response's http status code.
listContactFlowModulesResponse_httpStatus :: Lens.Lens' ListContactFlowModulesResponse Prelude.Int
listContactFlowModulesResponse_httpStatus = Lens.lens (\ListContactFlowModulesResponse' {httpStatus} -> httpStatus) (\s@ListContactFlowModulesResponse' {} a -> s {httpStatus = a} :: ListContactFlowModulesResponse)

instance
  Prelude.NFData
    ListContactFlowModulesResponse
  where
  rnf ListContactFlowModulesResponse' {..} =
    Prelude.rnf contactFlowModulesSummaryList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
