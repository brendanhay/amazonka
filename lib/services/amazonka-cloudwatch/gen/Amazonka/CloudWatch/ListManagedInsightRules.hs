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
-- Module      : Amazonka.CloudWatch.ListManagedInsightRules
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list that contains the number of managed Contributor Insights
-- rules in your account.
module Amazonka.CloudWatch.ListManagedInsightRules
  ( -- * Creating a Request
    ListManagedInsightRules (..),
    newListManagedInsightRules,

    -- * Request Lenses
    listManagedInsightRules_nextToken,
    listManagedInsightRules_maxResults,
    listManagedInsightRules_resourceARN,

    -- * Destructuring the Response
    ListManagedInsightRulesResponse (..),
    newListManagedInsightRulesResponse,

    -- * Response Lenses
    listManagedInsightRulesResponse_nextToken,
    listManagedInsightRulesResponse_managedRules,
    listManagedInsightRulesResponse_httpStatus,
  )
where

import Amazonka.CloudWatch.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListManagedInsightRules' smart constructor.
data ListManagedInsightRules = ListManagedInsightRules'
  { -- | Include this value to get the next set of rules if the value was
    -- returned by the previous operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in one operation. If you omit
    -- this parameter, the default number is used. The default number is @100@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of an Amazon Web Services resource that has managed Contributor
    -- Insights rules.
    resourceARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedInsightRules' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedInsightRules_nextToken' - Include this value to get the next set of rules if the value was
-- returned by the previous operation.
--
-- 'maxResults', 'listManagedInsightRules_maxResults' - The maximum number of results to return in one operation. If you omit
-- this parameter, the default number is used. The default number is @100@.
--
-- 'resourceARN', 'listManagedInsightRules_resourceARN' - The ARN of an Amazon Web Services resource that has managed Contributor
-- Insights rules.
newListManagedInsightRules ::
  -- | 'resourceARN'
  Prelude.Text ->
  ListManagedInsightRules
newListManagedInsightRules pResourceARN_ =
  ListManagedInsightRules'
    { nextToken =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      resourceARN = pResourceARN_
    }

-- | Include this value to get the next set of rules if the value was
-- returned by the previous operation.
listManagedInsightRules_nextToken :: Lens.Lens' ListManagedInsightRules (Prelude.Maybe Prelude.Text)
listManagedInsightRules_nextToken = Lens.lens (\ListManagedInsightRules' {nextToken} -> nextToken) (\s@ListManagedInsightRules' {} a -> s {nextToken = a} :: ListManagedInsightRules)

-- | The maximum number of results to return in one operation. If you omit
-- this parameter, the default number is used. The default number is @100@.
listManagedInsightRules_maxResults :: Lens.Lens' ListManagedInsightRules (Prelude.Maybe Prelude.Natural)
listManagedInsightRules_maxResults = Lens.lens (\ListManagedInsightRules' {maxResults} -> maxResults) (\s@ListManagedInsightRules' {} a -> s {maxResults = a} :: ListManagedInsightRules)

-- | The ARN of an Amazon Web Services resource that has managed Contributor
-- Insights rules.
listManagedInsightRules_resourceARN :: Lens.Lens' ListManagedInsightRules Prelude.Text
listManagedInsightRules_resourceARN = Lens.lens (\ListManagedInsightRules' {resourceARN} -> resourceARN) (\s@ListManagedInsightRules' {} a -> s {resourceARN = a} :: ListManagedInsightRules)

instance Core.AWSRequest ListManagedInsightRules where
  type
    AWSResponse ListManagedInsightRules =
      ListManagedInsightRulesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ListManagedInsightRulesResult"
      ( \s h x ->
          ListManagedInsightRulesResponse'
            Prelude.<$> (x Data..@? "NextToken")
            Prelude.<*> ( x Data..@? "ManagedRules" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListManagedInsightRules where
  hashWithSalt _salt ListManagedInsightRules' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` resourceARN

instance Prelude.NFData ListManagedInsightRules where
  rnf ListManagedInsightRules' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf resourceARN

instance Data.ToHeaders ListManagedInsightRules where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ListManagedInsightRules where
  toPath = Prelude.const "/"

instance Data.ToQuery ListManagedInsightRules where
  toQuery ListManagedInsightRules' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ListManagedInsightRules" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-08-01" :: Prelude.ByteString),
        "NextToken" Data.=: nextToken,
        "MaxResults" Data.=: maxResults,
        "ResourceARN" Data.=: resourceARN
      ]

-- | /See:/ 'newListManagedInsightRulesResponse' smart constructor.
data ListManagedInsightRulesResponse = ListManagedInsightRulesResponse'
  { -- | Include this value to get the next set of rules if the value was
    -- returned by the previous operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The managed rules that are available for the specified Amazon Web
    -- Services resource.
    managedRules :: Prelude.Maybe [ManagedRuleDescription],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListManagedInsightRulesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listManagedInsightRulesResponse_nextToken' - Include this value to get the next set of rules if the value was
-- returned by the previous operation.
--
-- 'managedRules', 'listManagedInsightRulesResponse_managedRules' - The managed rules that are available for the specified Amazon Web
-- Services resource.
--
-- 'httpStatus', 'listManagedInsightRulesResponse_httpStatus' - The response's http status code.
newListManagedInsightRulesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListManagedInsightRulesResponse
newListManagedInsightRulesResponse pHttpStatus_ =
  ListManagedInsightRulesResponse'
    { nextToken =
        Prelude.Nothing,
      managedRules = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Include this value to get the next set of rules if the value was
-- returned by the previous operation.
listManagedInsightRulesResponse_nextToken :: Lens.Lens' ListManagedInsightRulesResponse (Prelude.Maybe Prelude.Text)
listManagedInsightRulesResponse_nextToken = Lens.lens (\ListManagedInsightRulesResponse' {nextToken} -> nextToken) (\s@ListManagedInsightRulesResponse' {} a -> s {nextToken = a} :: ListManagedInsightRulesResponse)

-- | The managed rules that are available for the specified Amazon Web
-- Services resource.
listManagedInsightRulesResponse_managedRules :: Lens.Lens' ListManagedInsightRulesResponse (Prelude.Maybe [ManagedRuleDescription])
listManagedInsightRulesResponse_managedRules = Lens.lens (\ListManagedInsightRulesResponse' {managedRules} -> managedRules) (\s@ListManagedInsightRulesResponse' {} a -> s {managedRules = a} :: ListManagedInsightRulesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listManagedInsightRulesResponse_httpStatus :: Lens.Lens' ListManagedInsightRulesResponse Prelude.Int
listManagedInsightRulesResponse_httpStatus = Lens.lens (\ListManagedInsightRulesResponse' {httpStatus} -> httpStatus) (\s@ListManagedInsightRulesResponse' {} a -> s {httpStatus = a} :: ListManagedInsightRulesResponse)

instance
  Prelude.NFData
    ListManagedInsightRulesResponse
  where
  rnf ListManagedInsightRulesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf managedRules
      `Prelude.seq` Prelude.rnf httpStatus
