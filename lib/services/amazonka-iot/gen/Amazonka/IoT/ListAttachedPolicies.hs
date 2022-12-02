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
-- Module      : Amazonka.IoT.ListAttachedPolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the policies attached to the specified thing group.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions ListAttachedPolicies>
-- action.
--
-- This operation returns paginated results.
module Amazonka.IoT.ListAttachedPolicies
  ( -- * Creating a Request
    ListAttachedPolicies (..),
    newListAttachedPolicies,

    -- * Request Lenses
    listAttachedPolicies_marker,
    listAttachedPolicies_recursive,
    listAttachedPolicies_pageSize,
    listAttachedPolicies_target,

    -- * Destructuring the Response
    ListAttachedPoliciesResponse (..),
    newListAttachedPoliciesResponse,

    -- * Response Lenses
    listAttachedPoliciesResponse_policies,
    listAttachedPoliciesResponse_nextMarker,
    listAttachedPoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListAttachedPolicies' smart constructor.
data ListAttachedPolicies = ListAttachedPolicies'
  { -- | The token to retrieve the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | When true, recursively list attached policies.
    recursive :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of results to be returned per request.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The group or principal for which the policies will be listed. Valid
    -- principals are CertificateArn
    -- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
    -- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
    -- (/region/:/id/).
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedPolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'listAttachedPolicies_marker' - The token to retrieve the next set of results.
--
-- 'recursive', 'listAttachedPolicies_recursive' - When true, recursively list attached policies.
--
-- 'pageSize', 'listAttachedPolicies_pageSize' - The maximum number of results to be returned per request.
--
-- 'target', 'listAttachedPolicies_target' - The group or principal for which the policies will be listed. Valid
-- principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
newListAttachedPolicies ::
  -- | 'target'
  Prelude.Text ->
  ListAttachedPolicies
newListAttachedPolicies pTarget_ =
  ListAttachedPolicies'
    { marker = Prelude.Nothing,
      recursive = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      target = pTarget_
    }

-- | The token to retrieve the next set of results.
listAttachedPolicies_marker :: Lens.Lens' ListAttachedPolicies (Prelude.Maybe Prelude.Text)
listAttachedPolicies_marker = Lens.lens (\ListAttachedPolicies' {marker} -> marker) (\s@ListAttachedPolicies' {} a -> s {marker = a} :: ListAttachedPolicies)

-- | When true, recursively list attached policies.
listAttachedPolicies_recursive :: Lens.Lens' ListAttachedPolicies (Prelude.Maybe Prelude.Bool)
listAttachedPolicies_recursive = Lens.lens (\ListAttachedPolicies' {recursive} -> recursive) (\s@ListAttachedPolicies' {} a -> s {recursive = a} :: ListAttachedPolicies)

-- | The maximum number of results to be returned per request.
listAttachedPolicies_pageSize :: Lens.Lens' ListAttachedPolicies (Prelude.Maybe Prelude.Natural)
listAttachedPolicies_pageSize = Lens.lens (\ListAttachedPolicies' {pageSize} -> pageSize) (\s@ListAttachedPolicies' {} a -> s {pageSize = a} :: ListAttachedPolicies)

-- | The group or principal for which the policies will be listed. Valid
-- principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
listAttachedPolicies_target :: Lens.Lens' ListAttachedPolicies Prelude.Text
listAttachedPolicies_target = Lens.lens (\ListAttachedPolicies' {target} -> target) (\s@ListAttachedPolicies' {} a -> s {target = a} :: ListAttachedPolicies)

instance Core.AWSPager ListAttachedPolicies where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAttachedPoliciesResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAttachedPoliciesResponse_policies
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAttachedPolicies_marker
          Lens..~ rs
          Lens.^? listAttachedPoliciesResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListAttachedPolicies where
  type
    AWSResponse ListAttachedPolicies =
      ListAttachedPoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAttachedPoliciesResponse'
            Prelude.<$> (x Data..?> "policies" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextMarker")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAttachedPolicies where
  hashWithSalt _salt ListAttachedPolicies' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` recursive
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` target

instance Prelude.NFData ListAttachedPolicies where
  rnf ListAttachedPolicies' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf recursive
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf target

instance Data.ToHeaders ListAttachedPolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON ListAttachedPolicies where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath ListAttachedPolicies where
  toPath ListAttachedPolicies' {..} =
    Prelude.mconcat
      ["/attached-policies/", Data.toBS target]

instance Data.ToQuery ListAttachedPolicies where
  toQuery ListAttachedPolicies' {..} =
    Prelude.mconcat
      [ "marker" Data.=: marker,
        "recursive" Data.=: recursive,
        "pageSize" Data.=: pageSize
      ]

-- | /See:/ 'newListAttachedPoliciesResponse' smart constructor.
data ListAttachedPoliciesResponse = ListAttachedPoliciesResponse'
  { -- | The policies.
    policies :: Prelude.Maybe [Policy],
    -- | The token to retrieve the next set of results, or \`\`null\`\` if there
    -- are no more results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAttachedPoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policies', 'listAttachedPoliciesResponse_policies' - The policies.
--
-- 'nextMarker', 'listAttachedPoliciesResponse_nextMarker' - The token to retrieve the next set of results, or \`\`null\`\` if there
-- are no more results.
--
-- 'httpStatus', 'listAttachedPoliciesResponse_httpStatus' - The response's http status code.
newListAttachedPoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAttachedPoliciesResponse
newListAttachedPoliciesResponse pHttpStatus_ =
  ListAttachedPoliciesResponse'
    { policies =
        Prelude.Nothing,
      nextMarker = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The policies.
listAttachedPoliciesResponse_policies :: Lens.Lens' ListAttachedPoliciesResponse (Prelude.Maybe [Policy])
listAttachedPoliciesResponse_policies = Lens.lens (\ListAttachedPoliciesResponse' {policies} -> policies) (\s@ListAttachedPoliciesResponse' {} a -> s {policies = a} :: ListAttachedPoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results, or \`\`null\`\` if there
-- are no more results.
listAttachedPoliciesResponse_nextMarker :: Lens.Lens' ListAttachedPoliciesResponse (Prelude.Maybe Prelude.Text)
listAttachedPoliciesResponse_nextMarker = Lens.lens (\ListAttachedPoliciesResponse' {nextMarker} -> nextMarker) (\s@ListAttachedPoliciesResponse' {} a -> s {nextMarker = a} :: ListAttachedPoliciesResponse)

-- | The response's http status code.
listAttachedPoliciesResponse_httpStatus :: Lens.Lens' ListAttachedPoliciesResponse Prelude.Int
listAttachedPoliciesResponse_httpStatus = Lens.lens (\ListAttachedPoliciesResponse' {httpStatus} -> httpStatus) (\s@ListAttachedPoliciesResponse' {} a -> s {httpStatus = a} :: ListAttachedPoliciesResponse)

instance Prelude.NFData ListAttachedPoliciesResponse where
  rnf ListAttachedPoliciesResponse' {..} =
    Prelude.rnf policies
      `Prelude.seq` Prelude.rnf nextMarker
      `Prelude.seq` Prelude.rnf httpStatus
