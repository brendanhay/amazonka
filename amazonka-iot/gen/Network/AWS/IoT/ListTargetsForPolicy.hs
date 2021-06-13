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
-- Module      : Network.AWS.IoT.ListTargetsForPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List targets for the specified policy.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListTargetsForPolicy
  ( -- * Creating a Request
    ListTargetsForPolicy (..),
    newListTargetsForPolicy,

    -- * Request Lenses
    listTargetsForPolicy_pageSize,
    listTargetsForPolicy_marker,
    listTargetsForPolicy_policyName,

    -- * Destructuring the Response
    ListTargetsForPolicyResponse (..),
    newListTargetsForPolicyResponse,

    -- * Response Lenses
    listTargetsForPolicyResponse_nextMarker,
    listTargetsForPolicyResponse_targets,
    listTargetsForPolicyResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListTargetsForPolicy' smart constructor.
data ListTargetsForPolicy = ListTargetsForPolicy'
  { -- | The maximum number of results to return at one time.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | A marker used to get the next set of results.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The policy name.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsForPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageSize', 'listTargetsForPolicy_pageSize' - The maximum number of results to return at one time.
--
-- 'marker', 'listTargetsForPolicy_marker' - A marker used to get the next set of results.
--
-- 'policyName', 'listTargetsForPolicy_policyName' - The policy name.
newListTargetsForPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  ListTargetsForPolicy
newListTargetsForPolicy pPolicyName_ =
  ListTargetsForPolicy'
    { pageSize = Prelude.Nothing,
      marker = Prelude.Nothing,
      policyName = pPolicyName_
    }

-- | The maximum number of results to return at one time.
listTargetsForPolicy_pageSize :: Lens.Lens' ListTargetsForPolicy (Prelude.Maybe Prelude.Natural)
listTargetsForPolicy_pageSize = Lens.lens (\ListTargetsForPolicy' {pageSize} -> pageSize) (\s@ListTargetsForPolicy' {} a -> s {pageSize = a} :: ListTargetsForPolicy)

-- | A marker used to get the next set of results.
listTargetsForPolicy_marker :: Lens.Lens' ListTargetsForPolicy (Prelude.Maybe Prelude.Text)
listTargetsForPolicy_marker = Lens.lens (\ListTargetsForPolicy' {marker} -> marker) (\s@ListTargetsForPolicy' {} a -> s {marker = a} :: ListTargetsForPolicy)

-- | The policy name.
listTargetsForPolicy_policyName :: Lens.Lens' ListTargetsForPolicy Prelude.Text
listTargetsForPolicy_policyName = Lens.lens (\ListTargetsForPolicy' {policyName} -> policyName) (\s@ListTargetsForPolicy' {} a -> s {policyName = a} :: ListTargetsForPolicy)

instance Core.AWSPager ListTargetsForPolicy where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTargetsForPolicyResponse_nextMarker
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTargetsForPolicyResponse_targets
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTargetsForPolicy_marker
          Lens..~ rs
          Lens.^? listTargetsForPolicyResponse_nextMarker
            Prelude.. Lens._Just

instance Core.AWSRequest ListTargetsForPolicy where
  type
    AWSResponse ListTargetsForPolicy =
      ListTargetsForPolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTargetsForPolicyResponse'
            Prelude.<$> (x Core..?> "nextMarker")
            Prelude.<*> (x Core..?> "targets" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTargetsForPolicy

instance Prelude.NFData ListTargetsForPolicy

instance Core.ToHeaders ListTargetsForPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON ListTargetsForPolicy where
  toJSON = Prelude.const (Core.Object Prelude.mempty)

instance Core.ToPath ListTargetsForPolicy where
  toPath ListTargetsForPolicy' {..} =
    Prelude.mconcat
      ["/policy-targets/", Core.toBS policyName]

instance Core.ToQuery ListTargetsForPolicy where
  toQuery ListTargetsForPolicy' {..} =
    Prelude.mconcat
      [ "pageSize" Core.=: pageSize,
        "marker" Core.=: marker
      ]

-- | /See:/ 'newListTargetsForPolicyResponse' smart constructor.
data ListTargetsForPolicyResponse = ListTargetsForPolicyResponse'
  { -- | A marker used to get the next set of results.
    nextMarker :: Prelude.Maybe Prelude.Text,
    -- | The policy targets.
    targets :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTargetsForPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextMarker', 'listTargetsForPolicyResponse_nextMarker' - A marker used to get the next set of results.
--
-- 'targets', 'listTargetsForPolicyResponse_targets' - The policy targets.
--
-- 'httpStatus', 'listTargetsForPolicyResponse_httpStatus' - The response's http status code.
newListTargetsForPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTargetsForPolicyResponse
newListTargetsForPolicyResponse pHttpStatus_ =
  ListTargetsForPolicyResponse'
    { nextMarker =
        Prelude.Nothing,
      targets = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A marker used to get the next set of results.
listTargetsForPolicyResponse_nextMarker :: Lens.Lens' ListTargetsForPolicyResponse (Prelude.Maybe Prelude.Text)
listTargetsForPolicyResponse_nextMarker = Lens.lens (\ListTargetsForPolicyResponse' {nextMarker} -> nextMarker) (\s@ListTargetsForPolicyResponse' {} a -> s {nextMarker = a} :: ListTargetsForPolicyResponse)

-- | The policy targets.
listTargetsForPolicyResponse_targets :: Lens.Lens' ListTargetsForPolicyResponse (Prelude.Maybe [Prelude.Text])
listTargetsForPolicyResponse_targets = Lens.lens (\ListTargetsForPolicyResponse' {targets} -> targets) (\s@ListTargetsForPolicyResponse' {} a -> s {targets = a} :: ListTargetsForPolicyResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTargetsForPolicyResponse_httpStatus :: Lens.Lens' ListTargetsForPolicyResponse Prelude.Int
listTargetsForPolicyResponse_httpStatus = Lens.lens (\ListTargetsForPolicyResponse' {httpStatus} -> httpStatus) (\s@ListTargetsForPolicyResponse' {} a -> s {httpStatus = a} :: ListTargetsForPolicyResponse)

instance Prelude.NFData ListTargetsForPolicyResponse
