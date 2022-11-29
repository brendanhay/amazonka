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
-- Module      : Amazonka.WellArchitected.ListShareInvitations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the workload invitations.
module Amazonka.WellArchitected.ListShareInvitations
  ( -- * Creating a Request
    ListShareInvitations (..),
    newListShareInvitations,

    -- * Request Lenses
    listShareInvitations_lensNamePrefix,
    listShareInvitations_nextToken,
    listShareInvitations_workloadNamePrefix,
    listShareInvitations_maxResults,
    listShareInvitations_shareResourceType,

    -- * Destructuring the Response
    ListShareInvitationsResponse (..),
    newListShareInvitationsResponse,

    -- * Response Lenses
    listShareInvitationsResponse_nextToken,
    listShareInvitationsResponse_shareInvitationSummaries,
    listShareInvitationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.WellArchitected.Types

-- | Input for List Share Invitations
--
-- /See:/ 'newListShareInvitations' smart constructor.
data ListShareInvitations = ListShareInvitations'
  { -- | An optional string added to the beginning of each lens name returned in
    -- the results.
    lensNamePrefix :: Prelude.Maybe Prelude.Text,
    nextToken :: Prelude.Maybe Prelude.Text,
    workloadNamePrefix :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The type of share invitations to be returned.
    shareResourceType :: Prelude.Maybe ShareResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListShareInvitations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lensNamePrefix', 'listShareInvitations_lensNamePrefix' - An optional string added to the beginning of each lens name returned in
-- the results.
--
-- 'nextToken', 'listShareInvitations_nextToken' - Undocumented member.
--
-- 'workloadNamePrefix', 'listShareInvitations_workloadNamePrefix' - Undocumented member.
--
-- 'maxResults', 'listShareInvitations_maxResults' - The maximum number of results to return for this request.
--
-- 'shareResourceType', 'listShareInvitations_shareResourceType' - The type of share invitations to be returned.
newListShareInvitations ::
  ListShareInvitations
newListShareInvitations =
  ListShareInvitations'
    { lensNamePrefix =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      workloadNamePrefix = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      shareResourceType = Prelude.Nothing
    }

-- | An optional string added to the beginning of each lens name returned in
-- the results.
listShareInvitations_lensNamePrefix :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Text)
listShareInvitations_lensNamePrefix = Lens.lens (\ListShareInvitations' {lensNamePrefix} -> lensNamePrefix) (\s@ListShareInvitations' {} a -> s {lensNamePrefix = a} :: ListShareInvitations)

-- | Undocumented member.
listShareInvitations_nextToken :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Text)
listShareInvitations_nextToken = Lens.lens (\ListShareInvitations' {nextToken} -> nextToken) (\s@ListShareInvitations' {} a -> s {nextToken = a} :: ListShareInvitations)

-- | Undocumented member.
listShareInvitations_workloadNamePrefix :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Text)
listShareInvitations_workloadNamePrefix = Lens.lens (\ListShareInvitations' {workloadNamePrefix} -> workloadNamePrefix) (\s@ListShareInvitations' {} a -> s {workloadNamePrefix = a} :: ListShareInvitations)

-- | The maximum number of results to return for this request.
listShareInvitations_maxResults :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Natural)
listShareInvitations_maxResults = Lens.lens (\ListShareInvitations' {maxResults} -> maxResults) (\s@ListShareInvitations' {} a -> s {maxResults = a} :: ListShareInvitations)

-- | The type of share invitations to be returned.
listShareInvitations_shareResourceType :: Lens.Lens' ListShareInvitations (Prelude.Maybe ShareResourceType)
listShareInvitations_shareResourceType = Lens.lens (\ListShareInvitations' {shareResourceType} -> shareResourceType) (\s@ListShareInvitations' {} a -> s {shareResourceType = a} :: ListShareInvitations)

instance Core.AWSRequest ListShareInvitations where
  type
    AWSResponse ListShareInvitations =
      ListShareInvitationsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListShareInvitationsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ShareInvitationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListShareInvitations where
  hashWithSalt _salt ListShareInvitations' {..} =
    _salt `Prelude.hashWithSalt` lensNamePrefix
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` workloadNamePrefix
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` shareResourceType

instance Prelude.NFData ListShareInvitations where
  rnf ListShareInvitations' {..} =
    Prelude.rnf lensNamePrefix
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf workloadNamePrefix
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf shareResourceType

instance Core.ToHeaders ListShareInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListShareInvitations where
  toPath = Prelude.const "/shareInvitations"

instance Core.ToQuery ListShareInvitations where
  toQuery ListShareInvitations' {..} =
    Prelude.mconcat
      [ "LensNamePrefix" Core.=: lensNamePrefix,
        "NextToken" Core.=: nextToken,
        "WorkloadNamePrefix" Core.=: workloadNamePrefix,
        "MaxResults" Core.=: maxResults,
        "ShareResourceType" Core.=: shareResourceType
      ]

-- | Input for List Share Invitations
--
-- /See:/ 'newListShareInvitationsResponse' smart constructor.
data ListShareInvitationsResponse = ListShareInvitationsResponse'
  { nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of share invitation summaries in a workload.
    shareInvitationSummaries :: Prelude.Maybe [ShareInvitationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListShareInvitationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listShareInvitationsResponse_nextToken' - Undocumented member.
--
-- 'shareInvitationSummaries', 'listShareInvitationsResponse_shareInvitationSummaries' - List of share invitation summaries in a workload.
--
-- 'httpStatus', 'listShareInvitationsResponse_httpStatus' - The response's http status code.
newListShareInvitationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListShareInvitationsResponse
newListShareInvitationsResponse pHttpStatus_ =
  ListShareInvitationsResponse'
    { nextToken =
        Prelude.Nothing,
      shareInvitationSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
listShareInvitationsResponse_nextToken :: Lens.Lens' ListShareInvitationsResponse (Prelude.Maybe Prelude.Text)
listShareInvitationsResponse_nextToken = Lens.lens (\ListShareInvitationsResponse' {nextToken} -> nextToken) (\s@ListShareInvitationsResponse' {} a -> s {nextToken = a} :: ListShareInvitationsResponse)

-- | List of share invitation summaries in a workload.
listShareInvitationsResponse_shareInvitationSummaries :: Lens.Lens' ListShareInvitationsResponse (Prelude.Maybe [ShareInvitationSummary])
listShareInvitationsResponse_shareInvitationSummaries = Lens.lens (\ListShareInvitationsResponse' {shareInvitationSummaries} -> shareInvitationSummaries) (\s@ListShareInvitationsResponse' {} a -> s {shareInvitationSummaries = a} :: ListShareInvitationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listShareInvitationsResponse_httpStatus :: Lens.Lens' ListShareInvitationsResponse Prelude.Int
listShareInvitationsResponse_httpStatus = Lens.lens (\ListShareInvitationsResponse' {httpStatus} -> httpStatus) (\s@ListShareInvitationsResponse' {} a -> s {httpStatus = a} :: ListShareInvitationsResponse)

instance Prelude.NFData ListShareInvitationsResponse where
  rnf ListShareInvitationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf shareInvitationSummaries
      `Prelude.seq` Prelude.rnf httpStatus
