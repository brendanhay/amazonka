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
    listShareInvitations_maxResults,
    listShareInvitations_nextToken,
    listShareInvitations_shareResourceType,
    listShareInvitations_workloadNamePrefix,

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
import qualified Amazonka.Data as Data
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
    -- | The maximum number of results to return for this request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The type of share invitations to be returned.
    shareResourceType :: Prelude.Maybe ShareResourceType,
    workloadNamePrefix :: Prelude.Maybe Prelude.Text
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
-- 'maxResults', 'listShareInvitations_maxResults' - The maximum number of results to return for this request.
--
-- 'nextToken', 'listShareInvitations_nextToken' - Undocumented member.
--
-- 'shareResourceType', 'listShareInvitations_shareResourceType' - The type of share invitations to be returned.
--
-- 'workloadNamePrefix', 'listShareInvitations_workloadNamePrefix' - Undocumented member.
newListShareInvitations ::
  ListShareInvitations
newListShareInvitations =
  ListShareInvitations'
    { lensNamePrefix =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      shareResourceType = Prelude.Nothing,
      workloadNamePrefix = Prelude.Nothing
    }

-- | An optional string added to the beginning of each lens name returned in
-- the results.
listShareInvitations_lensNamePrefix :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Text)
listShareInvitations_lensNamePrefix = Lens.lens (\ListShareInvitations' {lensNamePrefix} -> lensNamePrefix) (\s@ListShareInvitations' {} a -> s {lensNamePrefix = a} :: ListShareInvitations)

-- | The maximum number of results to return for this request.
listShareInvitations_maxResults :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Natural)
listShareInvitations_maxResults = Lens.lens (\ListShareInvitations' {maxResults} -> maxResults) (\s@ListShareInvitations' {} a -> s {maxResults = a} :: ListShareInvitations)

-- | Undocumented member.
listShareInvitations_nextToken :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Text)
listShareInvitations_nextToken = Lens.lens (\ListShareInvitations' {nextToken} -> nextToken) (\s@ListShareInvitations' {} a -> s {nextToken = a} :: ListShareInvitations)

-- | The type of share invitations to be returned.
listShareInvitations_shareResourceType :: Lens.Lens' ListShareInvitations (Prelude.Maybe ShareResourceType)
listShareInvitations_shareResourceType = Lens.lens (\ListShareInvitations' {shareResourceType} -> shareResourceType) (\s@ListShareInvitations' {} a -> s {shareResourceType = a} :: ListShareInvitations)

-- | Undocumented member.
listShareInvitations_workloadNamePrefix :: Lens.Lens' ListShareInvitations (Prelude.Maybe Prelude.Text)
listShareInvitations_workloadNamePrefix = Lens.lens (\ListShareInvitations' {workloadNamePrefix} -> workloadNamePrefix) (\s@ListShareInvitations' {} a -> s {workloadNamePrefix = a} :: ListShareInvitations)

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
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ShareInvitationSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListShareInvitations where
  hashWithSalt _salt ListShareInvitations' {..} =
    _salt `Prelude.hashWithSalt` lensNamePrefix
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` shareResourceType
      `Prelude.hashWithSalt` workloadNamePrefix

instance Prelude.NFData ListShareInvitations where
  rnf ListShareInvitations' {..} =
    Prelude.rnf lensNamePrefix
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf shareResourceType
      `Prelude.seq` Prelude.rnf workloadNamePrefix

instance Data.ToHeaders ListShareInvitations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListShareInvitations where
  toPath = Prelude.const "/shareInvitations"

instance Data.ToQuery ListShareInvitations where
  toQuery ListShareInvitations' {..} =
    Prelude.mconcat
      [ "LensNamePrefix" Data.=: lensNamePrefix,
        "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken,
        "ShareResourceType" Data.=: shareResourceType,
        "WorkloadNamePrefix" Data.=: workloadNamePrefix
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
