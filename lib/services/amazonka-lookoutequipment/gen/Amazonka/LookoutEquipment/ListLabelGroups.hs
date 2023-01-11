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
-- Module      : Amazonka.LookoutEquipment.ListLabelGroups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the label groups.
module Amazonka.LookoutEquipment.ListLabelGroups
  ( -- * Creating a Request
    ListLabelGroups (..),
    newListLabelGroups,

    -- * Request Lenses
    listLabelGroups_labelGroupNameBeginsWith,
    listLabelGroups_maxResults,
    listLabelGroups_nextToken,

    -- * Destructuring the Response
    ListLabelGroupsResponse (..),
    newListLabelGroupsResponse,

    -- * Response Lenses
    listLabelGroupsResponse_labelGroupSummaries,
    listLabelGroupsResponse_nextToken,
    listLabelGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LookoutEquipment.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListLabelGroups' smart constructor.
data ListLabelGroups = ListLabelGroups'
  { -- | The beginning of the name of the label groups to be listed.
    labelGroupNameBeginsWith :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of label groups to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | An opaque pagination token indicating where to continue the listing of
    -- label groups.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupNameBeginsWith', 'listLabelGroups_labelGroupNameBeginsWith' - The beginning of the name of the label groups to be listed.
--
-- 'maxResults', 'listLabelGroups_maxResults' - Specifies the maximum number of label groups to list.
--
-- 'nextToken', 'listLabelGroups_nextToken' - An opaque pagination token indicating where to continue the listing of
-- label groups.
newListLabelGroups ::
  ListLabelGroups
newListLabelGroups =
  ListLabelGroups'
    { labelGroupNameBeginsWith =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The beginning of the name of the label groups to be listed.
listLabelGroups_labelGroupNameBeginsWith :: Lens.Lens' ListLabelGroups (Prelude.Maybe Prelude.Text)
listLabelGroups_labelGroupNameBeginsWith = Lens.lens (\ListLabelGroups' {labelGroupNameBeginsWith} -> labelGroupNameBeginsWith) (\s@ListLabelGroups' {} a -> s {labelGroupNameBeginsWith = a} :: ListLabelGroups)

-- | Specifies the maximum number of label groups to list.
listLabelGroups_maxResults :: Lens.Lens' ListLabelGroups (Prelude.Maybe Prelude.Natural)
listLabelGroups_maxResults = Lens.lens (\ListLabelGroups' {maxResults} -> maxResults) (\s@ListLabelGroups' {} a -> s {maxResults = a} :: ListLabelGroups)

-- | An opaque pagination token indicating where to continue the listing of
-- label groups.
listLabelGroups_nextToken :: Lens.Lens' ListLabelGroups (Prelude.Maybe Prelude.Text)
listLabelGroups_nextToken = Lens.lens (\ListLabelGroups' {nextToken} -> nextToken) (\s@ListLabelGroups' {} a -> s {nextToken = a} :: ListLabelGroups)

instance Core.AWSRequest ListLabelGroups where
  type
    AWSResponse ListLabelGroups =
      ListLabelGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLabelGroupsResponse'
            Prelude.<$> ( x Data..?> "LabelGroupSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLabelGroups where
  hashWithSalt _salt ListLabelGroups' {..} =
    _salt
      `Prelude.hashWithSalt` labelGroupNameBeginsWith
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListLabelGroups where
  rnf ListLabelGroups' {..} =
    Prelude.rnf labelGroupNameBeginsWith
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListLabelGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSLookoutEquipmentFrontendService.ListLabelGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLabelGroups where
  toJSON ListLabelGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("LabelGroupNameBeginsWith" Data..=)
              Prelude.<$> labelGroupNameBeginsWith,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListLabelGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLabelGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLabelGroupsResponse' smart constructor.
data ListLabelGroupsResponse = ListLabelGroupsResponse'
  { -- | A summary of the label groups.
    labelGroupSummaries :: Prelude.Maybe [LabelGroupSummary],
    -- | An opaque pagination token indicating where to continue the listing of
    -- label groups.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLabelGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labelGroupSummaries', 'listLabelGroupsResponse_labelGroupSummaries' - A summary of the label groups.
--
-- 'nextToken', 'listLabelGroupsResponse_nextToken' - An opaque pagination token indicating where to continue the listing of
-- label groups.
--
-- 'httpStatus', 'listLabelGroupsResponse_httpStatus' - The response's http status code.
newListLabelGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLabelGroupsResponse
newListLabelGroupsResponse pHttpStatus_ =
  ListLabelGroupsResponse'
    { labelGroupSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A summary of the label groups.
listLabelGroupsResponse_labelGroupSummaries :: Lens.Lens' ListLabelGroupsResponse (Prelude.Maybe [LabelGroupSummary])
listLabelGroupsResponse_labelGroupSummaries = Lens.lens (\ListLabelGroupsResponse' {labelGroupSummaries} -> labelGroupSummaries) (\s@ListLabelGroupsResponse' {} a -> s {labelGroupSummaries = a} :: ListLabelGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An opaque pagination token indicating where to continue the listing of
-- label groups.
listLabelGroupsResponse_nextToken :: Lens.Lens' ListLabelGroupsResponse (Prelude.Maybe Prelude.Text)
listLabelGroupsResponse_nextToken = Lens.lens (\ListLabelGroupsResponse' {nextToken} -> nextToken) (\s@ListLabelGroupsResponse' {} a -> s {nextToken = a} :: ListLabelGroupsResponse)

-- | The response's http status code.
listLabelGroupsResponse_httpStatus :: Lens.Lens' ListLabelGroupsResponse Prelude.Int
listLabelGroupsResponse_httpStatus = Lens.lens (\ListLabelGroupsResponse' {httpStatus} -> httpStatus) (\s@ListLabelGroupsResponse' {} a -> s {httpStatus = a} :: ListLabelGroupsResponse)

instance Prelude.NFData ListLabelGroupsResponse where
  rnf ListLabelGroupsResponse' {..} =
    Prelude.rnf labelGroupSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
