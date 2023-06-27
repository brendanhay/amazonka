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
-- Module      : Amazonka.Evidently.ListSegmentReferences
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to find which experiments or launches are using a
-- specified segment.
--
-- This operation returns paginated results.
module Amazonka.Evidently.ListSegmentReferences
  ( -- * Creating a Request
    ListSegmentReferences (..),
    newListSegmentReferences,

    -- * Request Lenses
    listSegmentReferences_maxResults,
    listSegmentReferences_nextToken,
    listSegmentReferences_segment,
    listSegmentReferences_type,

    -- * Destructuring the Response
    ListSegmentReferencesResponse (..),
    newListSegmentReferencesResponse,

    -- * Response Lenses
    listSegmentReferencesResponse_nextToken,
    listSegmentReferencesResponse_referencedBy,
    listSegmentReferencesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Evidently.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSegmentReferences' smart constructor.
data ListSegmentReferences = ListSegmentReferences'
  { -- | The maximum number of results to include in the response. If you omit
    -- this, the default of 50 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to use when requesting the next set of results. You received
    -- this token from a previous @ListSegmentReferences@ operation.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the segment that you want to view information for.
    segment :: Prelude.Text,
    -- | Specifies whether to return information about launches or experiments
    -- that use this segment.
    type' :: SegmentReferenceResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSegmentReferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listSegmentReferences_maxResults' - The maximum number of results to include in the response. If you omit
-- this, the default of 50 is used.
--
-- 'nextToken', 'listSegmentReferences_nextToken' - The token to use when requesting the next set of results. You received
-- this token from a previous @ListSegmentReferences@ operation.
--
-- 'segment', 'listSegmentReferences_segment' - The ARN of the segment that you want to view information for.
--
-- 'type'', 'listSegmentReferences_type' - Specifies whether to return information about launches or experiments
-- that use this segment.
newListSegmentReferences ::
  -- | 'segment'
  Prelude.Text ->
  -- | 'type''
  SegmentReferenceResourceType ->
  ListSegmentReferences
newListSegmentReferences pSegment_ pType_ =
  ListSegmentReferences'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      segment = pSegment_,
      type' = pType_
    }

-- | The maximum number of results to include in the response. If you omit
-- this, the default of 50 is used.
listSegmentReferences_maxResults :: Lens.Lens' ListSegmentReferences (Prelude.Maybe Prelude.Natural)
listSegmentReferences_maxResults = Lens.lens (\ListSegmentReferences' {maxResults} -> maxResults) (\s@ListSegmentReferences' {} a -> s {maxResults = a} :: ListSegmentReferences)

-- | The token to use when requesting the next set of results. You received
-- this token from a previous @ListSegmentReferences@ operation.
listSegmentReferences_nextToken :: Lens.Lens' ListSegmentReferences (Prelude.Maybe Prelude.Text)
listSegmentReferences_nextToken = Lens.lens (\ListSegmentReferences' {nextToken} -> nextToken) (\s@ListSegmentReferences' {} a -> s {nextToken = a} :: ListSegmentReferences)

-- | The ARN of the segment that you want to view information for.
listSegmentReferences_segment :: Lens.Lens' ListSegmentReferences Prelude.Text
listSegmentReferences_segment = Lens.lens (\ListSegmentReferences' {segment} -> segment) (\s@ListSegmentReferences' {} a -> s {segment = a} :: ListSegmentReferences)

-- | Specifies whether to return information about launches or experiments
-- that use this segment.
listSegmentReferences_type :: Lens.Lens' ListSegmentReferences SegmentReferenceResourceType
listSegmentReferences_type = Lens.lens (\ListSegmentReferences' {type'} -> type') (\s@ListSegmentReferences' {} a -> s {type' = a} :: ListSegmentReferences)

instance Core.AWSPager ListSegmentReferences where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSegmentReferencesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSegmentReferencesResponse_referencedBy
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSegmentReferences_nextToken
          Lens..~ rs
          Lens.^? listSegmentReferencesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSegmentReferences where
  type
    AWSResponse ListSegmentReferences =
      ListSegmentReferencesResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSegmentReferencesResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "referencedBy" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSegmentReferences where
  hashWithSalt _salt ListSegmentReferences' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` segment
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListSegmentReferences where
  rnf ListSegmentReferences' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf segment
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListSegmentReferences where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListSegmentReferences where
  toPath ListSegmentReferences' {..} =
    Prelude.mconcat
      ["/segments/", Data.toBS segment, "/references"]

instance Data.ToQuery ListSegmentReferences where
  toQuery ListSegmentReferences' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken,
        "type" Data.=: type'
      ]

-- | /See:/ 'newListSegmentReferencesResponse' smart constructor.
data ListSegmentReferencesResponse = ListSegmentReferencesResponse'
  { -- | The token to use in a subsequent @ListSegmentReferences@ operation to
    -- return the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of structures, where each structure contains information about
    -- one experiment or launch that uses this segment.
    referencedBy :: Prelude.Maybe [RefResource],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSegmentReferencesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSegmentReferencesResponse_nextToken' - The token to use in a subsequent @ListSegmentReferences@ operation to
-- return the next set of results.
--
-- 'referencedBy', 'listSegmentReferencesResponse_referencedBy' - An array of structures, where each structure contains information about
-- one experiment or launch that uses this segment.
--
-- 'httpStatus', 'listSegmentReferencesResponse_httpStatus' - The response's http status code.
newListSegmentReferencesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSegmentReferencesResponse
newListSegmentReferencesResponse pHttpStatus_ =
  ListSegmentReferencesResponse'
    { nextToken =
        Prelude.Nothing,
      referencedBy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use in a subsequent @ListSegmentReferences@ operation to
-- return the next set of results.
listSegmentReferencesResponse_nextToken :: Lens.Lens' ListSegmentReferencesResponse (Prelude.Maybe Prelude.Text)
listSegmentReferencesResponse_nextToken = Lens.lens (\ListSegmentReferencesResponse' {nextToken} -> nextToken) (\s@ListSegmentReferencesResponse' {} a -> s {nextToken = a} :: ListSegmentReferencesResponse)

-- | An array of structures, where each structure contains information about
-- one experiment or launch that uses this segment.
listSegmentReferencesResponse_referencedBy :: Lens.Lens' ListSegmentReferencesResponse (Prelude.Maybe [RefResource])
listSegmentReferencesResponse_referencedBy = Lens.lens (\ListSegmentReferencesResponse' {referencedBy} -> referencedBy) (\s@ListSegmentReferencesResponse' {} a -> s {referencedBy = a} :: ListSegmentReferencesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSegmentReferencesResponse_httpStatus :: Lens.Lens' ListSegmentReferencesResponse Prelude.Int
listSegmentReferencesResponse_httpStatus = Lens.lens (\ListSegmentReferencesResponse' {httpStatus} -> httpStatus) (\s@ListSegmentReferencesResponse' {} a -> s {httpStatus = a} :: ListSegmentReferencesResponse)

instance Prelude.NFData ListSegmentReferencesResponse where
  rnf ListSegmentReferencesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf referencedBy
      `Prelude.seq` Prelude.rnf httpStatus
