{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.MediaConvert.ListJobTemplates
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve a JSON array of up to twenty of your job templates. This will
-- return the templates themselves, not just a list of them. To retrieve
-- the next twenty templates, use the nextToken string returned with the
-- array
--
-- This operation returns paginated results.
module Network.AWS.MediaConvert.ListJobTemplates
  ( -- * Creating a Request
    ListJobTemplates (..),
    newListJobTemplates,

    -- * Request Lenses
    listJobTemplates_nextToken,
    listJobTemplates_listBy,
    listJobTemplates_maxResults,
    listJobTemplates_category,
    listJobTemplates_order,

    -- * Destructuring the Response
    ListJobTemplatesResponse (..),
    newListJobTemplatesResponse,

    -- * Response Lenses
    listJobTemplatesResponse_jobTemplates,
    listJobTemplatesResponse_nextToken,
    listJobTemplatesResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaConvert.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListJobTemplates' smart constructor.
data ListJobTemplates = ListJobTemplates'
  { -- | Use this string, provided with the response to a previous request, to
    -- request the next batch of job templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Optional. When you request a list of job templates, you can choose to
    -- list them alphabetically by NAME or chronologically by CREATION_DATE. If
    -- you don\'t specify, the service will list them by name.
    listBy :: Prelude.Maybe JobTemplateListBy,
    -- | Optional. Number of job templates, up to twenty, that will be returned
    -- at one time.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Optionally, specify a job template category to limit responses to only
    -- job templates from that category.
    category :: Prelude.Maybe Prelude.Text,
    -- | Optional. When you request lists of resources, you can specify whether
    -- they are sorted in ASCENDING or DESCENDING order. Default varies by
    -- resource.
    order :: Prelude.Maybe Order
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListJobTemplates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listJobTemplates_nextToken' - Use this string, provided with the response to a previous request, to
-- request the next batch of job templates.
--
-- 'listBy', 'listJobTemplates_listBy' - Optional. When you request a list of job templates, you can choose to
-- list them alphabetically by NAME or chronologically by CREATION_DATE. If
-- you don\'t specify, the service will list them by name.
--
-- 'maxResults', 'listJobTemplates_maxResults' - Optional. Number of job templates, up to twenty, that will be returned
-- at one time.
--
-- 'category', 'listJobTemplates_category' - Optionally, specify a job template category to limit responses to only
-- job templates from that category.
--
-- 'order', 'listJobTemplates_order' - Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
newListJobTemplates ::
  ListJobTemplates
newListJobTemplates =
  ListJobTemplates'
    { nextToken = Prelude.Nothing,
      listBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      category = Prelude.Nothing,
      order = Prelude.Nothing
    }

-- | Use this string, provided with the response to a previous request, to
-- request the next batch of job templates.
listJobTemplates_nextToken :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.Text)
listJobTemplates_nextToken = Lens.lens (\ListJobTemplates' {nextToken} -> nextToken) (\s@ListJobTemplates' {} a -> s {nextToken = a} :: ListJobTemplates)

-- | Optional. When you request a list of job templates, you can choose to
-- list them alphabetically by NAME or chronologically by CREATION_DATE. If
-- you don\'t specify, the service will list them by name.
listJobTemplates_listBy :: Lens.Lens' ListJobTemplates (Prelude.Maybe JobTemplateListBy)
listJobTemplates_listBy = Lens.lens (\ListJobTemplates' {listBy} -> listBy) (\s@ListJobTemplates' {} a -> s {listBy = a} :: ListJobTemplates)

-- | Optional. Number of job templates, up to twenty, that will be returned
-- at one time.
listJobTemplates_maxResults :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.Natural)
listJobTemplates_maxResults = Lens.lens (\ListJobTemplates' {maxResults} -> maxResults) (\s@ListJobTemplates' {} a -> s {maxResults = a} :: ListJobTemplates)

-- | Optionally, specify a job template category to limit responses to only
-- job templates from that category.
listJobTemplates_category :: Lens.Lens' ListJobTemplates (Prelude.Maybe Prelude.Text)
listJobTemplates_category = Lens.lens (\ListJobTemplates' {category} -> category) (\s@ListJobTemplates' {} a -> s {category = a} :: ListJobTemplates)

-- | Optional. When you request lists of resources, you can specify whether
-- they are sorted in ASCENDING or DESCENDING order. Default varies by
-- resource.
listJobTemplates_order :: Lens.Lens' ListJobTemplates (Prelude.Maybe Order)
listJobTemplates_order = Lens.lens (\ListJobTemplates' {order} -> order) (\s@ListJobTemplates' {} a -> s {order = a} :: ListJobTemplates)

instance Pager.AWSPager ListJobTemplates where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listJobTemplatesResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? listJobTemplatesResponse_jobTemplates
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listJobTemplates_nextToken
          Lens..~ rs
          Lens.^? listJobTemplatesResponse_nextToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest ListJobTemplates where
  type Rs ListJobTemplates = ListJobTemplatesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListJobTemplatesResponse'
            Prelude.<$> ( x Prelude..?> "jobTemplates"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListJobTemplates

instance Prelude.NFData ListJobTemplates

instance Prelude.ToHeaders ListJobTemplates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToPath ListJobTemplates where
  toPath = Prelude.const "/2017-08-29/jobTemplates"

instance Prelude.ToQuery ListJobTemplates where
  toQuery ListJobTemplates' {..} =
    Prelude.mconcat
      [ "nextToken" Prelude.=: nextToken,
        "listBy" Prelude.=: listBy,
        "maxResults" Prelude.=: maxResults,
        "category" Prelude.=: category,
        "order" Prelude.=: order
      ]

-- | /See:/ 'newListJobTemplatesResponse' smart constructor.
data ListJobTemplatesResponse = ListJobTemplatesResponse'
  { -- | List of Job templates.
    jobTemplates :: Prelude.Maybe [JobTemplate],
    -- | Use this string to request the next batch of job templates.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListJobTemplatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobTemplates', 'listJobTemplatesResponse_jobTemplates' - List of Job templates.
--
-- 'nextToken', 'listJobTemplatesResponse_nextToken' - Use this string to request the next batch of job templates.
--
-- 'httpStatus', 'listJobTemplatesResponse_httpStatus' - The response's http status code.
newListJobTemplatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListJobTemplatesResponse
newListJobTemplatesResponse pHttpStatus_ =
  ListJobTemplatesResponse'
    { jobTemplates =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of Job templates.
listJobTemplatesResponse_jobTemplates :: Lens.Lens' ListJobTemplatesResponse (Prelude.Maybe [JobTemplate])
listJobTemplatesResponse_jobTemplates = Lens.lens (\ListJobTemplatesResponse' {jobTemplates} -> jobTemplates) (\s@ListJobTemplatesResponse' {} a -> s {jobTemplates = a} :: ListJobTemplatesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | Use this string to request the next batch of job templates.
listJobTemplatesResponse_nextToken :: Lens.Lens' ListJobTemplatesResponse (Prelude.Maybe Prelude.Text)
listJobTemplatesResponse_nextToken = Lens.lens (\ListJobTemplatesResponse' {nextToken} -> nextToken) (\s@ListJobTemplatesResponse' {} a -> s {nextToken = a} :: ListJobTemplatesResponse)

-- | The response's http status code.
listJobTemplatesResponse_httpStatus :: Lens.Lens' ListJobTemplatesResponse Prelude.Int
listJobTemplatesResponse_httpStatus = Lens.lens (\ListJobTemplatesResponse' {httpStatus} -> httpStatus) (\s@ListJobTemplatesResponse' {} a -> s {httpStatus = a} :: ListJobTemplatesResponse)

instance Prelude.NFData ListJobTemplatesResponse
