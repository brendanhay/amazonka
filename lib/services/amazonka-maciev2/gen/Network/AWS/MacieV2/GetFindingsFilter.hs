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
-- Module      : Network.AWS.MacieV2.GetFindingsFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the criteria and other settings for a findings filter.
module Network.AWS.MacieV2.GetFindingsFilter
  ( -- * Creating a Request
    GetFindingsFilter (..),
    newGetFindingsFilter,

    -- * Request Lenses
    getFindingsFilter_id,

    -- * Destructuring the Response
    GetFindingsFilterResponse (..),
    newGetFindingsFilterResponse,

    -- * Response Lenses
    getFindingsFilterResponse_arn,
    getFindingsFilterResponse_findingCriteria,
    getFindingsFilterResponse_action,
    getFindingsFilterResponse_name,
    getFindingsFilterResponse_id,
    getFindingsFilterResponse_description,
    getFindingsFilterResponse_tags,
    getFindingsFilterResponse_position,
    getFindingsFilterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MacieV2.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFindingsFilter' smart constructor.
data GetFindingsFilter = GetFindingsFilter'
  { -- | The unique identifier for the Amazon Macie resource or account that the
    -- request applies to.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getFindingsFilter_id' - The unique identifier for the Amazon Macie resource or account that the
-- request applies to.
newGetFindingsFilter ::
  -- | 'id'
  Prelude.Text ->
  GetFindingsFilter
newGetFindingsFilter pId_ =
  GetFindingsFilter' {id = pId_}

-- | The unique identifier for the Amazon Macie resource or account that the
-- request applies to.
getFindingsFilter_id :: Lens.Lens' GetFindingsFilter Prelude.Text
getFindingsFilter_id = Lens.lens (\GetFindingsFilter' {id} -> id) (\s@GetFindingsFilter' {} a -> s {id = a} :: GetFindingsFilter)

instance Core.AWSRequest GetFindingsFilter where
  type
    AWSResponse GetFindingsFilter =
      GetFindingsFilterResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFindingsFilterResponse'
            Prelude.<$> (x Core..?> "arn")
            Prelude.<*> (x Core..?> "findingCriteria")
            Prelude.<*> (x Core..?> "action")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "id")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (x Core..?> "tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "position")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFindingsFilter

instance Prelude.NFData GetFindingsFilter

instance Core.ToHeaders GetFindingsFilter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetFindingsFilter where
  toPath GetFindingsFilter' {..} =
    Prelude.mconcat ["/findingsfilters/", Core.toBS id]

instance Core.ToQuery GetFindingsFilter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFindingsFilterResponse' smart constructor.
data GetFindingsFilterResponse = GetFindingsFilterResponse'
  { -- | The Amazon Resource Name (ARN) of the filter.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The criteria that\'s used to filter findings.
    findingCriteria :: Prelude.Maybe FindingCriteria,
    -- | The action that\'s performed on findings that meet the filter criteria
    -- (findingCriteria). Possible values are: ARCHIVE, suppress (automatically
    -- archive) the findings; and, NOOP, don\'t perform any action on the
    -- findings.
    action :: Prelude.Maybe FindingsFilterAction,
    -- | The custom name of the filter.
    name :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the filter.
    id :: Prelude.Maybe Prelude.Text,
    -- | The custom description of the filter.
    description :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs that identifies the tags (keys and values) that
    -- are associated with the filter.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The position of the filter in the list of saved filters on the Amazon
    -- Macie console. This value also determines the order in which the filter
    -- is applied to findings, relative to other filters that are also applied
    -- to the findings.
    position :: Prelude.Maybe Prelude.Int,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetFindingsFilterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getFindingsFilterResponse_arn' - The Amazon Resource Name (ARN) of the filter.
--
-- 'findingCriteria', 'getFindingsFilterResponse_findingCriteria' - The criteria that\'s used to filter findings.
--
-- 'action', 'getFindingsFilterResponse_action' - The action that\'s performed on findings that meet the filter criteria
-- (findingCriteria). Possible values are: ARCHIVE, suppress (automatically
-- archive) the findings; and, NOOP, don\'t perform any action on the
-- findings.
--
-- 'name', 'getFindingsFilterResponse_name' - The custom name of the filter.
--
-- 'id', 'getFindingsFilterResponse_id' - The unique identifier for the filter.
--
-- 'description', 'getFindingsFilterResponse_description' - The custom description of the filter.
--
-- 'tags', 'getFindingsFilterResponse_tags' - A map of key-value pairs that identifies the tags (keys and values) that
-- are associated with the filter.
--
-- 'position', 'getFindingsFilterResponse_position' - The position of the filter in the list of saved filters on the Amazon
-- Macie console. This value also determines the order in which the filter
-- is applied to findings, relative to other filters that are also applied
-- to the findings.
--
-- 'httpStatus', 'getFindingsFilterResponse_httpStatus' - The response's http status code.
newGetFindingsFilterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFindingsFilterResponse
newGetFindingsFilterResponse pHttpStatus_ =
  GetFindingsFilterResponse'
    { arn = Prelude.Nothing,
      findingCriteria = Prelude.Nothing,
      action = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      position = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the filter.
getFindingsFilterResponse_arn :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe Prelude.Text)
getFindingsFilterResponse_arn = Lens.lens (\GetFindingsFilterResponse' {arn} -> arn) (\s@GetFindingsFilterResponse' {} a -> s {arn = a} :: GetFindingsFilterResponse)

-- | The criteria that\'s used to filter findings.
getFindingsFilterResponse_findingCriteria :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe FindingCriteria)
getFindingsFilterResponse_findingCriteria = Lens.lens (\GetFindingsFilterResponse' {findingCriteria} -> findingCriteria) (\s@GetFindingsFilterResponse' {} a -> s {findingCriteria = a} :: GetFindingsFilterResponse)

-- | The action that\'s performed on findings that meet the filter criteria
-- (findingCriteria). Possible values are: ARCHIVE, suppress (automatically
-- archive) the findings; and, NOOP, don\'t perform any action on the
-- findings.
getFindingsFilterResponse_action :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe FindingsFilterAction)
getFindingsFilterResponse_action = Lens.lens (\GetFindingsFilterResponse' {action} -> action) (\s@GetFindingsFilterResponse' {} a -> s {action = a} :: GetFindingsFilterResponse)

-- | The custom name of the filter.
getFindingsFilterResponse_name :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe Prelude.Text)
getFindingsFilterResponse_name = Lens.lens (\GetFindingsFilterResponse' {name} -> name) (\s@GetFindingsFilterResponse' {} a -> s {name = a} :: GetFindingsFilterResponse)

-- | The unique identifier for the filter.
getFindingsFilterResponse_id :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe Prelude.Text)
getFindingsFilterResponse_id = Lens.lens (\GetFindingsFilterResponse' {id} -> id) (\s@GetFindingsFilterResponse' {} a -> s {id = a} :: GetFindingsFilterResponse)

-- | The custom description of the filter.
getFindingsFilterResponse_description :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe Prelude.Text)
getFindingsFilterResponse_description = Lens.lens (\GetFindingsFilterResponse' {description} -> description) (\s@GetFindingsFilterResponse' {} a -> s {description = a} :: GetFindingsFilterResponse)

-- | A map of key-value pairs that identifies the tags (keys and values) that
-- are associated with the filter.
getFindingsFilterResponse_tags :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getFindingsFilterResponse_tags = Lens.lens (\GetFindingsFilterResponse' {tags} -> tags) (\s@GetFindingsFilterResponse' {} a -> s {tags = a} :: GetFindingsFilterResponse) Prelude.. Lens.mapping Lens.coerced

-- | The position of the filter in the list of saved filters on the Amazon
-- Macie console. This value also determines the order in which the filter
-- is applied to findings, relative to other filters that are also applied
-- to the findings.
getFindingsFilterResponse_position :: Lens.Lens' GetFindingsFilterResponse (Prelude.Maybe Prelude.Int)
getFindingsFilterResponse_position = Lens.lens (\GetFindingsFilterResponse' {position} -> position) (\s@GetFindingsFilterResponse' {} a -> s {position = a} :: GetFindingsFilterResponse)

-- | The response's http status code.
getFindingsFilterResponse_httpStatus :: Lens.Lens' GetFindingsFilterResponse Prelude.Int
getFindingsFilterResponse_httpStatus = Lens.lens (\GetFindingsFilterResponse' {httpStatus} -> httpStatus) (\s@GetFindingsFilterResponse' {} a -> s {httpStatus = a} :: GetFindingsFilterResponse)

instance Prelude.NFData GetFindingsFilterResponse
