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
-- Module      : Amazonka.DMS.DescribeEventCategories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists categories for all event source types, or, if specified, for a
-- specified source type. You can see a list of the event categories and
-- source types in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
-- in the /Database Migration Service User Guide./
module Amazonka.DMS.DescribeEventCategories
  ( -- * Creating a Request
    DescribeEventCategories (..),
    newDescribeEventCategories,

    -- * Request Lenses
    describeEventCategories_filters,
    describeEventCategories_sourceType,

    -- * Destructuring the Response
    DescribeEventCategoriesResponse (..),
    newDescribeEventCategoriesResponse,

    -- * Response Lenses
    describeEventCategoriesResponse_eventCategoryGroupList,
    describeEventCategoriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | Filters applied to the event categories.
    filters :: Prelude.Maybe [Filter],
    -- | The type of DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventCategories' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeEventCategories_filters' - Filters applied to the event categories.
--
-- 'sourceType', 'describeEventCategories_sourceType' - The type of DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
newDescribeEventCategories ::
  DescribeEventCategories
newDescribeEventCategories =
  DescribeEventCategories'
    { filters = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | Filters applied to the event categories.
describeEventCategories_filters :: Lens.Lens' DescribeEventCategories (Prelude.Maybe [Filter])
describeEventCategories_filters = Lens.lens (\DescribeEventCategories' {filters} -> filters) (\s@DescribeEventCategories' {} a -> s {filters = a} :: DescribeEventCategories) Prelude.. Lens.mapping Lens.coerced

-- | The type of DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
describeEventCategories_sourceType :: Lens.Lens' DescribeEventCategories (Prelude.Maybe Prelude.Text)
describeEventCategories_sourceType = Lens.lens (\DescribeEventCategories' {sourceType} -> sourceType) (\s@DescribeEventCategories' {} a -> s {sourceType = a} :: DescribeEventCategories)

instance Core.AWSRequest DescribeEventCategories where
  type
    AWSResponse DescribeEventCategories =
      DescribeEventCategoriesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Prelude.<$> ( x
                            Data..?> "EventCategoryGroupList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventCategories where
  hashWithSalt _salt DescribeEventCategories' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData DescribeEventCategories where
  rnf DescribeEventCategories' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sourceType

instance Data.ToHeaders DescribeEventCategories where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.DescribeEventCategories" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventCategories where
  toJSON DescribeEventCategories' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("SourceType" Data..=) Prelude.<$> sourceType
          ]
      )

instance Data.ToPath DescribeEventCategories where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventCategories where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories.
    eventCategoryGroupList :: Prelude.Maybe [EventCategoryGroup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeEventCategoriesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventCategoryGroupList', 'describeEventCategoriesResponse_eventCategoryGroupList' - A list of event categories.
--
-- 'httpStatus', 'describeEventCategoriesResponse_httpStatus' - The response's http status code.
newDescribeEventCategoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventCategoriesResponse
newDescribeEventCategoriesResponse pHttpStatus_ =
  DescribeEventCategoriesResponse'
    { eventCategoryGroupList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event categories.
describeEventCategoriesResponse_eventCategoryGroupList :: Lens.Lens' DescribeEventCategoriesResponse (Prelude.Maybe [EventCategoryGroup])
describeEventCategoriesResponse_eventCategoryGroupList = Lens.lens (\DescribeEventCategoriesResponse' {eventCategoryGroupList} -> eventCategoryGroupList) (\s@DescribeEventCategoriesResponse' {} a -> s {eventCategoryGroupList = a} :: DescribeEventCategoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEventCategoriesResponse_httpStatus :: Lens.Lens' DescribeEventCategoriesResponse Prelude.Int
describeEventCategoriesResponse_httpStatus = Lens.lens (\DescribeEventCategoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventCategoriesResponse' {} a -> s {httpStatus = a} :: DescribeEventCategoriesResponse)

instance
  Prelude.NFData
    DescribeEventCategoriesResponse
  where
  rnf DescribeEventCategoriesResponse' {..} =
    Prelude.rnf eventCategoryGroupList
      `Prelude.seq` Prelude.rnf httpStatus
