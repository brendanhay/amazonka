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
-- Module      : Network.AWS.DMS.DescribeEventCategories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists categories for all event source types, or, if specified, for a
-- specified source type. You can see a list of the event categories and
-- source types in
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Events.html Working with Events and Notifications>
-- in the /AWS Database Migration Service User Guide./
module Network.AWS.DMS.DescribeEventCategories
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

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | Filters applied to the event categories.
    filters :: Core.Maybe [Filter],
    -- | The type of AWS DMS resource that generates events.
    --
    -- Valid values: replication-instance | replication-task
    sourceType :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'sourceType', 'describeEventCategories_sourceType' - The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
newDescribeEventCategories ::
  DescribeEventCategories
newDescribeEventCategories =
  DescribeEventCategories'
    { filters = Core.Nothing,
      sourceType = Core.Nothing
    }

-- | Filters applied to the event categories.
describeEventCategories_filters :: Lens.Lens' DescribeEventCategories (Core.Maybe [Filter])
describeEventCategories_filters = Lens.lens (\DescribeEventCategories' {filters} -> filters) (\s@DescribeEventCategories' {} a -> s {filters = a} :: DescribeEventCategories) Core.. Lens.mapping Lens._Coerce

-- | The type of AWS DMS resource that generates events.
--
-- Valid values: replication-instance | replication-task
describeEventCategories_sourceType :: Lens.Lens' DescribeEventCategories (Core.Maybe Core.Text)
describeEventCategories_sourceType = Lens.lens (\DescribeEventCategories' {sourceType} -> sourceType) (\s@DescribeEventCategories' {} a -> s {sourceType = a} :: DescribeEventCategories)

instance Core.AWSRequest DescribeEventCategories where
  type
    AWSResponse DescribeEventCategories =
      DescribeEventCategoriesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Core.<$> ( x Core..?> "EventCategoryGroupList"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventCategories

instance Core.NFData DescribeEventCategories

instance Core.ToHeaders DescribeEventCategories where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.DescribeEventCategories" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventCategories where
  toJSON DescribeEventCategories' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Filters" Core..=) Core.<$> filters,
            ("SourceType" Core..=) Core.<$> sourceType
          ]
      )

instance Core.ToPath DescribeEventCategories where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventCategories where
  toQuery = Core.const Core.mempty

-- |
--
-- /See:/ 'newDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories.
    eventCategoryGroupList :: Core.Maybe [EventCategoryGroup],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeEventCategoriesResponse
newDescribeEventCategoriesResponse pHttpStatus_ =
  DescribeEventCategoriesResponse'
    { eventCategoryGroupList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event categories.
describeEventCategoriesResponse_eventCategoryGroupList :: Lens.Lens' DescribeEventCategoriesResponse (Core.Maybe [EventCategoryGroup])
describeEventCategoriesResponse_eventCategoryGroupList = Lens.lens (\DescribeEventCategoriesResponse' {eventCategoryGroupList} -> eventCategoryGroupList) (\s@DescribeEventCategoriesResponse' {} a -> s {eventCategoryGroupList = a} :: DescribeEventCategoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventCategoriesResponse_httpStatus :: Lens.Lens' DescribeEventCategoriesResponse Core.Int
describeEventCategoriesResponse_httpStatus = Lens.lens (\DescribeEventCategoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventCategoriesResponse' {} a -> s {httpStatus = a} :: DescribeEventCategoriesResponse)

instance Core.NFData DescribeEventCategoriesResponse
