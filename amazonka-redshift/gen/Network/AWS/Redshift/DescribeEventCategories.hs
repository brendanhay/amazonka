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
-- Module      : Network.AWS.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source
-- types, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications>.
module Network.AWS.Redshift.DescribeEventCategories
  ( -- * Creating a Request
    DescribeEventCategories (..),
    newDescribeEventCategories,

    -- * Request Lenses
    describeEventCategories_sourceType,

    -- * Destructuring the Response
    DescribeEventCategoriesResponse (..),
    newDescribeEventCategoriesResponse,

    -- * Response Lenses
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | The source type, such as cluster or parameter group, to which the
    -- described event categories apply.
    --
    -- Valid values: cluster, cluster-snapshot, cluster-parameter-group,
    -- cluster-security-group, and scheduled-action.
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
-- 'sourceType', 'describeEventCategories_sourceType' - The source type, such as cluster or parameter group, to which the
-- described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group,
-- cluster-security-group, and scheduled-action.
newDescribeEventCategories ::
  DescribeEventCategories
newDescribeEventCategories =
  DescribeEventCategories' {sourceType = Core.Nothing}

-- | The source type, such as cluster or parameter group, to which the
-- described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group,
-- cluster-security-group, and scheduled-action.
describeEventCategories_sourceType :: Lens.Lens' DescribeEventCategories (Core.Maybe Core.Text)
describeEventCategories_sourceType = Lens.lens (\DescribeEventCategories' {sourceType} -> sourceType) (\s@DescribeEventCategories' {} a -> s {sourceType = a} :: DescribeEventCategories)

instance Core.AWSRequest DescribeEventCategories where
  type
    AWSResponse DescribeEventCategories =
      DescribeEventCategoriesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeEventCategoriesResult"
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Core.<$> ( x Core..@? "EventCategoriesMapList"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "EventCategoriesMap")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventCategories

instance Core.NFData DescribeEventCategories

instance Core.ToHeaders DescribeEventCategories where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeEventCategories where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventCategories where
  toQuery DescribeEventCategories' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DescribeEventCategories" :: Core.ByteString),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "SourceType" Core.=: sourceType
      ]

-- |
--
-- /See:/ 'newDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories descriptions.
    eventCategoriesMapList :: Core.Maybe [EventCategoriesMap],
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
-- 'eventCategoriesMapList', 'describeEventCategoriesResponse_eventCategoriesMapList' - A list of event categories descriptions.
--
-- 'httpStatus', 'describeEventCategoriesResponse_httpStatus' - The response's http status code.
newDescribeEventCategoriesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventCategoriesResponse
newDescribeEventCategoriesResponse pHttpStatus_ =
  DescribeEventCategoriesResponse'
    { eventCategoriesMapList =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event categories descriptions.
describeEventCategoriesResponse_eventCategoriesMapList :: Lens.Lens' DescribeEventCategoriesResponse (Core.Maybe [EventCategoriesMap])
describeEventCategoriesResponse_eventCategoriesMapList = Lens.lens (\DescribeEventCategoriesResponse' {eventCategoriesMapList} -> eventCategoriesMapList) (\s@DescribeEventCategoriesResponse' {} a -> s {eventCategoriesMapList = a} :: DescribeEventCategoriesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeEventCategoriesResponse_httpStatus :: Lens.Lens' DescribeEventCategoriesResponse Core.Int
describeEventCategoriesResponse_httpStatus = Lens.lens (\DescribeEventCategoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventCategoriesResponse' {} a -> s {httpStatus = a} :: DescribeEventCategoriesResponse)

instance Core.NFData DescribeEventCategoriesResponse
