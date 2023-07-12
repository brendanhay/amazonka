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
-- Module      : Amazonka.Redshift.DescribeEventCategories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of event categories for all event source types, or for a
-- specified source type. For a list of the event categories and source
-- types, go to
-- <https://docs.aws.amazon.com/redshift/latest/mgmt/working-with-event-notifications.html Amazon Redshift Event Notifications>.
module Amazonka.Redshift.DescribeEventCategories
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | The source type, such as cluster or parameter group, to which the
    -- described event categories apply.
    --
    -- Valid values: cluster, cluster-snapshot, cluster-parameter-group,
    -- cluster-security-group, and scheduled-action.
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
-- 'sourceType', 'describeEventCategories_sourceType' - The source type, such as cluster or parameter group, to which the
-- described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group,
-- cluster-security-group, and scheduled-action.
newDescribeEventCategories ::
  DescribeEventCategories
newDescribeEventCategories =
  DescribeEventCategories'
    { sourceType =
        Prelude.Nothing
    }

-- | The source type, such as cluster or parameter group, to which the
-- described event categories apply.
--
-- Valid values: cluster, cluster-snapshot, cluster-parameter-group,
-- cluster-security-group, and scheduled-action.
describeEventCategories_sourceType :: Lens.Lens' DescribeEventCategories (Prelude.Maybe Prelude.Text)
describeEventCategories_sourceType = Lens.lens (\DescribeEventCategories' {sourceType} -> sourceType) (\s@DescribeEventCategories' {} a -> s {sourceType = a} :: DescribeEventCategories)

instance Core.AWSRequest DescribeEventCategories where
  type
    AWSResponse DescribeEventCategories =
      DescribeEventCategoriesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DescribeEventCategoriesResult"
      ( \s h x ->
          DescribeEventCategoriesResponse'
            Prelude.<$> ( x
                            Data..@? "EventCategoriesMapList"
                            Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "EventCategoriesMap")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventCategories where
  hashWithSalt _salt DescribeEventCategories' {..} =
    _salt `Prelude.hashWithSalt` sourceType

instance Prelude.NFData DescribeEventCategories where
  rnf DescribeEventCategories' {..} =
    Prelude.rnf sourceType

instance Data.ToHeaders DescribeEventCategories where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeEventCategories where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventCategories where
  toQuery DescribeEventCategories' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DescribeEventCategories" :: Prelude.ByteString),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "SourceType" Data.=: sourceType
      ]

-- |
--
-- /See:/ 'newDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of event categories descriptions.
    eventCategoriesMapList :: Prelude.Maybe [EventCategoriesMap],
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
-- 'eventCategoriesMapList', 'describeEventCategoriesResponse_eventCategoriesMapList' - A list of event categories descriptions.
--
-- 'httpStatus', 'describeEventCategoriesResponse_httpStatus' - The response's http status code.
newDescribeEventCategoriesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeEventCategoriesResponse
newDescribeEventCategoriesResponse pHttpStatus_ =
  DescribeEventCategoriesResponse'
    { eventCategoriesMapList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of event categories descriptions.
describeEventCategoriesResponse_eventCategoriesMapList :: Lens.Lens' DescribeEventCategoriesResponse (Prelude.Maybe [EventCategoriesMap])
describeEventCategoriesResponse_eventCategoriesMapList = Lens.lens (\DescribeEventCategoriesResponse' {eventCategoriesMapList} -> eventCategoriesMapList) (\s@DescribeEventCategoriesResponse' {} a -> s {eventCategoriesMapList = a} :: DescribeEventCategoriesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeEventCategoriesResponse_httpStatus :: Lens.Lens' DescribeEventCategoriesResponse Prelude.Int
describeEventCategoriesResponse_httpStatus = Lens.lens (\DescribeEventCategoriesResponse' {httpStatus} -> httpStatus) (\s@DescribeEventCategoriesResponse' {} a -> s {httpStatus = a} :: DescribeEventCategoriesResponse)

instance
  Prelude.NFData
    DescribeEventCategoriesResponse
  where
  rnf DescribeEventCategoriesResponse' {..} =
    Prelude.rnf eventCategoriesMapList
      `Prelude.seq` Prelude.rnf httpStatus
