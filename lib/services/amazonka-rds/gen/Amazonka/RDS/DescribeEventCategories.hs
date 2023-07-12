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
-- Module      : Amazonka.RDS.DescribeEventCategories
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays a list of categories for all event source types, or, if
-- specified, for a specified source type. You can also see this list in
-- the \"Amazon RDS event categories and event messages\" section of the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Events.Messages.html Amazon RDS User Guide>
-- or the
-- <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/USER_Events.Messages.html Amazon Aurora User Guide>
-- .
module Amazonka.RDS.DescribeEventCategories
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
    describeEventCategoriesResponse_eventCategoriesMapList,
    describeEventCategoriesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RDS.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newDescribeEventCategories' smart constructor.
data DescribeEventCategories = DescribeEventCategories'
  { -- | This parameter isn\'t currently supported.
    filters :: Prelude.Maybe [Filter],
    -- | The type of source that is generating the events. For RDS Proxy events,
    -- specify @db-proxy@.
    --
    -- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
    -- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ | @db-proxy@
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
-- 'filters', 'describeEventCategories_filters' - This parameter isn\'t currently supported.
--
-- 'sourceType', 'describeEventCategories_sourceType' - The type of source that is generating the events. For RDS Proxy events,
-- specify @db-proxy@.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
-- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ | @db-proxy@
newDescribeEventCategories ::
  DescribeEventCategories
newDescribeEventCategories =
  DescribeEventCategories'
    { filters = Prelude.Nothing,
      sourceType = Prelude.Nothing
    }

-- | This parameter isn\'t currently supported.
describeEventCategories_filters :: Lens.Lens' DescribeEventCategories (Prelude.Maybe [Filter])
describeEventCategories_filters = Lens.lens (\DescribeEventCategories' {filters} -> filters) (\s@DescribeEventCategories' {} a -> s {filters = a} :: DescribeEventCategories) Prelude.. Lens.mapping Lens.coerced

-- | The type of source that is generating the events. For RDS Proxy events,
-- specify @db-proxy@.
--
-- Valid values: @db-instance@ | @db-cluster@ | @db-parameter-group@ |
-- @db-security-group@ | @db-snapshot@ | @db-cluster-snapshot@ | @db-proxy@
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
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` sourceType

instance Prelude.NFData DescribeEventCategories where
  rnf DescribeEventCategories' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf sourceType

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
          Data.=: ("2014-10-31" :: Prelude.ByteString),
        "Filters"
          Data.=: Data.toQuery
            (Data.toQueryList "Filter" Prelude.<$> filters),
        "SourceType" Data.=: sourceType
      ]

-- | Data returned from the @DescribeEventCategories@ operation.
--
-- /See:/ 'newDescribeEventCategoriesResponse' smart constructor.
data DescribeEventCategoriesResponse = DescribeEventCategoriesResponse'
  { -- | A list of @EventCategoriesMap@ data types.
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
-- 'eventCategoriesMapList', 'describeEventCategoriesResponse_eventCategoriesMapList' - A list of @EventCategoriesMap@ data types.
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

-- | A list of @EventCategoriesMap@ data types.
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
