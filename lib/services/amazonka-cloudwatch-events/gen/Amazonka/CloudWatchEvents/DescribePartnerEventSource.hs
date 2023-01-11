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
-- Module      : Amazonka.CloudWatchEvents.DescribePartnerEventSource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list details about a partner
-- event source that they have created. Amazon Web Services customers do
-- not use this operation. Instead, Amazon Web Services customers can use
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_DescribeEventSource.html DescribeEventSource>
-- to see details about a partner event source that is shared with them.
module Amazonka.CloudWatchEvents.DescribePartnerEventSource
  ( -- * Creating a Request
    DescribePartnerEventSource (..),
    newDescribePartnerEventSource,

    -- * Request Lenses
    describePartnerEventSource_name,

    -- * Destructuring the Response
    DescribePartnerEventSourceResponse (..),
    newDescribePartnerEventSourceResponse,

    -- * Response Lenses
    describePartnerEventSourceResponse_arn,
    describePartnerEventSourceResponse_name,
    describePartnerEventSourceResponse_httpStatus,
  )
where

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePartnerEventSource' smart constructor.
data DescribePartnerEventSource = DescribePartnerEventSource'
  { -- | The name of the event source to display.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePartnerEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describePartnerEventSource_name' - The name of the event source to display.
newDescribePartnerEventSource ::
  -- | 'name'
  Prelude.Text ->
  DescribePartnerEventSource
newDescribePartnerEventSource pName_ =
  DescribePartnerEventSource' {name = pName_}

-- | The name of the event source to display.
describePartnerEventSource_name :: Lens.Lens' DescribePartnerEventSource Prelude.Text
describePartnerEventSource_name = Lens.lens (\DescribePartnerEventSource' {name} -> name) (\s@DescribePartnerEventSource' {} a -> s {name = a} :: DescribePartnerEventSource)

instance Core.AWSRequest DescribePartnerEventSource where
  type
    AWSResponse DescribePartnerEventSource =
      DescribePartnerEventSourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePartnerEventSourceResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePartnerEventSource where
  hashWithSalt _salt DescribePartnerEventSource' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribePartnerEventSource where
  rnf DescribePartnerEventSource' {..} =
    Prelude.rnf name

instance Data.ToHeaders DescribePartnerEventSource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSEvents.DescribePartnerEventSource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribePartnerEventSource where
  toJSON DescribePartnerEventSource' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath DescribePartnerEventSource where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribePartnerEventSource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePartnerEventSourceResponse' smart constructor.
data DescribePartnerEventSourceResponse = DescribePartnerEventSourceResponse'
  { -- | The ARN of the event source.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the event source.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePartnerEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describePartnerEventSourceResponse_arn' - The ARN of the event source.
--
-- 'name', 'describePartnerEventSourceResponse_name' - The name of the event source.
--
-- 'httpStatus', 'describePartnerEventSourceResponse_httpStatus' - The response's http status code.
newDescribePartnerEventSourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePartnerEventSourceResponse
newDescribePartnerEventSourceResponse pHttpStatus_ =
  DescribePartnerEventSourceResponse'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the event source.
describePartnerEventSourceResponse_arn :: Lens.Lens' DescribePartnerEventSourceResponse (Prelude.Maybe Prelude.Text)
describePartnerEventSourceResponse_arn = Lens.lens (\DescribePartnerEventSourceResponse' {arn} -> arn) (\s@DescribePartnerEventSourceResponse' {} a -> s {arn = a} :: DescribePartnerEventSourceResponse)

-- | The name of the event source.
describePartnerEventSourceResponse_name :: Lens.Lens' DescribePartnerEventSourceResponse (Prelude.Maybe Prelude.Text)
describePartnerEventSourceResponse_name = Lens.lens (\DescribePartnerEventSourceResponse' {name} -> name) (\s@DescribePartnerEventSourceResponse' {} a -> s {name = a} :: DescribePartnerEventSourceResponse)

-- | The response's http status code.
describePartnerEventSourceResponse_httpStatus :: Lens.Lens' DescribePartnerEventSourceResponse Prelude.Int
describePartnerEventSourceResponse_httpStatus = Lens.lens (\DescribePartnerEventSourceResponse' {httpStatus} -> httpStatus) (\s@DescribePartnerEventSourceResponse' {} a -> s {httpStatus = a} :: DescribePartnerEventSourceResponse)

instance
  Prelude.NFData
    DescribePartnerEventSourceResponse
  where
  rnf DescribePartnerEventSourceResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
