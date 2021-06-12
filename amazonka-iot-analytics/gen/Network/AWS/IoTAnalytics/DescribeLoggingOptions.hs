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
-- Module      : Network.AWS.IoTAnalytics.DescribeLoggingOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current settings of the AWS IoT Analytics logging options.
module Network.AWS.IoTAnalytics.DescribeLoggingOptions
  ( -- * Creating a Request
    DescribeLoggingOptions (..),
    newDescribeLoggingOptions,

    -- * Destructuring the Response
    DescribeLoggingOptionsResponse (..),
    newDescribeLoggingOptionsResponse,

    -- * Response Lenses
    describeLoggingOptionsResponse_loggingOptions,
    describeLoggingOptionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeLoggingOptions' smart constructor.
data DescribeLoggingOptions = DescribeLoggingOptions'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoggingOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeLoggingOptions ::
  DescribeLoggingOptions
newDescribeLoggingOptions = DescribeLoggingOptions'

instance Core.AWSRequest DescribeLoggingOptions where
  type
    AWSResponse DescribeLoggingOptions =
      DescribeLoggingOptionsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoggingOptionsResponse'
            Core.<$> (x Core..?> "loggingOptions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeLoggingOptions

instance Core.NFData DescribeLoggingOptions

instance Core.ToHeaders DescribeLoggingOptions where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DescribeLoggingOptions where
  toPath = Core.const "/logging"

instance Core.ToQuery DescribeLoggingOptions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeLoggingOptionsResponse' smart constructor.
data DescribeLoggingOptionsResponse = DescribeLoggingOptionsResponse'
  { -- | The current settings of the AWS IoT Analytics logging options.
    loggingOptions :: Core.Maybe LoggingOptions,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingOptions', 'describeLoggingOptionsResponse_loggingOptions' - The current settings of the AWS IoT Analytics logging options.
--
-- 'httpStatus', 'describeLoggingOptionsResponse_httpStatus' - The response's http status code.
newDescribeLoggingOptionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeLoggingOptionsResponse
newDescribeLoggingOptionsResponse pHttpStatus_ =
  DescribeLoggingOptionsResponse'
    { loggingOptions =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current settings of the AWS IoT Analytics logging options.
describeLoggingOptionsResponse_loggingOptions :: Lens.Lens' DescribeLoggingOptionsResponse (Core.Maybe LoggingOptions)
describeLoggingOptionsResponse_loggingOptions = Lens.lens (\DescribeLoggingOptionsResponse' {loggingOptions} -> loggingOptions) (\s@DescribeLoggingOptionsResponse' {} a -> s {loggingOptions = a} :: DescribeLoggingOptionsResponse)

-- | The response's http status code.
describeLoggingOptionsResponse_httpStatus :: Lens.Lens' DescribeLoggingOptionsResponse Core.Int
describeLoggingOptionsResponse_httpStatus = Lens.lens (\DescribeLoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeLoggingOptionsResponse' {} a -> s {httpStatus = a} :: DescribeLoggingOptionsResponse)

instance Core.NFData DescribeLoggingOptionsResponse
