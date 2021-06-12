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
-- Module      : Network.AWS.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
module Network.AWS.DeviceFarm.GetSuite
  ( -- * Creating a Request
    GetSuite (..),
    newGetSuite,

    -- * Request Lenses
    getSuite_arn,

    -- * Destructuring the Response
    GetSuiteResponse (..),
    newGetSuiteResponse,

    -- * Response Lenses
    getSuiteResponse_suite,
    getSuiteResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get suite operation.
--
-- /See:/ 'newGetSuite' smart constructor.
data GetSuite = GetSuite'
  { -- | The suite\'s ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSuite' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getSuite_arn' - The suite\'s ARN.
newGetSuite ::
  -- | 'arn'
  Core.Text ->
  GetSuite
newGetSuite pArn_ = GetSuite' {arn = pArn_}

-- | The suite\'s ARN.
getSuite_arn :: Lens.Lens' GetSuite Core.Text
getSuite_arn = Lens.lens (\GetSuite' {arn} -> arn) (\s@GetSuite' {} a -> s {arn = a} :: GetSuite)

instance Core.AWSRequest GetSuite where
  type AWSResponse GetSuite = GetSuiteResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSuiteResponse'
            Core.<$> (x Core..?> "suite")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetSuite

instance Core.NFData GetSuite

instance Core.ToHeaders GetSuite where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("DeviceFarm_20150623.GetSuite" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetSuite where
  toJSON GetSuite' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("arn" Core..= arn)])

instance Core.ToPath GetSuite where
  toPath = Core.const "/"

instance Core.ToQuery GetSuite where
  toQuery = Core.const Core.mempty

-- | Represents the result of a get suite request.
--
-- /See:/ 'newGetSuiteResponse' smart constructor.
data GetSuiteResponse = GetSuiteResponse'
  { -- | A collection of one or more tests.
    suite :: Core.Maybe Suite,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetSuiteResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'suite', 'getSuiteResponse_suite' - A collection of one or more tests.
--
-- 'httpStatus', 'getSuiteResponse_httpStatus' - The response's http status code.
newGetSuiteResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetSuiteResponse
newGetSuiteResponse pHttpStatus_ =
  GetSuiteResponse'
    { suite = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of one or more tests.
getSuiteResponse_suite :: Lens.Lens' GetSuiteResponse (Core.Maybe Suite)
getSuiteResponse_suite = Lens.lens (\GetSuiteResponse' {suite} -> suite) (\s@GetSuiteResponse' {} a -> s {suite = a} :: GetSuiteResponse)

-- | The response's http status code.
getSuiteResponse_httpStatus :: Lens.Lens' GetSuiteResponse Core.Int
getSuiteResponse_httpStatus = Lens.lens (\GetSuiteResponse' {httpStatus} -> httpStatus) (\s@GetSuiteResponse' {} a -> s {httpStatus = a} :: GetSuiteResponse)

instance Core.NFData GetSuiteResponse
