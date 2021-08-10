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
-- Module      : Network.AWS.DeviceFarm.GetTest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a test.
module Network.AWS.DeviceFarm.GetTest
  ( -- * Creating a Request
    GetTest (..),
    newGetTest,

    -- * Request Lenses
    getTest_arn,

    -- * Destructuring the Response
    GetTestResponse (..),
    newGetTestResponse,

    -- * Response Lenses
    getTestResponse_test,
    getTestResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents a request to the get test operation.
--
-- /See:/ 'newGetTest' smart constructor.
data GetTest = GetTest'
  { -- | The test\'s ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getTest_arn' - The test\'s ARN.
newGetTest ::
  -- | 'arn'
  Prelude.Text ->
  GetTest
newGetTest pArn_ = GetTest' {arn = pArn_}

-- | The test\'s ARN.
getTest_arn :: Lens.Lens' GetTest Prelude.Text
getTest_arn = Lens.lens (\GetTest' {arn} -> arn) (\s@GetTest' {} a -> s {arn = a} :: GetTest)

instance Core.AWSRequest GetTest where
  type AWSResponse GetTest = GetTestResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTestResponse'
            Prelude.<$> (x Core..?> "test")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTest

instance Prelude.NFData GetTest

instance Core.ToHeaders GetTest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.GetTest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTest where
  toJSON GetTest' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Core..= arn)]
      )

instance Core.ToPath GetTest where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTest where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a get test request.
--
-- /See:/ 'newGetTestResponse' smart constructor.
data GetTestResponse = GetTestResponse'
  { -- | A test condition that is evaluated.
    test :: Prelude.Maybe Test,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'test', 'getTestResponse_test' - A test condition that is evaluated.
--
-- 'httpStatus', 'getTestResponse_httpStatus' - The response's http status code.
newGetTestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTestResponse
newGetTestResponse pHttpStatus_ =
  GetTestResponse'
    { test = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A test condition that is evaluated.
getTestResponse_test :: Lens.Lens' GetTestResponse (Prelude.Maybe Test)
getTestResponse_test = Lens.lens (\GetTestResponse' {test} -> test) (\s@GetTestResponse' {} a -> s {test = a} :: GetTestResponse)

-- | The response's http status code.
getTestResponse_httpStatus :: Lens.Lens' GetTestResponse Prelude.Int
getTestResponse_httpStatus = Lens.lens (\GetTestResponse' {httpStatus} -> httpStatus) (\s@GetTestResponse' {} a -> s {httpStatus = a} :: GetTestResponse)

instance Prelude.NFData GetTestResponse
