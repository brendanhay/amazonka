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
-- Module      : Amazonka.DeviceFarm.GetSuite
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a suite.
module Amazonka.DeviceFarm.GetSuite
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents a request to the get suite operation.
--
-- /See:/ 'newGetSuite' smart constructor.
data GetSuite = GetSuite'
  { -- | The suite\'s ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  GetSuite
newGetSuite pArn_ = GetSuite' {arn = pArn_}

-- | The suite\'s ARN.
getSuite_arn :: Lens.Lens' GetSuite Prelude.Text
getSuite_arn = Lens.lens (\GetSuite' {arn} -> arn) (\s@GetSuite' {} a -> s {arn = a} :: GetSuite)

instance Core.AWSRequest GetSuite where
  type AWSResponse GetSuite = GetSuiteResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSuiteResponse'
            Prelude.<$> (x Data..?> "suite")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSuite where
  hashWithSalt _salt GetSuite' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetSuite where
  rnf GetSuite' {..} = Prelude.rnf arn

instance Data.ToHeaders GetSuite where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.GetSuite" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSuite where
  toJSON GetSuite' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetSuite where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSuite where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a get suite request.
--
-- /See:/ 'newGetSuiteResponse' smart constructor.
data GetSuiteResponse = GetSuiteResponse'
  { -- | A collection of one or more tests.
    suite :: Prelude.Maybe Suite,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetSuiteResponse
newGetSuiteResponse pHttpStatus_ =
  GetSuiteResponse'
    { suite = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of one or more tests.
getSuiteResponse_suite :: Lens.Lens' GetSuiteResponse (Prelude.Maybe Suite)
getSuiteResponse_suite = Lens.lens (\GetSuiteResponse' {suite} -> suite) (\s@GetSuiteResponse' {} a -> s {suite = a} :: GetSuiteResponse)

-- | The response's http status code.
getSuiteResponse_httpStatus :: Lens.Lens' GetSuiteResponse Prelude.Int
getSuiteResponse_httpStatus = Lens.lens (\GetSuiteResponse' {httpStatus} -> httpStatus) (\s@GetSuiteResponse' {} a -> s {httpStatus = a} :: GetSuiteResponse)

instance Prelude.NFData GetSuiteResponse where
  rnf GetSuiteResponse' {..} =
    Prelude.rnf suite
      `Prelude.seq` Prelude.rnf httpStatus
