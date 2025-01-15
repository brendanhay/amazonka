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
-- Module      : Amazonka.IoTSiteWise.DescribeLoggingOptions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current IoT SiteWise logging options.
module Amazonka.IoTSiteWise.DescribeLoggingOptions
  ( -- * Creating a Request
    DescribeLoggingOptions (..),
    newDescribeLoggingOptions,

    -- * Destructuring the Response
    DescribeLoggingOptionsResponse (..),
    newDescribeLoggingOptionsResponse,

    -- * Response Lenses
    describeLoggingOptionsResponse_httpStatus,
    describeLoggingOptionsResponse_loggingOptions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeLoggingOptions' smart constructor.
data DescribeLoggingOptions = DescribeLoggingOptions'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLoggingOptionsResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "loggingOptions")
      )

instance Prelude.Hashable DescribeLoggingOptions where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeLoggingOptions where
  rnf _ = ()

instance Data.ToHeaders DescribeLoggingOptions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeLoggingOptions where
  toPath = Prelude.const "/logging"

instance Data.ToQuery DescribeLoggingOptions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeLoggingOptionsResponse' smart constructor.
data DescribeLoggingOptionsResponse = DescribeLoggingOptionsResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The current logging options.
    loggingOptions :: LoggingOptions
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLoggingOptionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeLoggingOptionsResponse_httpStatus' - The response's http status code.
--
-- 'loggingOptions', 'describeLoggingOptionsResponse_loggingOptions' - The current logging options.
newDescribeLoggingOptionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'loggingOptions'
  LoggingOptions ->
  DescribeLoggingOptionsResponse
newDescribeLoggingOptionsResponse
  pHttpStatus_
  pLoggingOptions_ =
    DescribeLoggingOptionsResponse'
      { httpStatus =
          pHttpStatus_,
        loggingOptions = pLoggingOptions_
      }

-- | The response's http status code.
describeLoggingOptionsResponse_httpStatus :: Lens.Lens' DescribeLoggingOptionsResponse Prelude.Int
describeLoggingOptionsResponse_httpStatus = Lens.lens (\DescribeLoggingOptionsResponse' {httpStatus} -> httpStatus) (\s@DescribeLoggingOptionsResponse' {} a -> s {httpStatus = a} :: DescribeLoggingOptionsResponse)

-- | The current logging options.
describeLoggingOptionsResponse_loggingOptions :: Lens.Lens' DescribeLoggingOptionsResponse LoggingOptions
describeLoggingOptionsResponse_loggingOptions = Lens.lens (\DescribeLoggingOptionsResponse' {loggingOptions} -> loggingOptions) (\s@DescribeLoggingOptionsResponse' {} a -> s {loggingOptions = a} :: DescribeLoggingOptionsResponse)

instance
  Prelude.NFData
    DescribeLoggingOptionsResponse
  where
  rnf DescribeLoggingOptionsResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf loggingOptions
