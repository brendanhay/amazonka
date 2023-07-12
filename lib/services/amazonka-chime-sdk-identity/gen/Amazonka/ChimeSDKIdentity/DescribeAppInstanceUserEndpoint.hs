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
-- Module      : Amazonka.ChimeSDKIdentity.DescribeAppInstanceUserEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the full details of an @AppInstanceUserEndpoint@.
module Amazonka.ChimeSDKIdentity.DescribeAppInstanceUserEndpoint
  ( -- * Creating a Request
    DescribeAppInstanceUserEndpoint (..),
    newDescribeAppInstanceUserEndpoint,

    -- * Request Lenses
    describeAppInstanceUserEndpoint_appInstanceUserArn,
    describeAppInstanceUserEndpoint_endpointId,

    -- * Destructuring the Response
    DescribeAppInstanceUserEndpointResponse (..),
    newDescribeAppInstanceUserEndpointResponse,

    -- * Response Lenses
    describeAppInstanceUserEndpointResponse_appInstanceUserEndpoint,
    describeAppInstanceUserEndpointResponse_httpStatus,
  )
where

import Amazonka.ChimeSDKIdentity.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeAppInstanceUserEndpoint' smart constructor.
data DescribeAppInstanceUserEndpoint = DescribeAppInstanceUserEndpoint'
  { -- | The ARN of the @AppInstanceUser@.
    appInstanceUserArn :: Data.Sensitive Prelude.Text,
    -- | The unique identifier of the @AppInstanceUserEndpoint@.
    endpointId :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceUserEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserArn', 'describeAppInstanceUserEndpoint_appInstanceUserArn' - The ARN of the @AppInstanceUser@.
--
-- 'endpointId', 'describeAppInstanceUserEndpoint_endpointId' - The unique identifier of the @AppInstanceUserEndpoint@.
newDescribeAppInstanceUserEndpoint ::
  -- | 'appInstanceUserArn'
  Prelude.Text ->
  -- | 'endpointId'
  Prelude.Text ->
  DescribeAppInstanceUserEndpoint
newDescribeAppInstanceUserEndpoint
  pAppInstanceUserArn_
  pEndpointId_ =
    DescribeAppInstanceUserEndpoint'
      { appInstanceUserArn =
          Data._Sensitive
            Lens.# pAppInstanceUserArn_,
        endpointId =
          Data._Sensitive Lens.# pEndpointId_
      }

-- | The ARN of the @AppInstanceUser@.
describeAppInstanceUserEndpoint_appInstanceUserArn :: Lens.Lens' DescribeAppInstanceUserEndpoint Prelude.Text
describeAppInstanceUserEndpoint_appInstanceUserArn = Lens.lens (\DescribeAppInstanceUserEndpoint' {appInstanceUserArn} -> appInstanceUserArn) (\s@DescribeAppInstanceUserEndpoint' {} a -> s {appInstanceUserArn = a} :: DescribeAppInstanceUserEndpoint) Prelude.. Data._Sensitive

-- | The unique identifier of the @AppInstanceUserEndpoint@.
describeAppInstanceUserEndpoint_endpointId :: Lens.Lens' DescribeAppInstanceUserEndpoint Prelude.Text
describeAppInstanceUserEndpoint_endpointId = Lens.lens (\DescribeAppInstanceUserEndpoint' {endpointId} -> endpointId) (\s@DescribeAppInstanceUserEndpoint' {} a -> s {endpointId = a} :: DescribeAppInstanceUserEndpoint) Prelude.. Data._Sensitive

instance
  Core.AWSRequest
    DescribeAppInstanceUserEndpoint
  where
  type
    AWSResponse DescribeAppInstanceUserEndpoint =
      DescribeAppInstanceUserEndpointResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeAppInstanceUserEndpointResponse'
            Prelude.<$> (x Data..?> "AppInstanceUserEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeAppInstanceUserEndpoint
  where
  hashWithSalt
    _salt
    DescribeAppInstanceUserEndpoint' {..} =
      _salt
        `Prelude.hashWithSalt` appInstanceUserArn
        `Prelude.hashWithSalt` endpointId

instance
  Prelude.NFData
    DescribeAppInstanceUserEndpoint
  where
  rnf DescribeAppInstanceUserEndpoint' {..} =
    Prelude.rnf appInstanceUserArn
      `Prelude.seq` Prelude.rnf endpointId

instance
  Data.ToHeaders
    DescribeAppInstanceUserEndpoint
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeAppInstanceUserEndpoint where
  toPath DescribeAppInstanceUserEndpoint' {..} =
    Prelude.mconcat
      [ "/app-instance-users/",
        Data.toBS appInstanceUserArn,
        "/endpoints/",
        Data.toBS endpointId
      ]

instance Data.ToQuery DescribeAppInstanceUserEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeAppInstanceUserEndpointResponse' smart constructor.
data DescribeAppInstanceUserEndpointResponse = DescribeAppInstanceUserEndpointResponse'
  { -- | The full details of an @AppInstanceUserEndpoint@: the
    -- @AppInstanceUserArn@, ID, name, type, resource ARN, attributes, allow
    -- messages, state, and created and last updated timestamps. All timestamps
    -- use epoch milliseconds.
    appInstanceUserEndpoint :: Prelude.Maybe AppInstanceUserEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeAppInstanceUserEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'appInstanceUserEndpoint', 'describeAppInstanceUserEndpointResponse_appInstanceUserEndpoint' - The full details of an @AppInstanceUserEndpoint@: the
-- @AppInstanceUserArn@, ID, name, type, resource ARN, attributes, allow
-- messages, state, and created and last updated timestamps. All timestamps
-- use epoch milliseconds.
--
-- 'httpStatus', 'describeAppInstanceUserEndpointResponse_httpStatus' - The response's http status code.
newDescribeAppInstanceUserEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeAppInstanceUserEndpointResponse
newDescribeAppInstanceUserEndpointResponse
  pHttpStatus_ =
    DescribeAppInstanceUserEndpointResponse'
      { appInstanceUserEndpoint =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The full details of an @AppInstanceUserEndpoint@: the
-- @AppInstanceUserArn@, ID, name, type, resource ARN, attributes, allow
-- messages, state, and created and last updated timestamps. All timestamps
-- use epoch milliseconds.
describeAppInstanceUserEndpointResponse_appInstanceUserEndpoint :: Lens.Lens' DescribeAppInstanceUserEndpointResponse (Prelude.Maybe AppInstanceUserEndpoint)
describeAppInstanceUserEndpointResponse_appInstanceUserEndpoint = Lens.lens (\DescribeAppInstanceUserEndpointResponse' {appInstanceUserEndpoint} -> appInstanceUserEndpoint) (\s@DescribeAppInstanceUserEndpointResponse' {} a -> s {appInstanceUserEndpoint = a} :: DescribeAppInstanceUserEndpointResponse)

-- | The response's http status code.
describeAppInstanceUserEndpointResponse_httpStatus :: Lens.Lens' DescribeAppInstanceUserEndpointResponse Prelude.Int
describeAppInstanceUserEndpointResponse_httpStatus = Lens.lens (\DescribeAppInstanceUserEndpointResponse' {httpStatus} -> httpStatus) (\s@DescribeAppInstanceUserEndpointResponse' {} a -> s {httpStatus = a} :: DescribeAppInstanceUserEndpointResponse)

instance
  Prelude.NFData
    DescribeAppInstanceUserEndpointResponse
  where
  rnf DescribeAppInstanceUserEndpointResponse' {..} =
    Prelude.rnf appInstanceUserEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
