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
-- Module      : Amazonka.AppRunner.DescribeService
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a full description of an App Runner service.
module Amazonka.AppRunner.DescribeService
  ( -- * Creating a Request
    DescribeService (..),
    newDescribeService,

    -- * Request Lenses
    describeService_serviceArn,

    -- * Destructuring the Response
    DescribeServiceResponse (..),
    newDescribeServiceResponse,

    -- * Response Lenses
    describeServiceResponse_httpStatus,
    describeServiceResponse_service,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeService' smart constructor.
data DescribeService = DescribeService'
  { -- | The Amazon Resource Name (ARN) of the App Runner service that you want a
    -- description for.
    serviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeService' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceArn', 'describeService_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that you want a
-- description for.
newDescribeService ::
  -- | 'serviceArn'
  Prelude.Text ->
  DescribeService
newDescribeService pServiceArn_ =
  DescribeService' {serviceArn = pServiceArn_}

-- | The Amazon Resource Name (ARN) of the App Runner service that you want a
-- description for.
describeService_serviceArn :: Lens.Lens' DescribeService Prelude.Text
describeService_serviceArn = Lens.lens (\DescribeService' {serviceArn} -> serviceArn) (\s@DescribeService' {} a -> s {serviceArn = a} :: DescribeService)

instance Core.AWSRequest DescribeService where
  type
    AWSResponse DescribeService =
      DescribeServiceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Service")
      )

instance Prelude.Hashable DescribeService where
  hashWithSalt _salt DescribeService' {..} =
    _salt `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData DescribeService where
  rnf DescribeService' {..} = Prelude.rnf serviceArn

instance Core.ToHeaders DescribeService where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AppRunner.DescribeService" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeService where
  toJSON DescribeService' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("ServiceArn" Core..= serviceArn)]
      )

instance Core.ToPath DescribeService where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeService where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceResponse' smart constructor.
data DescribeServiceResponse = DescribeServiceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A full description of the App Runner service that you specified in this
    -- request.
    service :: Service
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'describeServiceResponse_httpStatus' - The response's http status code.
--
-- 'service', 'describeServiceResponse_service' - A full description of the App Runner service that you specified in this
-- request.
newDescribeServiceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'service'
  Service ->
  DescribeServiceResponse
newDescribeServiceResponse pHttpStatus_ pService_ =
  DescribeServiceResponse'
    { httpStatus = pHttpStatus_,
      service = pService_
    }

-- | The response's http status code.
describeServiceResponse_httpStatus :: Lens.Lens' DescribeServiceResponse Prelude.Int
describeServiceResponse_httpStatus = Lens.lens (\DescribeServiceResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceResponse' {} a -> s {httpStatus = a} :: DescribeServiceResponse)

-- | A full description of the App Runner service that you specified in this
-- request.
describeServiceResponse_service :: Lens.Lens' DescribeServiceResponse Service
describeServiceResponse_service = Lens.lens (\DescribeServiceResponse' {service} -> service) (\s@DescribeServiceResponse' {} a -> s {service = a} :: DescribeServiceResponse)

instance Prelude.NFData DescribeServiceResponse where
  rnf DescribeServiceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf service
