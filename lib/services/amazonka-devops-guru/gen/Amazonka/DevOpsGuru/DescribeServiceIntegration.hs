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
-- Module      : Amazonka.DevOpsGuru.DescribeServiceIntegration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the integration status of services that are integrated with
-- DevOps Guru. The one service that can be integrated with DevOps Guru is
-- Amazon Web Services Systems Manager, which can be used to create an
-- OpsItem for each generated insight.
module Amazonka.DevOpsGuru.DescribeServiceIntegration
  ( -- * Creating a Request
    DescribeServiceIntegration (..),
    newDescribeServiceIntegration,

    -- * Destructuring the Response
    DescribeServiceIntegrationResponse (..),
    newDescribeServiceIntegrationResponse,

    -- * Response Lenses
    describeServiceIntegrationResponse_serviceIntegration,
    describeServiceIntegrationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServiceIntegration' smart constructor.
data DescribeServiceIntegration = DescribeServiceIntegration'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeServiceIntegration ::
  DescribeServiceIntegration
newDescribeServiceIntegration =
  DescribeServiceIntegration'

instance Core.AWSRequest DescribeServiceIntegration where
  type
    AWSResponse DescribeServiceIntegration =
      DescribeServiceIntegrationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceIntegrationResponse'
            Prelude.<$> (x Data..?> "ServiceIntegration")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceIntegration where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeServiceIntegration where
  rnf _ = ()

instance Data.ToHeaders DescribeServiceIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeServiceIntegration where
  toPath = Prelude.const "/service-integrations"

instance Data.ToQuery DescribeServiceIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeServiceIntegrationResponse' smart constructor.
data DescribeServiceIntegrationResponse = DescribeServiceIntegrationResponse'
  { serviceIntegration :: Prelude.Maybe ServiceIntegrationConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeServiceIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceIntegration', 'describeServiceIntegrationResponse_serviceIntegration' - Undocumented member.
--
-- 'httpStatus', 'describeServiceIntegrationResponse_httpStatus' - The response's http status code.
newDescribeServiceIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeServiceIntegrationResponse
newDescribeServiceIntegrationResponse pHttpStatus_ =
  DescribeServiceIntegrationResponse'
    { serviceIntegration =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
describeServiceIntegrationResponse_serviceIntegration :: Lens.Lens' DescribeServiceIntegrationResponse (Prelude.Maybe ServiceIntegrationConfig)
describeServiceIntegrationResponse_serviceIntegration = Lens.lens (\DescribeServiceIntegrationResponse' {serviceIntegration} -> serviceIntegration) (\s@DescribeServiceIntegrationResponse' {} a -> s {serviceIntegration = a} :: DescribeServiceIntegrationResponse)

-- | The response's http status code.
describeServiceIntegrationResponse_httpStatus :: Lens.Lens' DescribeServiceIntegrationResponse Prelude.Int
describeServiceIntegrationResponse_httpStatus = Lens.lens (\DescribeServiceIntegrationResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceIntegrationResponse' {} a -> s {httpStatus = a} :: DescribeServiceIntegrationResponse)

instance
  Prelude.NFData
    DescribeServiceIntegrationResponse
  where
  rnf DescribeServiceIntegrationResponse' {..} =
    Prelude.rnf serviceIntegration `Prelude.seq`
      Prelude.rnf httpStatus
