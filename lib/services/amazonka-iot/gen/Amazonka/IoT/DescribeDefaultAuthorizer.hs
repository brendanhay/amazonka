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
-- Module      : Amazonka.IoT.DescribeDefaultAuthorizer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the default authorizer.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions DescribeDefaultAuthorizer>
-- action.
module Amazonka.IoT.DescribeDefaultAuthorizer
  ( -- * Creating a Request
    DescribeDefaultAuthorizer (..),
    newDescribeDefaultAuthorizer,

    -- * Destructuring the Response
    DescribeDefaultAuthorizerResponse (..),
    newDescribeDefaultAuthorizerResponse,

    -- * Response Lenses
    describeDefaultAuthorizerResponse_authorizerDescription,
    describeDefaultAuthorizerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeDefaultAuthorizer' smart constructor.
data DescribeDefaultAuthorizer = DescribeDefaultAuthorizer'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDefaultAuthorizer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeDefaultAuthorizer ::
  DescribeDefaultAuthorizer
newDescribeDefaultAuthorizer =
  DescribeDefaultAuthorizer'

instance Core.AWSRequest DescribeDefaultAuthorizer where
  type
    AWSResponse DescribeDefaultAuthorizer =
      DescribeDefaultAuthorizerResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDefaultAuthorizerResponse'
            Prelude.<$> (x Data..?> "authorizerDescription")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDefaultAuthorizer where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData DescribeDefaultAuthorizer where
  rnf _ = ()

instance Data.ToHeaders DescribeDefaultAuthorizer where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DescribeDefaultAuthorizer where
  toPath = Prelude.const "/default-authorizer"

instance Data.ToQuery DescribeDefaultAuthorizer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeDefaultAuthorizerResponse' smart constructor.
data DescribeDefaultAuthorizerResponse = DescribeDefaultAuthorizerResponse'
  { -- | The default authorizer\'s description.
    authorizerDescription :: Prelude.Maybe AuthorizerDescription,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeDefaultAuthorizerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorizerDescription', 'describeDefaultAuthorizerResponse_authorizerDescription' - The default authorizer\'s description.
--
-- 'httpStatus', 'describeDefaultAuthorizerResponse_httpStatus' - The response's http status code.
newDescribeDefaultAuthorizerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeDefaultAuthorizerResponse
newDescribeDefaultAuthorizerResponse pHttpStatus_ =
  DescribeDefaultAuthorizerResponse'
    { authorizerDescription =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The default authorizer\'s description.
describeDefaultAuthorizerResponse_authorizerDescription :: Lens.Lens' DescribeDefaultAuthorizerResponse (Prelude.Maybe AuthorizerDescription)
describeDefaultAuthorizerResponse_authorizerDescription = Lens.lens (\DescribeDefaultAuthorizerResponse' {authorizerDescription} -> authorizerDescription) (\s@DescribeDefaultAuthorizerResponse' {} a -> s {authorizerDescription = a} :: DescribeDefaultAuthorizerResponse)

-- | The response's http status code.
describeDefaultAuthorizerResponse_httpStatus :: Lens.Lens' DescribeDefaultAuthorizerResponse Prelude.Int
describeDefaultAuthorizerResponse_httpStatus = Lens.lens (\DescribeDefaultAuthorizerResponse' {httpStatus} -> httpStatus) (\s@DescribeDefaultAuthorizerResponse' {} a -> s {httpStatus = a} :: DescribeDefaultAuthorizerResponse)

instance
  Prelude.NFData
    DescribeDefaultAuthorizerResponse
  where
  rnf DescribeDefaultAuthorizerResponse' {..} =
    Prelude.rnf authorizerDescription
      `Prelude.seq` Prelude.rnf httpStatus
