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
-- Module      : Amazonka.SSM.GetParameter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a single parameter by specifying the parameter
-- name.
--
-- To get information about more than one parameter at a time, use the
-- GetParameters operation.
module Amazonka.SSM.GetParameter
  ( -- * Creating a Request
    GetParameter (..),
    newGetParameter,

    -- * Request Lenses
    getParameter_withDecryption,
    getParameter_name,

    -- * Destructuring the Response
    GetParameterResponse (..),
    newGetParameterResponse,

    -- * Response Lenses
    getParameterResponse_httpStatus,
    getParameterResponse_parameter,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetParameter' smart constructor.
data GetParameter = GetParameter'
  { -- | Return decrypted values for secure string parameters. This flag is
    -- ignored for @String@ and @StringList@ parameter types.
    withDecryption :: Prelude.Maybe Prelude.Bool,
    -- | The name of the parameter you want to query.
    --
    -- To query by parameter label, use @\"Name\": \"name:label\"@. To query by
    -- parameter version, use @\"Name\": \"name:version\"@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'withDecryption', 'getParameter_withDecryption' - Return decrypted values for secure string parameters. This flag is
-- ignored for @String@ and @StringList@ parameter types.
--
-- 'name', 'getParameter_name' - The name of the parameter you want to query.
--
-- To query by parameter label, use @\"Name\": \"name:label\"@. To query by
-- parameter version, use @\"Name\": \"name:version\"@.
newGetParameter ::
  -- | 'name'
  Prelude.Text ->
  GetParameter
newGetParameter pName_ =
  GetParameter'
    { withDecryption = Prelude.Nothing,
      name = pName_
    }

-- | Return decrypted values for secure string parameters. This flag is
-- ignored for @String@ and @StringList@ parameter types.
getParameter_withDecryption :: Lens.Lens' GetParameter (Prelude.Maybe Prelude.Bool)
getParameter_withDecryption = Lens.lens (\GetParameter' {withDecryption} -> withDecryption) (\s@GetParameter' {} a -> s {withDecryption = a} :: GetParameter)

-- | The name of the parameter you want to query.
--
-- To query by parameter label, use @\"Name\": \"name:label\"@. To query by
-- parameter version, use @\"Name\": \"name:version\"@.
getParameter_name :: Lens.Lens' GetParameter Prelude.Text
getParameter_name = Lens.lens (\GetParameter' {name} -> name) (\s@GetParameter' {} a -> s {name = a} :: GetParameter)

instance Core.AWSRequest GetParameter where
  type AWSResponse GetParameter = GetParameterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Parameter")
      )

instance Prelude.Hashable GetParameter where
  hashWithSalt _salt GetParameter' {..} =
    _salt `Prelude.hashWithSalt` withDecryption
      `Prelude.hashWithSalt` name

instance Prelude.NFData GetParameter where
  rnf GetParameter' {..} =
    Prelude.rnf withDecryption
      `Prelude.seq` Prelude.rnf name

instance Core.ToHeaders GetParameter where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetParameter" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetParameter where
  toJSON GetParameter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WithDecryption" Core..=)
              Prelude.<$> withDecryption,
            Prelude.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetParameter where
  toPath = Prelude.const "/"

instance Core.ToQuery GetParameter where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParameterResponse' smart constructor.
data GetParameterResponse = GetParameterResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Information about a parameter.
    parameter :: Parameter
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getParameterResponse_httpStatus' - The response's http status code.
--
-- 'parameter', 'getParameterResponse_parameter' - Information about a parameter.
newGetParameterResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'parameter'
  Parameter ->
  GetParameterResponse
newGetParameterResponse pHttpStatus_ pParameter_ =
  GetParameterResponse'
    { httpStatus = pHttpStatus_,
      parameter = pParameter_
    }

-- | The response's http status code.
getParameterResponse_httpStatus :: Lens.Lens' GetParameterResponse Prelude.Int
getParameterResponse_httpStatus = Lens.lens (\GetParameterResponse' {httpStatus} -> httpStatus) (\s@GetParameterResponse' {} a -> s {httpStatus = a} :: GetParameterResponse)

-- | Information about a parameter.
getParameterResponse_parameter :: Lens.Lens' GetParameterResponse Parameter
getParameterResponse_parameter = Lens.lens (\GetParameterResponse' {parameter} -> parameter) (\s@GetParameterResponse' {} a -> s {parameter = a} :: GetParameterResponse)

instance Prelude.NFData GetParameterResponse where
  rnf GetParameterResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf parameter
