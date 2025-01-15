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
-- Module      : Amazonka.MwAA.GetEnvironment
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes an Amazon Managed Workflows for Apache Airflow (MWAA)
-- environment.
module Amazonka.MwAA.GetEnvironment
  ( -- * Creating a Request
    GetEnvironment (..),
    newGetEnvironment,

    -- * Request Lenses
    getEnvironment_name,

    -- * Destructuring the Response
    GetEnvironmentResponse (..),
    newGetEnvironmentResponse,

    -- * Response Lenses
    getEnvironmentResponse_environment,
    getEnvironmentResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MwAA.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnvironment' smart constructor.
data GetEnvironment = GetEnvironment'
  { -- | The name of the Amazon MWAA environment. For example,
    -- @MyMWAAEnvironment@.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getEnvironment_name' - The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
newGetEnvironment ::
  -- | 'name'
  Prelude.Text ->
  GetEnvironment
newGetEnvironment pName_ =
  GetEnvironment' {name = pName_}

-- | The name of the Amazon MWAA environment. For example,
-- @MyMWAAEnvironment@.
getEnvironment_name :: Lens.Lens' GetEnvironment Prelude.Text
getEnvironment_name = Lens.lens (\GetEnvironment' {name} -> name) (\s@GetEnvironment' {} a -> s {name = a} :: GetEnvironment)

instance Core.AWSRequest GetEnvironment where
  type
    AWSResponse GetEnvironment =
      GetEnvironmentResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnvironmentResponse'
            Prelude.<$> (x Data..?> "Environment")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEnvironment where
  hashWithSalt _salt GetEnvironment' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetEnvironment where
  rnf GetEnvironment' {..} = Prelude.rnf name

instance Data.ToHeaders GetEnvironment where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetEnvironment where
  toPath GetEnvironment' {..} =
    Prelude.mconcat ["/environments/", Data.toBS name]

instance Data.ToQuery GetEnvironment where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnvironmentResponse' smart constructor.
data GetEnvironmentResponse = GetEnvironmentResponse'
  { -- | An object containing all available details about the environment.
    environment :: Prelude.Maybe Environment,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environment', 'getEnvironmentResponse_environment' - An object containing all available details about the environment.
--
-- 'httpStatus', 'getEnvironmentResponse_httpStatus' - The response's http status code.
newGetEnvironmentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEnvironmentResponse
newGetEnvironmentResponse pHttpStatus_ =
  GetEnvironmentResponse'
    { environment =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object containing all available details about the environment.
getEnvironmentResponse_environment :: Lens.Lens' GetEnvironmentResponse (Prelude.Maybe Environment)
getEnvironmentResponse_environment = Lens.lens (\GetEnvironmentResponse' {environment} -> environment) (\s@GetEnvironmentResponse' {} a -> s {environment = a} :: GetEnvironmentResponse)

-- | The response's http status code.
getEnvironmentResponse_httpStatus :: Lens.Lens' GetEnvironmentResponse Prelude.Int
getEnvironmentResponse_httpStatus = Lens.lens (\GetEnvironmentResponse' {httpStatus} -> httpStatus) (\s@GetEnvironmentResponse' {} a -> s {httpStatus = a} :: GetEnvironmentResponse)

instance Prelude.NFData GetEnvironmentResponse where
  rnf GetEnvironmentResponse' {..} =
    Prelude.rnf environment `Prelude.seq`
      Prelude.rnf httpStatus
