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
-- Module      : Amazonka.SSM.GetParameters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about one or more parameters by specifying multiple
-- parameter names.
--
-- To get information about a single parameter, you can use the
-- GetParameter operation instead.
module Amazonka.SSM.GetParameters
  ( -- * Creating a Request
    GetParameters (..),
    newGetParameters,

    -- * Request Lenses
    getParameters_withDecryption,
    getParameters_names,

    -- * Destructuring the Response
    GetParametersResponse (..),
    newGetParametersResponse,

    -- * Response Lenses
    getParametersResponse_httpStatus,
    getParametersResponse_invalidParameters,
    getParametersResponse_parameters,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetParameters' smart constructor.
data GetParameters = GetParameters'
  { -- | Return decrypted secure string value. Return decrypted values for secure
    -- string parameters. This flag is ignored for @String@ and @StringList@
    -- parameter types.
    withDecryption :: Prelude.Maybe Prelude.Bool,
    -- | Names of the parameters for which you want to query information.
    --
    -- To query by parameter label, use @\"Name\": \"name:label\"@. To query by
    -- parameter version, use @\"Name\": \"name:version\"@.
    names :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParameters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'withDecryption', 'getParameters_withDecryption' - Return decrypted secure string value. Return decrypted values for secure
-- string parameters. This flag is ignored for @String@ and @StringList@
-- parameter types.
--
-- 'names', 'getParameters_names' - Names of the parameters for which you want to query information.
--
-- To query by parameter label, use @\"Name\": \"name:label\"@. To query by
-- parameter version, use @\"Name\": \"name:version\"@.
newGetParameters ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  GetParameters
newGetParameters pNames_ =
  GetParameters'
    { withDecryption = Prelude.Nothing,
      names = Lens.coerced Lens.# pNames_
    }

-- | Return decrypted secure string value. Return decrypted values for secure
-- string parameters. This flag is ignored for @String@ and @StringList@
-- parameter types.
getParameters_withDecryption :: Lens.Lens' GetParameters (Prelude.Maybe Prelude.Bool)
getParameters_withDecryption = Lens.lens (\GetParameters' {withDecryption} -> withDecryption) (\s@GetParameters' {} a -> s {withDecryption = a} :: GetParameters)

-- | Names of the parameters for which you want to query information.
--
-- To query by parameter label, use @\"Name\": \"name:label\"@. To query by
-- parameter version, use @\"Name\": \"name:version\"@.
getParameters_names :: Lens.Lens' GetParameters (Prelude.NonEmpty Prelude.Text)
getParameters_names = Lens.lens (\GetParameters' {names} -> names) (\s@GetParameters' {} a -> s {names = a} :: GetParameters) Prelude.. Lens.coerced

instance Core.AWSRequest GetParameters where
  type
    AWSResponse GetParameters =
      GetParametersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "InvalidParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "Parameters" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable GetParameters where
  hashWithSalt _salt GetParameters' {..} =
    _salt `Prelude.hashWithSalt` withDecryption
      `Prelude.hashWithSalt` names

instance Prelude.NFData GetParameters where
  rnf GetParameters' {..} =
    Prelude.rnf withDecryption
      `Prelude.seq` Prelude.rnf names

instance Data.ToHeaders GetParameters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AmazonSSM.GetParameters" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetParameters where
  toJSON GetParameters' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("WithDecryption" Data..=)
              Prelude.<$> withDecryption,
            Prelude.Just ("Names" Data..= names)
          ]
      )

instance Data.ToPath GetParameters where
  toPath = Prelude.const "/"

instance Data.ToQuery GetParameters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParametersResponse' smart constructor.
data GetParametersResponse = GetParametersResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of parameters that aren\'t formatted correctly or don\'t run
    -- during an execution.
    invalidParameters :: [Prelude.Text],
    -- | A list of details for a parameter.
    parameters :: [Parameter]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getParametersResponse_httpStatus' - The response's http status code.
--
-- 'invalidParameters', 'getParametersResponse_invalidParameters' - A list of parameters that aren\'t formatted correctly or don\'t run
-- during an execution.
--
-- 'parameters', 'getParametersResponse_parameters' - A list of details for a parameter.
newGetParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParametersResponse
newGetParametersResponse pHttpStatus_ =
  GetParametersResponse'
    { httpStatus = pHttpStatus_,
      invalidParameters = Prelude.mempty,
      parameters = Prelude.mempty
    }

-- | The response's http status code.
getParametersResponse_httpStatus :: Lens.Lens' GetParametersResponse Prelude.Int
getParametersResponse_httpStatus = Lens.lens (\GetParametersResponse' {httpStatus} -> httpStatus) (\s@GetParametersResponse' {} a -> s {httpStatus = a} :: GetParametersResponse)

-- | A list of parameters that aren\'t formatted correctly or don\'t run
-- during an execution.
getParametersResponse_invalidParameters :: Lens.Lens' GetParametersResponse [Prelude.Text]
getParametersResponse_invalidParameters = Lens.lens (\GetParametersResponse' {invalidParameters} -> invalidParameters) (\s@GetParametersResponse' {} a -> s {invalidParameters = a} :: GetParametersResponse) Prelude.. Lens.coerced

-- | A list of details for a parameter.
getParametersResponse_parameters :: Lens.Lens' GetParametersResponse [Parameter]
getParametersResponse_parameters = Lens.lens (\GetParametersResponse' {parameters} -> parameters) (\s@GetParametersResponse' {} a -> s {parameters = a} :: GetParametersResponse) Prelude.. Lens.coerced

instance Prelude.NFData GetParametersResponse where
  rnf GetParametersResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf invalidParameters
      `Prelude.seq` Prelude.rnf parameters
