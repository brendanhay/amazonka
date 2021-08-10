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
-- Module      : Network.AWS.SSM.GetParameters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get details of a parameter. Don\'t confuse this API action with the
-- GetParameter API action.
module Network.AWS.SSM.GetParameters
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
    getParametersResponse_invalidParameters,
    getParametersResponse_parameters,
    getParametersResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetParameters' smart constructor.
data GetParameters = GetParameters'
  { -- | Return decrypted secure string value. Return decrypted values for secure
    -- string parameters. This flag is ignored for String and StringList
    -- parameter types.
    withDecryption :: Prelude.Maybe Prelude.Bool,
    -- | Names of the parameters for which you want to query information.
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
-- string parameters. This flag is ignored for String and StringList
-- parameter types.
--
-- 'names', 'getParameters_names' - Names of the parameters for which you want to query information.
newGetParameters ::
  -- | 'names'
  Prelude.NonEmpty Prelude.Text ->
  GetParameters
newGetParameters pNames_ =
  GetParameters'
    { withDecryption = Prelude.Nothing,
      names = Lens._Coerce Lens.# pNames_
    }

-- | Return decrypted secure string value. Return decrypted values for secure
-- string parameters. This flag is ignored for String and StringList
-- parameter types.
getParameters_withDecryption :: Lens.Lens' GetParameters (Prelude.Maybe Prelude.Bool)
getParameters_withDecryption = Lens.lens (\GetParameters' {withDecryption} -> withDecryption) (\s@GetParameters' {} a -> s {withDecryption = a} :: GetParameters)

-- | Names of the parameters for which you want to query information.
getParameters_names :: Lens.Lens' GetParameters (Prelude.NonEmpty Prelude.Text)
getParameters_names = Lens.lens (\GetParameters' {names} -> names) (\s@GetParameters' {} a -> s {names = a} :: GetParameters) Prelude.. Lens._Coerce

instance Core.AWSRequest GetParameters where
  type
    AWSResponse GetParameters =
      GetParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersResponse'
            Prelude.<$> ( x Core..?> "InvalidParameters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "Parameters" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetParameters

instance Prelude.NFData GetParameters

instance Core.ToHeaders GetParameters where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetParameters" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetParameters where
  toJSON GetParameters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("WithDecryption" Core..=)
              Prelude.<$> withDecryption,
            Prelude.Just ("Names" Core..= names)
          ]
      )

instance Core.ToPath GetParameters where
  toPath = Prelude.const "/"

instance Core.ToQuery GetParameters where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetParametersResponse' smart constructor.
data GetParametersResponse = GetParametersResponse'
  { -- | A list of parameters that are not formatted correctly or do not run
    -- during an execution.
    invalidParameters :: Prelude.Maybe [Prelude.Text],
    -- | A list of details for a parameter.
    parameters :: Prelude.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetParametersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'invalidParameters', 'getParametersResponse_invalidParameters' - A list of parameters that are not formatted correctly or do not run
-- during an execution.
--
-- 'parameters', 'getParametersResponse_parameters' - A list of details for a parameter.
--
-- 'httpStatus', 'getParametersResponse_httpStatus' - The response's http status code.
newGetParametersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetParametersResponse
newGetParametersResponse pHttpStatus_ =
  GetParametersResponse'
    { invalidParameters =
        Prelude.Nothing,
      parameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of parameters that are not formatted correctly or do not run
-- during an execution.
getParametersResponse_invalidParameters :: Lens.Lens' GetParametersResponse (Prelude.Maybe [Prelude.Text])
getParametersResponse_invalidParameters = Lens.lens (\GetParametersResponse' {invalidParameters} -> invalidParameters) (\s@GetParametersResponse' {} a -> s {invalidParameters = a} :: GetParametersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | A list of details for a parameter.
getParametersResponse_parameters :: Lens.Lens' GetParametersResponse (Prelude.Maybe [Parameter])
getParametersResponse_parameters = Lens.lens (\GetParametersResponse' {parameters} -> parameters) (\s@GetParametersResponse' {} a -> s {parameters = a} :: GetParametersResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getParametersResponse_httpStatus :: Lens.Lens' GetParametersResponse Prelude.Int
getParametersResponse_httpStatus = Lens.lens (\GetParametersResponse' {httpStatus} -> httpStatus) (\s@GetParametersResponse' {} a -> s {httpStatus = a} :: GetParametersResponse)

instance Prelude.NFData GetParametersResponse
