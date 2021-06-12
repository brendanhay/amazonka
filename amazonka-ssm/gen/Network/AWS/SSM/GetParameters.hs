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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetParameters' smart constructor.
data GetParameters = GetParameters'
  { -- | Return decrypted secure string value. Return decrypted values for secure
    -- string parameters. This flag is ignored for String and StringList
    -- parameter types.
    withDecryption :: Core.Maybe Core.Bool,
    -- | Names of the parameters for which you want to query information.
    names :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Core.Text ->
  GetParameters
newGetParameters pNames_ =
  GetParameters'
    { withDecryption = Core.Nothing,
      names = Lens._Coerce Lens.# pNames_
    }

-- | Return decrypted secure string value. Return decrypted values for secure
-- string parameters. This flag is ignored for String and StringList
-- parameter types.
getParameters_withDecryption :: Lens.Lens' GetParameters (Core.Maybe Core.Bool)
getParameters_withDecryption = Lens.lens (\GetParameters' {withDecryption} -> withDecryption) (\s@GetParameters' {} a -> s {withDecryption = a} :: GetParameters)

-- | Names of the parameters for which you want to query information.
getParameters_names :: Lens.Lens' GetParameters (Core.NonEmpty Core.Text)
getParameters_names = Lens.lens (\GetParameters' {names} -> names) (\s@GetParameters' {} a -> s {names = a} :: GetParameters) Core.. Lens._Coerce

instance Core.AWSRequest GetParameters where
  type
    AWSResponse GetParameters =
      GetParametersResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParametersResponse'
            Core.<$> (x Core..?> "InvalidParameters" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "Parameters" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetParameters

instance Core.NFData GetParameters

instance Core.ToHeaders GetParameters where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetParameters" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetParameters where
  toJSON GetParameters' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WithDecryption" Core..=) Core.<$> withDecryption,
            Core.Just ("Names" Core..= names)
          ]
      )

instance Core.ToPath GetParameters where
  toPath = Core.const "/"

instance Core.ToQuery GetParameters where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetParametersResponse' smart constructor.
data GetParametersResponse = GetParametersResponse'
  { -- | A list of parameters that are not formatted correctly or do not run
    -- during an execution.
    invalidParameters :: Core.Maybe [Core.Text],
    -- | A list of details for a parameter.
    parameters :: Core.Maybe [Parameter],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetParametersResponse
newGetParametersResponse pHttpStatus_ =
  GetParametersResponse'
    { invalidParameters =
        Core.Nothing,
      parameters = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of parameters that are not formatted correctly or do not run
-- during an execution.
getParametersResponse_invalidParameters :: Lens.Lens' GetParametersResponse (Core.Maybe [Core.Text])
getParametersResponse_invalidParameters = Lens.lens (\GetParametersResponse' {invalidParameters} -> invalidParameters) (\s@GetParametersResponse' {} a -> s {invalidParameters = a} :: GetParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | A list of details for a parameter.
getParametersResponse_parameters :: Lens.Lens' GetParametersResponse (Core.Maybe [Parameter])
getParametersResponse_parameters = Lens.lens (\GetParametersResponse' {parameters} -> parameters) (\s@GetParametersResponse' {} a -> s {parameters = a} :: GetParametersResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getParametersResponse_httpStatus :: Lens.Lens' GetParametersResponse Core.Int
getParametersResponse_httpStatus = Lens.lens (\GetParametersResponse' {httpStatus} -> httpStatus) (\s@GetParametersResponse' {} a -> s {httpStatus = a} :: GetParametersResponse)

instance Core.NFData GetParametersResponse
