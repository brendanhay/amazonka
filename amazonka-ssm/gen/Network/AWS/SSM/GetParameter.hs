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
-- Module      : Network.AWS.SSM.GetParameter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get information about a parameter by using the parameter name. Don\'t
-- confuse this API action with the GetParameters API action.
module Network.AWS.SSM.GetParameter
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
    getParameterResponse_parameter,
    getParameterResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetParameter' smart constructor.
data GetParameter = GetParameter'
  { -- | Return decrypted values for secure string parameters. This flag is
    -- ignored for String and StringList parameter types.
    withDecryption :: Core.Maybe Core.Bool,
    -- | The name of the parameter you want to query.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetParameter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'withDecryption', 'getParameter_withDecryption' - Return decrypted values for secure string parameters. This flag is
-- ignored for String and StringList parameter types.
--
-- 'name', 'getParameter_name' - The name of the parameter you want to query.
newGetParameter ::
  -- | 'name'
  Core.Text ->
  GetParameter
newGetParameter pName_ =
  GetParameter'
    { withDecryption = Core.Nothing,
      name = pName_
    }

-- | Return decrypted values for secure string parameters. This flag is
-- ignored for String and StringList parameter types.
getParameter_withDecryption :: Lens.Lens' GetParameter (Core.Maybe Core.Bool)
getParameter_withDecryption = Lens.lens (\GetParameter' {withDecryption} -> withDecryption) (\s@GetParameter' {} a -> s {withDecryption = a} :: GetParameter)

-- | The name of the parameter you want to query.
getParameter_name :: Lens.Lens' GetParameter Core.Text
getParameter_name = Lens.lens (\GetParameter' {name} -> name) (\s@GetParameter' {} a -> s {name = a} :: GetParameter)

instance Core.AWSRequest GetParameter where
  type AWSResponse GetParameter = GetParameterResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParameterResponse'
            Core.<$> (x Core..?> "Parameter")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetParameter

instance Core.NFData GetParameter

instance Core.ToHeaders GetParameter where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonSSM.GetParameter" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetParameter where
  toJSON GetParameter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("WithDecryption" Core..=) Core.<$> withDecryption,
            Core.Just ("Name" Core..= name)
          ]
      )

instance Core.ToPath GetParameter where
  toPath = Core.const "/"

instance Core.ToQuery GetParameter where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetParameterResponse' smart constructor.
data GetParameterResponse = GetParameterResponse'
  { -- | Information about a parameter.
    parameter :: Core.Maybe Parameter,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetParameterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameter', 'getParameterResponse_parameter' - Information about a parameter.
--
-- 'httpStatus', 'getParameterResponse_httpStatus' - The response's http status code.
newGetParameterResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetParameterResponse
newGetParameterResponse pHttpStatus_ =
  GetParameterResponse'
    { parameter = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about a parameter.
getParameterResponse_parameter :: Lens.Lens' GetParameterResponse (Core.Maybe Parameter)
getParameterResponse_parameter = Lens.lens (\GetParameterResponse' {parameter} -> parameter) (\s@GetParameterResponse' {} a -> s {parameter = a} :: GetParameterResponse)

-- | The response's http status code.
getParameterResponse_httpStatus :: Lens.Lens' GetParameterResponse Core.Int
getParameterResponse_httpStatus = Lens.lens (\GetParameterResponse' {httpStatus} -> httpStatus) (\s@GetParameterResponse' {} a -> s {httpStatus = a} :: GetParameterResponse)

instance Core.NFData GetParameterResponse
