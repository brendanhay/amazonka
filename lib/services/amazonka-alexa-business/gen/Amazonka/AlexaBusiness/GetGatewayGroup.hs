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
-- Module      : Amazonka.AlexaBusiness.GetGatewayGroup
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a gateway group.
module Amazonka.AlexaBusiness.GetGatewayGroup
  ( -- * Creating a Request
    GetGatewayGroup (..),
    newGetGatewayGroup,

    -- * Request Lenses
    getGatewayGroup_gatewayGroupArn,

    -- * Destructuring the Response
    GetGatewayGroupResponse (..),
    newGetGatewayGroupResponse,

    -- * Response Lenses
    getGatewayGroupResponse_gatewayGroup,
    getGatewayGroupResponse_httpStatus,
  )
where

import Amazonka.AlexaBusiness.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGatewayGroup' smart constructor.
data GetGatewayGroup = GetGatewayGroup'
  { -- | The ARN of the gateway group to get.
    gatewayGroupArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGatewayGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroupArn', 'getGatewayGroup_gatewayGroupArn' - The ARN of the gateway group to get.
newGetGatewayGroup ::
  -- | 'gatewayGroupArn'
  Prelude.Text ->
  GetGatewayGroup
newGetGatewayGroup pGatewayGroupArn_ =
  GetGatewayGroup'
    { gatewayGroupArn =
        pGatewayGroupArn_
    }

-- | The ARN of the gateway group to get.
getGatewayGroup_gatewayGroupArn :: Lens.Lens' GetGatewayGroup Prelude.Text
getGatewayGroup_gatewayGroupArn = Lens.lens (\GetGatewayGroup' {gatewayGroupArn} -> gatewayGroupArn) (\s@GetGatewayGroup' {} a -> s {gatewayGroupArn = a} :: GetGatewayGroup)

instance Core.AWSRequest GetGatewayGroup where
  type
    AWSResponse GetGatewayGroup =
      GetGatewayGroupResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGatewayGroupResponse'
            Prelude.<$> (x Core..?> "GatewayGroup")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetGatewayGroup where
  hashWithSalt _salt GetGatewayGroup' {..} =
    _salt `Prelude.hashWithSalt` gatewayGroupArn

instance Prelude.NFData GetGatewayGroup where
  rnf GetGatewayGroup' {..} =
    Prelude.rnf gatewayGroupArn

instance Core.ToHeaders GetGatewayGroup where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AlexaForBusiness.GetGatewayGroup" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetGatewayGroup where
  toJSON GetGatewayGroup' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("GatewayGroupArn" Core..= gatewayGroupArn)
          ]
      )

instance Core.ToPath GetGatewayGroup where
  toPath = Prelude.const "/"

instance Core.ToQuery GetGatewayGroup where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGatewayGroupResponse' smart constructor.
data GetGatewayGroupResponse = GetGatewayGroupResponse'
  { gatewayGroup :: Prelude.Maybe GatewayGroup,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGatewayGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayGroup', 'getGatewayGroupResponse_gatewayGroup' - Undocumented member.
--
-- 'httpStatus', 'getGatewayGroupResponse_httpStatus' - The response's http status code.
newGetGatewayGroupResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetGatewayGroupResponse
newGetGatewayGroupResponse pHttpStatus_ =
  GetGatewayGroupResponse'
    { gatewayGroup =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
getGatewayGroupResponse_gatewayGroup :: Lens.Lens' GetGatewayGroupResponse (Prelude.Maybe GatewayGroup)
getGatewayGroupResponse_gatewayGroup = Lens.lens (\GetGatewayGroupResponse' {gatewayGroup} -> gatewayGroup) (\s@GetGatewayGroupResponse' {} a -> s {gatewayGroup = a} :: GetGatewayGroupResponse)

-- | The response's http status code.
getGatewayGroupResponse_httpStatus :: Lens.Lens' GetGatewayGroupResponse Prelude.Int
getGatewayGroupResponse_httpStatus = Lens.lens (\GetGatewayGroupResponse' {httpStatus} -> httpStatus) (\s@GetGatewayGroupResponse' {} a -> s {httpStatus = a} :: GetGatewayGroupResponse)

instance Prelude.NFData GetGatewayGroupResponse where
  rnf GetGatewayGroupResponse' {..} =
    Prelude.rnf gatewayGroup
      `Prelude.seq` Prelude.rnf httpStatus
