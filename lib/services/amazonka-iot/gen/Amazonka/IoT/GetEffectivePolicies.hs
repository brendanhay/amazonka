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
-- Module      : Amazonka.IoT.GetEffectivePolicies
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the policies that have an effect on the authorization
-- behavior of the specified device when it connects to the IoT device
-- gateway.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetEffectivePolicies>
-- action.
module Amazonka.IoT.GetEffectivePolicies
  ( -- * Creating a Request
    GetEffectivePolicies (..),
    newGetEffectivePolicies,

    -- * Request Lenses
    getEffectivePolicies_principal,
    getEffectivePolicies_thingName,
    getEffectivePolicies_cognitoIdentityPoolId,

    -- * Destructuring the Response
    GetEffectivePoliciesResponse (..),
    newGetEffectivePoliciesResponse,

    -- * Response Lenses
    getEffectivePoliciesResponse_effectivePolicies,
    getEffectivePoliciesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEffectivePolicies' smart constructor.
data GetEffectivePolicies = GetEffectivePolicies'
  { -- | The principal. Valid principals are CertificateArn
    -- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
    -- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
    -- (/region/:/id/).
    principal :: Prelude.Maybe Prelude.Text,
    -- | The thing name.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The Cognito identity pool ID.
    cognitoIdentityPoolId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEffectivePolicies' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'principal', 'getEffectivePolicies_principal' - The principal. Valid principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
--
-- 'thingName', 'getEffectivePolicies_thingName' - The thing name.
--
-- 'cognitoIdentityPoolId', 'getEffectivePolicies_cognitoIdentityPoolId' - The Cognito identity pool ID.
newGetEffectivePolicies ::
  GetEffectivePolicies
newGetEffectivePolicies =
  GetEffectivePolicies'
    { principal = Prelude.Nothing,
      thingName = Prelude.Nothing,
      cognitoIdentityPoolId = Prelude.Nothing
    }

-- | The principal. Valid principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
getEffectivePolicies_principal :: Lens.Lens' GetEffectivePolicies (Prelude.Maybe Prelude.Text)
getEffectivePolicies_principal = Lens.lens (\GetEffectivePolicies' {principal} -> principal) (\s@GetEffectivePolicies' {} a -> s {principal = a} :: GetEffectivePolicies)

-- | The thing name.
getEffectivePolicies_thingName :: Lens.Lens' GetEffectivePolicies (Prelude.Maybe Prelude.Text)
getEffectivePolicies_thingName = Lens.lens (\GetEffectivePolicies' {thingName} -> thingName) (\s@GetEffectivePolicies' {} a -> s {thingName = a} :: GetEffectivePolicies)

-- | The Cognito identity pool ID.
getEffectivePolicies_cognitoIdentityPoolId :: Lens.Lens' GetEffectivePolicies (Prelude.Maybe Prelude.Text)
getEffectivePolicies_cognitoIdentityPoolId = Lens.lens (\GetEffectivePolicies' {cognitoIdentityPoolId} -> cognitoIdentityPoolId) (\s@GetEffectivePolicies' {} a -> s {cognitoIdentityPoolId = a} :: GetEffectivePolicies)

instance Core.AWSRequest GetEffectivePolicies where
  type
    AWSResponse GetEffectivePolicies =
      GetEffectivePoliciesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEffectivePoliciesResponse'
            Prelude.<$> ( x Core..?> "effectivePolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEffectivePolicies where
  hashWithSalt _salt GetEffectivePolicies' {..} =
    _salt `Prelude.hashWithSalt` principal
      `Prelude.hashWithSalt` thingName
      `Prelude.hashWithSalt` cognitoIdentityPoolId

instance Prelude.NFData GetEffectivePolicies where
  rnf GetEffectivePolicies' {..} =
    Prelude.rnf principal
      `Prelude.seq` Prelude.rnf thingName
      `Prelude.seq` Prelude.rnf cognitoIdentityPoolId

instance Core.ToHeaders GetEffectivePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetEffectivePolicies where
  toJSON GetEffectivePolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("principal" Core..=) Prelude.<$> principal,
            ("cognitoIdentityPoolId" Core..=)
              Prelude.<$> cognitoIdentityPoolId
          ]
      )

instance Core.ToPath GetEffectivePolicies where
  toPath = Prelude.const "/effective-policies"

instance Core.ToQuery GetEffectivePolicies where
  toQuery GetEffectivePolicies' {..} =
    Prelude.mconcat ["thingName" Core.=: thingName]

-- | /See:/ 'newGetEffectivePoliciesResponse' smart constructor.
data GetEffectivePoliciesResponse = GetEffectivePoliciesResponse'
  { -- | The effective policies.
    effectivePolicies :: Prelude.Maybe [EffectivePolicy],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEffectivePoliciesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'effectivePolicies', 'getEffectivePoliciesResponse_effectivePolicies' - The effective policies.
--
-- 'httpStatus', 'getEffectivePoliciesResponse_httpStatus' - The response's http status code.
newGetEffectivePoliciesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetEffectivePoliciesResponse
newGetEffectivePoliciesResponse pHttpStatus_ =
  GetEffectivePoliciesResponse'
    { effectivePolicies =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The effective policies.
getEffectivePoliciesResponse_effectivePolicies :: Lens.Lens' GetEffectivePoliciesResponse (Prelude.Maybe [EffectivePolicy])
getEffectivePoliciesResponse_effectivePolicies = Lens.lens (\GetEffectivePoliciesResponse' {effectivePolicies} -> effectivePolicies) (\s@GetEffectivePoliciesResponse' {} a -> s {effectivePolicies = a} :: GetEffectivePoliciesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getEffectivePoliciesResponse_httpStatus :: Lens.Lens' GetEffectivePoliciesResponse Prelude.Int
getEffectivePoliciesResponse_httpStatus = Lens.lens (\GetEffectivePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetEffectivePoliciesResponse' {} a -> s {httpStatus = a} :: GetEffectivePoliciesResponse)

instance Prelude.NFData GetEffectivePoliciesResponse where
  rnf GetEffectivePoliciesResponse' {..} =
    Prelude.rnf effectivePolicies
      `Prelude.seq` Prelude.rnf httpStatus
