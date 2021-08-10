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
-- Module      : Network.AWS.IoT.GetEffectivePolicies
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the policies that have an effect on the authorization
-- behavior of the specified device when it connects to the AWS IoT device
-- gateway.
module Network.AWS.IoT.GetEffectivePolicies
  ( -- * Creating a Request
    GetEffectivePolicies (..),
    newGetEffectivePolicies,

    -- * Request Lenses
    getEffectivePolicies_thingName,
    getEffectivePolicies_cognitoIdentityPoolId,
    getEffectivePolicies_principal,

    -- * Destructuring the Response
    GetEffectivePoliciesResponse (..),
    newGetEffectivePoliciesResponse,

    -- * Response Lenses
    getEffectivePoliciesResponse_effectivePolicies,
    getEffectivePoliciesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetEffectivePolicies' smart constructor.
data GetEffectivePolicies = GetEffectivePolicies'
  { -- | The thing name.
    thingName :: Prelude.Maybe Prelude.Text,
    -- | The Cognito identity pool ID.
    cognitoIdentityPoolId :: Prelude.Maybe Prelude.Text,
    -- | The principal. Valid principals are CertificateArn
    -- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
    -- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
    -- (/region/:/id/).
    principal :: Prelude.Maybe Prelude.Text
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
-- 'thingName', 'getEffectivePolicies_thingName' - The thing name.
--
-- 'cognitoIdentityPoolId', 'getEffectivePolicies_cognitoIdentityPoolId' - The Cognito identity pool ID.
--
-- 'principal', 'getEffectivePolicies_principal' - The principal. Valid principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
newGetEffectivePolicies ::
  GetEffectivePolicies
newGetEffectivePolicies =
  GetEffectivePolicies'
    { thingName = Prelude.Nothing,
      cognitoIdentityPoolId = Prelude.Nothing,
      principal = Prelude.Nothing
    }

-- | The thing name.
getEffectivePolicies_thingName :: Lens.Lens' GetEffectivePolicies (Prelude.Maybe Prelude.Text)
getEffectivePolicies_thingName = Lens.lens (\GetEffectivePolicies' {thingName} -> thingName) (\s@GetEffectivePolicies' {} a -> s {thingName = a} :: GetEffectivePolicies)

-- | The Cognito identity pool ID.
getEffectivePolicies_cognitoIdentityPoolId :: Lens.Lens' GetEffectivePolicies (Prelude.Maybe Prelude.Text)
getEffectivePolicies_cognitoIdentityPoolId = Lens.lens (\GetEffectivePolicies' {cognitoIdentityPoolId} -> cognitoIdentityPoolId) (\s@GetEffectivePolicies' {} a -> s {cognitoIdentityPoolId = a} :: GetEffectivePolicies)

-- | The principal. Valid principals are CertificateArn
-- (arn:aws:iot:/region/:/accountId/:cert\//certificateId/), thingGroupArn
-- (arn:aws:iot:/region/:/accountId/:thinggroup\//groupName/) and CognitoId
-- (/region/:/id/).
getEffectivePolicies_principal :: Lens.Lens' GetEffectivePolicies (Prelude.Maybe Prelude.Text)
getEffectivePolicies_principal = Lens.lens (\GetEffectivePolicies' {principal} -> principal) (\s@GetEffectivePolicies' {} a -> s {principal = a} :: GetEffectivePolicies)

instance Core.AWSRequest GetEffectivePolicies where
  type
    AWSResponse GetEffectivePolicies =
      GetEffectivePoliciesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEffectivePoliciesResponse'
            Prelude.<$> ( x Core..?> "effectivePolicies"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetEffectivePolicies

instance Prelude.NFData GetEffectivePolicies

instance Core.ToHeaders GetEffectivePolicies where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON GetEffectivePolicies where
  toJSON GetEffectivePolicies' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("cognitoIdentityPoolId" Core..=)
              Prelude.<$> cognitoIdentityPoolId,
            ("principal" Core..=) Prelude.<$> principal
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
getEffectivePoliciesResponse_effectivePolicies = Lens.lens (\GetEffectivePoliciesResponse' {effectivePolicies} -> effectivePolicies) (\s@GetEffectivePoliciesResponse' {} a -> s {effectivePolicies = a} :: GetEffectivePoliciesResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getEffectivePoliciesResponse_httpStatus :: Lens.Lens' GetEffectivePoliciesResponse Prelude.Int
getEffectivePoliciesResponse_httpStatus = Lens.lens (\GetEffectivePoliciesResponse' {httpStatus} -> httpStatus) (\s@GetEffectivePoliciesResponse' {} a -> s {httpStatus = a} :: GetEffectivePoliciesResponse)

instance Prelude.NFData GetEffectivePoliciesResponse
