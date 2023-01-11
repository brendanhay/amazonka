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
-- Module      : Amazonka.OpenSearchServerless.GetSecurityPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a configured OpenSearch Serverless security
-- policy. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-network.html Network access for Amazon OpenSearch Serverless>
-- and
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-encryption.html Encryption at rest for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.GetSecurityPolicy
  ( -- * Creating a Request
    GetSecurityPolicy (..),
    newGetSecurityPolicy,

    -- * Request Lenses
    getSecurityPolicy_name,
    getSecurityPolicy_type,

    -- * Destructuring the Response
    GetSecurityPolicyResponse (..),
    newGetSecurityPolicyResponse,

    -- * Response Lenses
    getSecurityPolicyResponse_securityPolicyDetail,
    getSecurityPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSecurityPolicy' smart constructor.
data GetSecurityPolicy = GetSecurityPolicy'
  { -- | The name of the security policy.
    name :: Prelude.Text,
    -- | The type of security policy.
    type' :: SecurityPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecurityPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getSecurityPolicy_name' - The name of the security policy.
--
-- 'type'', 'getSecurityPolicy_type' - The type of security policy.
newGetSecurityPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  SecurityPolicyType ->
  GetSecurityPolicy
newGetSecurityPolicy pName_ pType_ =
  GetSecurityPolicy' {name = pName_, type' = pType_}

-- | The name of the security policy.
getSecurityPolicy_name :: Lens.Lens' GetSecurityPolicy Prelude.Text
getSecurityPolicy_name = Lens.lens (\GetSecurityPolicy' {name} -> name) (\s@GetSecurityPolicy' {} a -> s {name = a} :: GetSecurityPolicy)

-- | The type of security policy.
getSecurityPolicy_type :: Lens.Lens' GetSecurityPolicy SecurityPolicyType
getSecurityPolicy_type = Lens.lens (\GetSecurityPolicy' {type'} -> type') (\s@GetSecurityPolicy' {} a -> s {type' = a} :: GetSecurityPolicy)

instance Core.AWSRequest GetSecurityPolicy where
  type
    AWSResponse GetSecurityPolicy =
      GetSecurityPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSecurityPolicyResponse'
            Prelude.<$> (x Data..?> "securityPolicyDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSecurityPolicy where
  hashWithSalt _salt GetSecurityPolicy' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData GetSecurityPolicy where
  rnf GetSecurityPolicy' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders GetSecurityPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.GetSecurityPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetSecurityPolicy where
  toJSON GetSecurityPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath GetSecurityPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetSecurityPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSecurityPolicyResponse' smart constructor.
data GetSecurityPolicyResponse = GetSecurityPolicyResponse'
  { -- | Details about the requested security policy.
    securityPolicyDetail :: Prelude.Maybe SecurityPolicyDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSecurityPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'securityPolicyDetail', 'getSecurityPolicyResponse_securityPolicyDetail' - Details about the requested security policy.
--
-- 'httpStatus', 'getSecurityPolicyResponse_httpStatus' - The response's http status code.
newGetSecurityPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSecurityPolicyResponse
newGetSecurityPolicyResponse pHttpStatus_ =
  GetSecurityPolicyResponse'
    { securityPolicyDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the requested security policy.
getSecurityPolicyResponse_securityPolicyDetail :: Lens.Lens' GetSecurityPolicyResponse (Prelude.Maybe SecurityPolicyDetail)
getSecurityPolicyResponse_securityPolicyDetail = Lens.lens (\GetSecurityPolicyResponse' {securityPolicyDetail} -> securityPolicyDetail) (\s@GetSecurityPolicyResponse' {} a -> s {securityPolicyDetail = a} :: GetSecurityPolicyResponse)

-- | The response's http status code.
getSecurityPolicyResponse_httpStatus :: Lens.Lens' GetSecurityPolicyResponse Prelude.Int
getSecurityPolicyResponse_httpStatus = Lens.lens (\GetSecurityPolicyResponse' {httpStatus} -> httpStatus) (\s@GetSecurityPolicyResponse' {} a -> s {httpStatus = a} :: GetSecurityPolicyResponse)

instance Prelude.NFData GetSecurityPolicyResponse where
  rnf GetSecurityPolicyResponse' {..} =
    Prelude.rnf securityPolicyDetail
      `Prelude.seq` Prelude.rnf httpStatus
