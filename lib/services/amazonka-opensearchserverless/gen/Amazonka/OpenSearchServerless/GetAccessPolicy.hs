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
-- Module      : Amazonka.OpenSearchServerless.GetAccessPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an OpenSearch Serverless access policy. For more information,
-- see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/serverless-data-access.html Data access control for Amazon OpenSearch Serverless>.
module Amazonka.OpenSearchServerless.GetAccessPolicy
  ( -- * Creating a Request
    GetAccessPolicy (..),
    newGetAccessPolicy,

    -- * Request Lenses
    getAccessPolicy_name,
    getAccessPolicy_type,

    -- * Destructuring the Response
    GetAccessPolicyResponse (..),
    newGetAccessPolicyResponse,

    -- * Response Lenses
    getAccessPolicyResponse_accessPolicyDetail,
    getAccessPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearchServerless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetAccessPolicy' smart constructor.
data GetAccessPolicy = GetAccessPolicy'
  { -- | The name of the access policy.
    name :: Prelude.Text,
    -- | Tye type of policy. Currently the only supported value is @data@.
    type' :: AccessPolicyType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getAccessPolicy_name' - The name of the access policy.
--
-- 'type'', 'getAccessPolicy_type' - Tye type of policy. Currently the only supported value is @data@.
newGetAccessPolicy ::
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  AccessPolicyType ->
  GetAccessPolicy
newGetAccessPolicy pName_ pType_ =
  GetAccessPolicy' {name = pName_, type' = pType_}

-- | The name of the access policy.
getAccessPolicy_name :: Lens.Lens' GetAccessPolicy Prelude.Text
getAccessPolicy_name = Lens.lens (\GetAccessPolicy' {name} -> name) (\s@GetAccessPolicy' {} a -> s {name = a} :: GetAccessPolicy)

-- | Tye type of policy. Currently the only supported value is @data@.
getAccessPolicy_type :: Lens.Lens' GetAccessPolicy AccessPolicyType
getAccessPolicy_type = Lens.lens (\GetAccessPolicy' {type'} -> type') (\s@GetAccessPolicy' {} a -> s {type' = a} :: GetAccessPolicy)

instance Core.AWSRequest GetAccessPolicy where
  type
    AWSResponse GetAccessPolicy =
      GetAccessPolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetAccessPolicyResponse'
            Prelude.<$> (x Data..?> "accessPolicyDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetAccessPolicy where
  hashWithSalt _salt GetAccessPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData GetAccessPolicy where
  rnf GetAccessPolicy' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders GetAccessPolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpenSearchServerless.GetAccessPolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetAccessPolicy where
  toJSON GetAccessPolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("type" Data..= type')
          ]
      )

instance Data.ToPath GetAccessPolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetAccessPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetAccessPolicyResponse' smart constructor.
data GetAccessPolicyResponse = GetAccessPolicyResponse'
  { -- | Details about the requested access policy.
    accessPolicyDetail :: Prelude.Maybe AccessPolicyDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetAccessPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessPolicyDetail', 'getAccessPolicyResponse_accessPolicyDetail' - Details about the requested access policy.
--
-- 'httpStatus', 'getAccessPolicyResponse_httpStatus' - The response's http status code.
newGetAccessPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetAccessPolicyResponse
newGetAccessPolicyResponse pHttpStatus_ =
  GetAccessPolicyResponse'
    { accessPolicyDetail =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about the requested access policy.
getAccessPolicyResponse_accessPolicyDetail :: Lens.Lens' GetAccessPolicyResponse (Prelude.Maybe AccessPolicyDetail)
getAccessPolicyResponse_accessPolicyDetail = Lens.lens (\GetAccessPolicyResponse' {accessPolicyDetail} -> accessPolicyDetail) (\s@GetAccessPolicyResponse' {} a -> s {accessPolicyDetail = a} :: GetAccessPolicyResponse)

-- | The response's http status code.
getAccessPolicyResponse_httpStatus :: Lens.Lens' GetAccessPolicyResponse Prelude.Int
getAccessPolicyResponse_httpStatus = Lens.lens (\GetAccessPolicyResponse' {httpStatus} -> httpStatus) (\s@GetAccessPolicyResponse' {} a -> s {httpStatus = a} :: GetAccessPolicyResponse)

instance Prelude.NFData GetAccessPolicyResponse where
  rnf GetAccessPolicyResponse' {..} =
    Prelude.rnf accessPolicyDetail
      `Prelude.seq` Prelude.rnf httpStatus
