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
-- Module      : Amazonka.CloudTrail.GetResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the JSON text of the resource-based policy document attached
-- to the CloudTrail channel.
module Amazonka.CloudTrail.GetResourcePolicy
  ( -- * Creating a Request
    GetResourcePolicy (..),
    newGetResourcePolicy,

    -- * Request Lenses
    getResourcePolicy_resourceArn,

    -- * Destructuring the Response
    GetResourcePolicyResponse (..),
    newGetResourcePolicyResponse,

    -- * Response Lenses
    getResourcePolicyResponse_resourceArn,
    getResourcePolicyResponse_resourcePolicy,
    getResourcePolicyResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
    -- resource-based policy. The following is the format of a resource ARN:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
-- resource-based policy. The following is the format of a resource ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
newGetResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetResourcePolicy
newGetResourcePolicy pResourceArn_ =
  GetResourcePolicy' {resourceArn = pResourceArn_}

-- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
-- resource-based policy. The following is the format of a resource ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
getResourcePolicy_resourceArn :: Lens.Lens' GetResourcePolicy Prelude.Text
getResourcePolicy_resourceArn = Lens.lens (\GetResourcePolicy' {resourceArn} -> resourceArn) (\s@GetResourcePolicy' {} a -> s {resourceArn = a} :: GetResourcePolicy)

instance Core.AWSRequest GetResourcePolicy where
  type
    AWSResponse GetResourcePolicy =
      GetResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Prelude.<$> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "ResourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicy where
  hashWithSalt _salt GetResourcePolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetResourcePolicy where
  rnf GetResourcePolicy' {..} = Prelude.rnf resourceArn

instance Data.ToHeaders GetResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.GetResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceArn" Data..= resourceArn)]
      )

instance Data.ToPath GetResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to
    -- resource-based policy.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | A JSON-formatted string that contains the resource-based policy attached
    -- to the CloudTrail channel.
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getResourcePolicyResponse_resourceArn' - The Amazon Resource Name (ARN) of the CloudTrail channel attached to
-- resource-based policy.
--
-- 'resourcePolicy', 'getResourcePolicyResponse_resourcePolicy' - A JSON-formatted string that contains the resource-based policy attached
-- to the CloudTrail channel.
--
-- 'httpStatus', 'getResourcePolicyResponse_httpStatus' - The response's http status code.
newGetResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { resourceArn =
        Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to
-- resource-based policy.
getResourcePolicyResponse_resourceArn :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_resourceArn = Lens.lens (\GetResourcePolicyResponse' {resourceArn} -> resourceArn) (\s@GetResourcePolicyResponse' {} a -> s {resourceArn = a} :: GetResourcePolicyResponse)

-- | A JSON-formatted string that contains the resource-based policy attached
-- to the CloudTrail channel.
getResourcePolicyResponse_resourcePolicy :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_resourcePolicy = Lens.lens (\GetResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@GetResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Prelude.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Prelude.NFData GetResourcePolicyResponse where
  rnf GetResourcePolicyResponse' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus
