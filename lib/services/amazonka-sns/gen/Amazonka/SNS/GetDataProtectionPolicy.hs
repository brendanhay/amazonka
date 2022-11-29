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
-- Module      : Amazonka.SNS.GetDataProtectionPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the specified inline @DataProtectionPolicy@ document that is
-- stored in the specified Amazon SNS topic.
module Amazonka.SNS.GetDataProtectionPolicy
  ( -- * Creating a Request
    GetDataProtectionPolicy (..),
    newGetDataProtectionPolicy,

    -- * Request Lenses
    getDataProtectionPolicy_resourceArn,

    -- * Destructuring the Response
    GetDataProtectionPolicyResponse (..),
    newGetDataProtectionPolicyResponse,

    -- * Response Lenses
    getDataProtectionPolicyResponse_dataProtectionPolicy,
    getDataProtectionPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SNS.Types

-- | /See:/ 'newGetDataProtectionPolicy' smart constructor.
data GetDataProtectionPolicy = GetDataProtectionPolicy'
  { -- | The ARN of the topic whose @DataProtectionPolicy@ you want to get.
    --
    -- For more information about ARNs, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
    -- in the Amazon Web Services General Reference.
    resourceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataProtectionPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getDataProtectionPolicy_resourceArn' - The ARN of the topic whose @DataProtectionPolicy@ you want to get.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the Amazon Web Services General Reference.
newGetDataProtectionPolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  GetDataProtectionPolicy
newGetDataProtectionPolicy pResourceArn_ =
  GetDataProtectionPolicy'
    { resourceArn =
        pResourceArn_
    }

-- | The ARN of the topic whose @DataProtectionPolicy@ you want to get.
--
-- For more information about ARNs, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs)>
-- in the Amazon Web Services General Reference.
getDataProtectionPolicy_resourceArn :: Lens.Lens' GetDataProtectionPolicy Prelude.Text
getDataProtectionPolicy_resourceArn = Lens.lens (\GetDataProtectionPolicy' {resourceArn} -> resourceArn) (\s@GetDataProtectionPolicy' {} a -> s {resourceArn = a} :: GetDataProtectionPolicy)

instance Core.AWSRequest GetDataProtectionPolicy where
  type
    AWSResponse GetDataProtectionPolicy =
      GetDataProtectionPolicyResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "GetDataProtectionPolicyResult"
      ( \s h x ->
          GetDataProtectionPolicyResponse'
            Prelude.<$> (x Core..@? "DataProtectionPolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataProtectionPolicy where
  hashWithSalt _salt GetDataProtectionPolicy' {..} =
    _salt `Prelude.hashWithSalt` resourceArn

instance Prelude.NFData GetDataProtectionPolicy where
  rnf GetDataProtectionPolicy' {..} =
    Prelude.rnf resourceArn

instance Core.ToHeaders GetDataProtectionPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetDataProtectionPolicy where
  toPath = Prelude.const "/"

instance Core.ToQuery GetDataProtectionPolicy where
  toQuery GetDataProtectionPolicy' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("GetDataProtectionPolicy" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-03-31" :: Prelude.ByteString),
        "ResourceArn" Core.=: resourceArn
      ]

-- | /See:/ 'newGetDataProtectionPolicyResponse' smart constructor.
data GetDataProtectionPolicyResponse = GetDataProtectionPolicyResponse'
  { -- | Retrieves the @DataProtectionPolicy@ in JSON string format.
    dataProtectionPolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataProtectionPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataProtectionPolicy', 'getDataProtectionPolicyResponse_dataProtectionPolicy' - Retrieves the @DataProtectionPolicy@ in JSON string format.
--
-- 'httpStatus', 'getDataProtectionPolicyResponse_httpStatus' - The response's http status code.
newGetDataProtectionPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataProtectionPolicyResponse
newGetDataProtectionPolicyResponse pHttpStatus_ =
  GetDataProtectionPolicyResponse'
    { dataProtectionPolicy =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Retrieves the @DataProtectionPolicy@ in JSON string format.
getDataProtectionPolicyResponse_dataProtectionPolicy :: Lens.Lens' GetDataProtectionPolicyResponse (Prelude.Maybe Prelude.Text)
getDataProtectionPolicyResponse_dataProtectionPolicy = Lens.lens (\GetDataProtectionPolicyResponse' {dataProtectionPolicy} -> dataProtectionPolicy) (\s@GetDataProtectionPolicyResponse' {} a -> s {dataProtectionPolicy = a} :: GetDataProtectionPolicyResponse)

-- | The response's http status code.
getDataProtectionPolicyResponse_httpStatus :: Lens.Lens' GetDataProtectionPolicyResponse Prelude.Int
getDataProtectionPolicyResponse_httpStatus = Lens.lens (\GetDataProtectionPolicyResponse' {httpStatus} -> httpStatus) (\s@GetDataProtectionPolicyResponse' {} a -> s {httpStatus = a} :: GetDataProtectionPolicyResponse)

instance
  Prelude.NFData
    GetDataProtectionPolicyResponse
  where
  rnf GetDataProtectionPolicyResponse' {..} =
    Prelude.rnf dataProtectionPolicy
      `Prelude.seq` Prelude.rnf httpStatus
