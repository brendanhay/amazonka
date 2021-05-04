{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Glue.GetResourcePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a specified resource policy.
module Network.AWS.Glue.GetResourcePolicy
  ( -- * Creating a Request
    GetResourcePolicy (..),
    newGetResourcePolicy,

    -- * Request Lenses
    getResourcePolicy_resourceArn,

    -- * Destructuring the Response
    GetResourcePolicyResponse (..),
    newGetResourcePolicyResponse,

    -- * Response Lenses
    getResourcePolicyResponse_policyInJson,
    getResourcePolicyResponse_updateTime,
    getResourcePolicyResponse_createTime,
    getResourcePolicyResponse_policyHash,
    getResourcePolicyResponse_httpStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetResourcePolicy' smart constructor.
data GetResourcePolicy = GetResourcePolicy'
  { -- | The ARN of the AWS Glue resource for the resource policy to be
    -- retrieved. For more information about AWS Glue resource ARNs, see the
    -- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
    resourceArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'getResourcePolicy_resourceArn' - The ARN of the AWS Glue resource for the resource policy to be
-- retrieved. For more information about AWS Glue resource ARNs, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
newGetResourcePolicy ::
  GetResourcePolicy
newGetResourcePolicy =
  GetResourcePolicy' {resourceArn = Prelude.Nothing}

-- | The ARN of the AWS Glue resource for the resource policy to be
-- retrieved. For more information about AWS Glue resource ARNs, see the
-- <https://docs.aws.amazon.com/glue/latest/dg/aws-glue-api-common.html#aws-glue-api-regex-aws-glue-arn-id AWS Glue ARN string pattern>
getResourcePolicy_resourceArn :: Lens.Lens' GetResourcePolicy (Prelude.Maybe Prelude.Text)
getResourcePolicy_resourceArn = Lens.lens (\GetResourcePolicy' {resourceArn} -> resourceArn) (\s@GetResourcePolicy' {} a -> s {resourceArn = a} :: GetResourcePolicy)

instance Prelude.AWSRequest GetResourcePolicy where
  type Rs GetResourcePolicy = GetResourcePolicyResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResourcePolicyResponse'
            Prelude.<$> (x Prelude..?> "PolicyInJson")
            Prelude.<*> (x Prelude..?> "UpdateTime")
            Prelude.<*> (x Prelude..?> "CreateTime")
            Prelude.<*> (x Prelude..?> "PolicyHash")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePolicy

instance Prelude.NFData GetResourcePolicy

instance Prelude.ToHeaders GetResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetResourcePolicy" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetResourcePolicy where
  toJSON GetResourcePolicy' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("ResourceArn" Prelude..=) Prelude.<$> resourceArn]
      )

instance Prelude.ToPath GetResourcePolicy where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResourcePolicyResponse' smart constructor.
data GetResourcePolicyResponse = GetResourcePolicyResponse'
  { -- | Contains the requested policy document, in JSON format.
    policyInJson :: Prelude.Maybe Prelude.Text,
    -- | The date and time at which the policy was last updated.
    updateTime :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time at which the policy was created.
    createTime :: Prelude.Maybe Prelude.POSIX,
    -- | Contains the hash value associated with this policy.
    policyHash :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyInJson', 'getResourcePolicyResponse_policyInJson' - Contains the requested policy document, in JSON format.
--
-- 'updateTime', 'getResourcePolicyResponse_updateTime' - The date and time at which the policy was last updated.
--
-- 'createTime', 'getResourcePolicyResponse_createTime' - The date and time at which the policy was created.
--
-- 'policyHash', 'getResourcePolicyResponse_policyHash' - Contains the hash value associated with this policy.
--
-- 'httpStatus', 'getResourcePolicyResponse_httpStatus' - The response's http status code.
newGetResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePolicyResponse
newGetResourcePolicyResponse pHttpStatus_ =
  GetResourcePolicyResponse'
    { policyInJson =
        Prelude.Nothing,
      updateTime = Prelude.Nothing,
      createTime = Prelude.Nothing,
      policyHash = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Contains the requested policy document, in JSON format.
getResourcePolicyResponse_policyInJson :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_policyInJson = Lens.lens (\GetResourcePolicyResponse' {policyInJson} -> policyInJson) (\s@GetResourcePolicyResponse' {} a -> s {policyInJson = a} :: GetResourcePolicyResponse)

-- | The date and time at which the policy was last updated.
getResourcePolicyResponse_updateTime :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.UTCTime)
getResourcePolicyResponse_updateTime = Lens.lens (\GetResourcePolicyResponse' {updateTime} -> updateTime) (\s@GetResourcePolicyResponse' {} a -> s {updateTime = a} :: GetResourcePolicyResponse) Prelude.. Lens.mapping Prelude._Time

-- | The date and time at which the policy was created.
getResourcePolicyResponse_createTime :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.UTCTime)
getResourcePolicyResponse_createTime = Lens.lens (\GetResourcePolicyResponse' {createTime} -> createTime) (\s@GetResourcePolicyResponse' {} a -> s {createTime = a} :: GetResourcePolicyResponse) Prelude.. Lens.mapping Prelude._Time

-- | Contains the hash value associated with this policy.
getResourcePolicyResponse_policyHash :: Lens.Lens' GetResourcePolicyResponse (Prelude.Maybe Prelude.Text)
getResourcePolicyResponse_policyHash = Lens.lens (\GetResourcePolicyResponse' {policyHash} -> policyHash) (\s@GetResourcePolicyResponse' {} a -> s {policyHash = a} :: GetResourcePolicyResponse)

-- | The response's http status code.
getResourcePolicyResponse_httpStatus :: Lens.Lens' GetResourcePolicyResponse Prelude.Int
getResourcePolicyResponse_httpStatus = Lens.lens (\GetResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@GetResourcePolicyResponse' {} a -> s {httpStatus = a} :: GetResourcePolicyResponse)

instance Prelude.NFData GetResourcePolicyResponse
