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
-- Module      : Amazonka.CloudTrail.PutResourcePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a resource-based permission policy to a CloudTrail channel that
-- is used for an integration with an event source outside of Amazon Web
-- Services. For more information about resource-based policies, see
-- <https://docs.aws.amazon.com/awscloudtrail/latest/userguide/security_iam_resource-based-policy-examples.html CloudTrail resource-based policy examples>
-- in the /CloudTrail User Guide/.
module Amazonka.CloudTrail.PutResourcePolicy
  ( -- * Creating a Request
    PutResourcePolicy (..),
    newPutResourcePolicy,

    -- * Request Lenses
    putResourcePolicy_resourceArn,
    putResourcePolicy_resourcePolicy,

    -- * Destructuring the Response
    PutResourcePolicyResponse (..),
    newPutResourcePolicyResponse,

    -- * Response Lenses
    putResourcePolicyResponse_resourceArn,
    putResourcePolicyResponse_resourcePolicy,
    putResourcePolicyResponse_httpStatus,
  )
where

import Amazonka.CloudTrail.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutResourcePolicy' smart constructor.
data PutResourcePolicy = PutResourcePolicy'
  { -- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
    -- resource-based policy. The following is the format of a resource ARN:
    -- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
    resourceArn :: Prelude.Text,
    -- | A JSON-formatted string for an Amazon Web Services resource-based
    -- policy.
    --
    -- The following are requirements for the resource policy:
    --
    -- -   Contains only one action: cloudtrail-data:PutAuditEvents
    --
    -- -   Contains at least one statement. The policy can have a maximum of 20
    --     statements.
    --
    -- -   Each statement contains at least one principal. A statement can have
    --     a maximum of 50 principals.
    resourcePolicy :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putResourcePolicy_resourceArn' - The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
-- resource-based policy. The following is the format of a resource ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
--
-- 'resourcePolicy', 'putResourcePolicy_resourcePolicy' - A JSON-formatted string for an Amazon Web Services resource-based
-- policy.
--
-- The following are requirements for the resource policy:
--
-- -   Contains only one action: cloudtrail-data:PutAuditEvents
--
-- -   Contains at least one statement. The policy can have a maximum of 20
--     statements.
--
-- -   Each statement contains at least one principal. A statement can have
--     a maximum of 50 principals.
newPutResourcePolicy ::
  -- | 'resourceArn'
  Prelude.Text ->
  -- | 'resourcePolicy'
  Prelude.Text ->
  PutResourcePolicy
newPutResourcePolicy pResourceArn_ pResourcePolicy_ =
  PutResourcePolicy'
    { resourceArn = pResourceArn_,
      resourcePolicy = pResourcePolicy_
    }

-- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
-- resource-based policy. The following is the format of a resource ARN:
-- @arn:aws:cloudtrail:us-east-2:123456789012:channel\/MyChannel@.
putResourcePolicy_resourceArn :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourceArn = Lens.lens (\PutResourcePolicy' {resourceArn} -> resourceArn) (\s@PutResourcePolicy' {} a -> s {resourceArn = a} :: PutResourcePolicy)

-- | A JSON-formatted string for an Amazon Web Services resource-based
-- policy.
--
-- The following are requirements for the resource policy:
--
-- -   Contains only one action: cloudtrail-data:PutAuditEvents
--
-- -   Contains at least one statement. The policy can have a maximum of 20
--     statements.
--
-- -   Each statement contains at least one principal. A statement can have
--     a maximum of 50 principals.
putResourcePolicy_resourcePolicy :: Lens.Lens' PutResourcePolicy Prelude.Text
putResourcePolicy_resourcePolicy = Lens.lens (\PutResourcePolicy' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicy' {} a -> s {resourcePolicy = a} :: PutResourcePolicy)

instance Core.AWSRequest PutResourcePolicy where
  type
    AWSResponse PutResourcePolicy =
      PutResourcePolicyResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutResourcePolicyResponse'
            Prelude.<$> (x Data..?> "ResourceArn")
            Prelude.<*> (x Data..?> "ResourcePolicy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutResourcePolicy where
  hashWithSalt _salt PutResourcePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` resourceArn
      `Prelude.hashWithSalt` resourcePolicy

instance Prelude.NFData PutResourcePolicy where
  rnf PutResourcePolicy' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourcePolicy

instance Data.ToHeaders PutResourcePolicy where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "com.amazonaws.cloudtrail.v20131101.CloudTrail_20131101.PutResourcePolicy" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutResourcePolicy where
  toJSON PutResourcePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ResourceArn" Data..= resourceArn),
            Prelude.Just
              ("ResourcePolicy" Data..= resourcePolicy)
          ]
      )

instance Data.ToPath PutResourcePolicy where
  toPath = Prelude.const "/"

instance Data.ToQuery PutResourcePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutResourcePolicyResponse' smart constructor.
data PutResourcePolicyResponse = PutResourcePolicyResponse'
  { -- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
    -- resource-based policy.
    resourceArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON-formatted string of the Amazon Web Services resource-based
    -- policy attached to the CloudTrail channel.
    resourcePolicy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutResourcePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceArn', 'putResourcePolicyResponse_resourceArn' - The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
-- resource-based policy.
--
-- 'resourcePolicy', 'putResourcePolicyResponse_resourcePolicy' - The JSON-formatted string of the Amazon Web Services resource-based
-- policy attached to the CloudTrail channel.
--
-- 'httpStatus', 'putResourcePolicyResponse_httpStatus' - The response's http status code.
newPutResourcePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutResourcePolicyResponse
newPutResourcePolicyResponse pHttpStatus_ =
  PutResourcePolicyResponse'
    { resourceArn =
        Prelude.Nothing,
      resourcePolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the CloudTrail channel attached to the
-- resource-based policy.
putResourcePolicyResponse_resourceArn :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_resourceArn = Lens.lens (\PutResourcePolicyResponse' {resourceArn} -> resourceArn) (\s@PutResourcePolicyResponse' {} a -> s {resourceArn = a} :: PutResourcePolicyResponse)

-- | The JSON-formatted string of the Amazon Web Services resource-based
-- policy attached to the CloudTrail channel.
putResourcePolicyResponse_resourcePolicy :: Lens.Lens' PutResourcePolicyResponse (Prelude.Maybe Prelude.Text)
putResourcePolicyResponse_resourcePolicy = Lens.lens (\PutResourcePolicyResponse' {resourcePolicy} -> resourcePolicy) (\s@PutResourcePolicyResponse' {} a -> s {resourcePolicy = a} :: PutResourcePolicyResponse)

-- | The response's http status code.
putResourcePolicyResponse_httpStatus :: Lens.Lens' PutResourcePolicyResponse Prelude.Int
putResourcePolicyResponse_httpStatus = Lens.lens (\PutResourcePolicyResponse' {httpStatus} -> httpStatus) (\s@PutResourcePolicyResponse' {} a -> s {httpStatus = a} :: PutResourcePolicyResponse)

instance Prelude.NFData PutResourcePolicyResponse where
  rnf PutResourcePolicyResponse' {..} =
    Prelude.rnf resourceArn
      `Prelude.seq` Prelude.rnf resourcePolicy
      `Prelude.seq` Prelude.rnf httpStatus
