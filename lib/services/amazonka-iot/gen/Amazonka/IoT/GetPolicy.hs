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
-- Module      : Amazonka.IoT.GetPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about the specified policy with the policy document of
-- the default version.
--
-- Requires permission to access the
-- <https://docs.aws.amazon.com/service-authorization/latest/reference/list_awsiot.html#awsiot-actions-as-permissions GetPolicy>
-- action.
module Amazonka.IoT.GetPolicy
  ( -- * Creating a Request
    GetPolicy (..),
    newGetPolicy,

    -- * Request Lenses
    getPolicy_policyName,

    -- * Destructuring the Response
    GetPolicyResponse (..),
    newGetPolicyResponse,

    -- * Response Lenses
    getPolicyResponse_creationDate,
    getPolicyResponse_defaultVersionId,
    getPolicyResponse_generationId,
    getPolicyResponse_lastModifiedDate,
    getPolicyResponse_policyArn,
    getPolicyResponse_policyDocument,
    getPolicyResponse_policyName,
    getPolicyResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the GetPolicy operation.
--
-- /See:/ 'newGetPolicy' smart constructor.
data GetPolicy = GetPolicy'
  { -- | The name of the policy.
    policyName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyName', 'getPolicy_policyName' - The name of the policy.
newGetPolicy ::
  -- | 'policyName'
  Prelude.Text ->
  GetPolicy
newGetPolicy pPolicyName_ =
  GetPolicy' {policyName = pPolicyName_}

-- | The name of the policy.
getPolicy_policyName :: Lens.Lens' GetPolicy Prelude.Text
getPolicy_policyName = Lens.lens (\GetPolicy' {policyName} -> policyName) (\s@GetPolicy' {} a -> s {policyName = a} :: GetPolicy)

instance Core.AWSRequest GetPolicy where
  type AWSResponse GetPolicy = GetPolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPolicyResponse'
            Prelude.<$> (x Data..?> "creationDate")
            Prelude.<*> (x Data..?> "defaultVersionId")
            Prelude.<*> (x Data..?> "generationId")
            Prelude.<*> (x Data..?> "lastModifiedDate")
            Prelude.<*> (x Data..?> "policyArn")
            Prelude.<*> (x Data..?> "policyDocument")
            Prelude.<*> (x Data..?> "policyName")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPolicy where
  hashWithSalt _salt GetPolicy' {..} =
    _salt `Prelude.hashWithSalt` policyName

instance Prelude.NFData GetPolicy where
  rnf GetPolicy' {..} = Prelude.rnf policyName

instance Data.ToHeaders GetPolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetPolicy where
  toPath GetPolicy' {..} =
    Prelude.mconcat
      ["/policies/", Data.toBS policyName]

instance Data.ToQuery GetPolicy where
  toQuery = Prelude.const Prelude.mempty

-- | The output from the GetPolicy operation.
--
-- /See:/ 'newGetPolicyResponse' smart constructor.
data GetPolicyResponse = GetPolicyResponse'
  { -- | The date the policy was created.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The default policy version ID.
    defaultVersionId :: Prelude.Maybe Prelude.Text,
    -- | The generation ID of the policy.
    generationId :: Prelude.Maybe Prelude.Text,
    -- | The date the policy was last modified.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The policy ARN.
    policyArn :: Prelude.Maybe Prelude.Text,
    -- | The JSON document that describes the policy.
    policyDocument :: Prelude.Maybe Prelude.Text,
    -- | The policy name.
    policyName :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'getPolicyResponse_creationDate' - The date the policy was created.
--
-- 'defaultVersionId', 'getPolicyResponse_defaultVersionId' - The default policy version ID.
--
-- 'generationId', 'getPolicyResponse_generationId' - The generation ID of the policy.
--
-- 'lastModifiedDate', 'getPolicyResponse_lastModifiedDate' - The date the policy was last modified.
--
-- 'policyArn', 'getPolicyResponse_policyArn' - The policy ARN.
--
-- 'policyDocument', 'getPolicyResponse_policyDocument' - The JSON document that describes the policy.
--
-- 'policyName', 'getPolicyResponse_policyName' - The policy name.
--
-- 'httpStatus', 'getPolicyResponse_httpStatus' - The response's http status code.
newGetPolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPolicyResponse
newGetPolicyResponse pHttpStatus_ =
  GetPolicyResponse'
    { creationDate = Prelude.Nothing,
      defaultVersionId = Prelude.Nothing,
      generationId = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      policyArn = Prelude.Nothing,
      policyDocument = Prelude.Nothing,
      policyName = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date the policy was created.
getPolicyResponse_creationDate :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.UTCTime)
getPolicyResponse_creationDate = Lens.lens (\GetPolicyResponse' {creationDate} -> creationDate) (\s@GetPolicyResponse' {} a -> s {creationDate = a} :: GetPolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The default policy version ID.
getPolicyResponse_defaultVersionId :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_defaultVersionId = Lens.lens (\GetPolicyResponse' {defaultVersionId} -> defaultVersionId) (\s@GetPolicyResponse' {} a -> s {defaultVersionId = a} :: GetPolicyResponse)

-- | The generation ID of the policy.
getPolicyResponse_generationId :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_generationId = Lens.lens (\GetPolicyResponse' {generationId} -> generationId) (\s@GetPolicyResponse' {} a -> s {generationId = a} :: GetPolicyResponse)

-- | The date the policy was last modified.
getPolicyResponse_lastModifiedDate :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.UTCTime)
getPolicyResponse_lastModifiedDate = Lens.lens (\GetPolicyResponse' {lastModifiedDate} -> lastModifiedDate) (\s@GetPolicyResponse' {} a -> s {lastModifiedDate = a} :: GetPolicyResponse) Prelude.. Lens.mapping Data._Time

-- | The policy ARN.
getPolicyResponse_policyArn :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_policyArn = Lens.lens (\GetPolicyResponse' {policyArn} -> policyArn) (\s@GetPolicyResponse' {} a -> s {policyArn = a} :: GetPolicyResponse)

-- | The JSON document that describes the policy.
getPolicyResponse_policyDocument :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_policyDocument = Lens.lens (\GetPolicyResponse' {policyDocument} -> policyDocument) (\s@GetPolicyResponse' {} a -> s {policyDocument = a} :: GetPolicyResponse)

-- | The policy name.
getPolicyResponse_policyName :: Lens.Lens' GetPolicyResponse (Prelude.Maybe Prelude.Text)
getPolicyResponse_policyName = Lens.lens (\GetPolicyResponse' {policyName} -> policyName) (\s@GetPolicyResponse' {} a -> s {policyName = a} :: GetPolicyResponse)

-- | The response's http status code.
getPolicyResponse_httpStatus :: Lens.Lens' GetPolicyResponse Prelude.Int
getPolicyResponse_httpStatus = Lens.lens (\GetPolicyResponse' {httpStatus} -> httpStatus) (\s@GetPolicyResponse' {} a -> s {httpStatus = a} :: GetPolicyResponse)

instance Prelude.NFData GetPolicyResponse where
  rnf GetPolicyResponse' {..} =
    Prelude.rnf creationDate `Prelude.seq`
      Prelude.rnf defaultVersionId `Prelude.seq`
        Prelude.rnf generationId `Prelude.seq`
          Prelude.rnf lastModifiedDate `Prelude.seq`
            Prelude.rnf policyArn `Prelude.seq`
              Prelude.rnf policyDocument `Prelude.seq`
                Prelude.rnf policyName `Prelude.seq`
                  Prelude.rnf httpStatus
